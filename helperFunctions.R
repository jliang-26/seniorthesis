set.seed(67)
library(MCMCpack)
library(combinat)
library(dplyr)
source("constants.R")

# Function 1: draw sample_size rankings from samplePopulation
poll_rankings <- function(populationSize, sample_size, samplePopulation) {
  colIndices <- sample(populationSize, size = sample_size, replace = FALSE)
  sample_rankings <- samplePopulation[,colIndices]
}

# Function 2: Compute parameters of the Dirichlet Posterior using sample_rankings
dirichlet_posterior = function(sample_rankings, candidates, prior = 1) {
  n = factorial(length(candidates))
  
  #Generate all possible rankings. Account for the possibility that a ranking isn't present in sample_rankings.
  all_rankings = sapply(permn(candidates), paste, collapse = "-")
  
  # Count occurrences of each ranking.
  rankings = apply(sample_rankings, 2, paste, collapse = "-")
  counts = table(factor(rankings, levels = all_rankings))
  
  # Compute posterior
  alpha_prior = rep(prior, n)
  alpha_post = alpha_prior + as.numeric(counts)
  names(alpha_post) = all_rankings
  
  alpha_post
}

# Function 2b: Compute Dirichlet posterior for a given poll sample.
# Need to change this if the prior isn't 1.
poll_dirichlet_posterior <- function(df) {
  
  # Compute posterior
  alpha_post = df$total + 1
  names(alpha_post) = df$combined_rankings
  
  alpha_post
}

# Function 3: Sample 500 probability distributions using posterior and preserve names.
sample_probs <- function(probs_size, posterior) {
  sample_probs = rdirichlet(probs_size, posterior)
  colnames(sample_probs) = names(posterior)
  
  sample_probs
}

# Function 4: Simulates RCV.

# The assumption now is that we only have full rankings. Must account for
# partial rankings too. 
run_rcv = function(sample_probs, candidates) {
  c = length(candidates)
  p = ncol(sample_probs)
  n = nrow(sample_probs)
  winner = rep(0, n)
  
  # Create the Rankings Matrix. Each column is one unique ballot ranking from the
  # sample_probs, including all unique rankings from the poll. Empty spots are 0s.
  orderings = colnames(sample_probs)
  
  split_rankings <- strsplit(orderings, "-")
  
  RCV <- sapply(split_rankings, function(x) {
    v <- as.numeric(x)
    length(v) <- c
    v[is.na(v)] <- 0
    v
  })
  
  #Create the RCV data matrix. This stores information about candidate elims, 
  #votes at elim, and winner's votes
  rcvData <- array(0, dim = c(c-1,c,n))
  dimnames(rcvData) <- list(
    paste0("Round ", 1:(c-1)), 
    paste0("Candidate ", 1:c),
    paste0("Poll ", 1:n)
  )
  
  #Begin rounds
  for (r in 1:n) {
    tally = rep(0, c)
    #Create the elimination matrix.
    elimTracker <- matrix(0, nrow = c , ncol = p)
    # Put .1 in elimTracker for all the places in RCV == 0 to skip them.
    elimTracker[RCV == 0] <- 0.1
    #Track all losers.
    loserTracker <- rep(0, c)
    
    #Bring probabilities into RCV calculator
    probs = sample_probs[r, ]
    
    #Tally round 1
    roundNum <- 1
    for (i in 1:p) {
      j = RCV[1,i]
      #Since we have entries in RCV == 0, ensure those don't mess with tally.
      if (j > 0) {
        tally[j] = tally[j] + probs[i]
      }
    }
    
    # While loop to do RCV candidate elimination.
    while (all(tally < 0.5)) {
      #Find the candidate with least votes for elimination
      # This uses an absolute value because we set previously eliminated
      # candidates to "-2" temporarily to prevent counting them.
      # Note m is an integer 1:c
      m = which.min(abs(tally))
      
      #Store the round results in rcvData
      rcvData[roundNum, ,r] <- tally
      rcvData[roundNum, ,r][rcvData[roundNum, ,r] == -2] <- 0
      
      #Track the round number for rcvData
      roundNum <- roundNum + 1
      
      # Update indexer to skip the eliminated candidate when retallying
      for (i in 1:p) {
        index <- which(RCV[, i] == m)
        elimTracker[index, i] <- .1
      }
      
      # Reset tally. Only nonzero values in loserTracker are for indexes of 
      # eliminated candidates == -2. We update loserTracker with the newly 
      # eliminated candidate and then reset tally to -2 for elim. candidates.
      loserTracker[m] <- -2
      tally <- loserTracker
      
      # Retally votes while skipping all eliminated candidates by finding
      # the first entry in each column of elimtracker which isn't 0.1.
      for (i in 1:p) {
        index <- match(0, elimTracker[, i])
        # Add the if statement in case all candidates on a ballot are elim'd. 
        if (!is.na(index)) {
          j <- RCV[index, i]
          tally[j] <- tally[j] + probs[i]
        }
      }
      
      # Rescale each candidate's total probability of support to account for 
      # decrease in total probability.
      rescale_factor <- sum(tally[tally>0])
      tally[tally>0] <- tally[tally>0]/rescale_factor
    }
    winner[r] = which.max(tally)
    rcvData[roundNum, ,r] <- tally
    rcvData[roundNum, ,r][rcvData[roundNum, ,r] == -2] <- 0
  }
  return(list(winner, rcvData))
}

#Function 5: extract rcv data.
collectWinners <- function(rcvOutputs) {
  winnersRecord <- matrix(0, nrow = pollCount, ncol = sample_size, 
                          dimnames = list(paste0("Poll ", 1:pollCount), 
                                          paste0("Sample ", 1:sample_size)))
  for (i in 1:pollCount) {
    winnersRecord[i,] <- rcvOutputs[[i]][[1]]
  }
  winnersRecord
}

#Function 6: extract rcv win percent data.
winPercents <- function(rcvOutputs) {
  allWinners <- collectWinners(rcvOutputs)
  #Win percentage for each poll
  pollWins <- matrix(0, nrow = candidatesNum, ncol = pollCount, 
                     dimnames = list(paste0("Candidate ", 1:candidatesNum),
                                     paste0("Poll ", 1:pollCount)))
  for (i in 1:pollCount) {
    holder <- table(allWinners[i,])
    for (j in 1:length(holder)) {
      pollWins[j,i] <- holder[j]/sum(table(allWinners[i,]))
    }
  }
  #Average over all polls to find win percentage overall.
  allWins <- rowMeans(sampleWins)
  
  return(list(pollWins, allWins))
}

#Function 7: compute real population winner
realWinner <- function(samplePopulation, candidates) {
  n = factorial(length(candidates))
  
  #Generate all possible rankings. Account for the possibility that a ranking isn't present in sample_rankings.
  all_rankings = sapply(permn(candidates), paste, collapse = "-")
  
  # Count occurrences of each ranking.
  rankings = apply(samplePopulation, 2, paste, collapse = "-")
  counts = t(as.matrix(table(factor(rankings, levels = all_rankings))))
  
  poprankTally <- tallyRanking(samplePopulation, candidates)/populationSize
  populationWinner <- run_rcv(poprankTally, candidates)
}

# Function 8: preprocess dfp poll
preprocess_dfp <- function(raw_data) {
  #Weight and rankings per poll.
  ranking_cols <- c("weight",
                    "rank_preference_sure_1",
                    "rank_preference_sure_2",
                    "rank_preference_sure_3",
                    "rank_preference_sure_4",
                    "rank_preference_sure_5")
  
  dfp_rankings <- raw_data[, ranking_cols]
  
  # convert names to numeric following candidate_names mapping
  dfp_rankings_numeric <- dfp_rankings %>%
    mutate(across(where(is.character), ~ unname(candidate_names[.x])))
  
  # Combine the numeric rankings and make blank rankings NA
  dfp_rankings_numeric$combined_rankings <- apply(dfp_rankings_numeric[, 2:6], 1, function(row)
    paste(row[!is.na(row)], collapse = "-")
  )
  dfp_rankings_numeric$combined_rankings[dfp_rankings_numeric$combined_rankings == ""] <- NA
  
  # check how many unique rankings there are
  n_distinct(dfp_rankings_numeric$combined_rankings, na.rm = TRUE)
  
  # Sum the weights corresponding to unique rankings.
  weighted_sum_rankings <- dfp_rankings_numeric %>%
    filter(!is.na(combined_rankings)) %>%
    group_by(combined_rankings) %>%
    summarise(total = sum(weight, na.rm = TRUE))
  
  # Remove weighted sums equal to zero.
  culled_weighted_sum_rankings <- weighted_sum_rankings %>% 
    filter(total != 0)
  
  # Compute the Dirichlet posterior
  posterior <- culled_weighted_sum_rankings$total + 1
  names(posterior) <- culled_weighted_sum_rankings$combined_rankings
  return(posterior)
}
