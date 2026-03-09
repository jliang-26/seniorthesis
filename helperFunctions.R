set.seed(67)
library(gtools)
library(MCMCpack)
library(combinat)
library(dplyr)
library(tidyverse)
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
run_rcv = function(sample_probs, candidates) {
  # c is the number of candidates (8)
  c = length(candidates)
  # p is the number of unique rankings (807)
  p = ncol(sample_probs)
  # n is the number of samplings (500)
  n = nrow(sample_probs)
  # this stores the RCV winner for each sampling
  winner = rep(NA, n)
  
  # Create the c x p Rankings Matrix which vectorizes each unique ranking
  # from sample_probs. Empty spots on a ranking are stored as zeroes.
  orderings <- colnames(sample_probs)
  
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
    # tally gives the total support for each candidate.
    tally = rep(0, c)
    # Create an indicator matrix which tracks eliminations corresponding in
    # size to the RCV matrix. Entries associated with eliminated candidates
    # will be replaced with 1s.
    elimTracker <- matrix(0, nrow = c , ncol = p)
    # Put 1 in elimTracker for all the places in RCV == 0 to skip them.
    # RCV == 0 for incomplete rankings Ex: if only ranking one candidate,
    # the next 7 spots will be empty meaning RCV == 0.
    elimTracker[RCV == 0] <- 1
    
    # Track all losers.
    loserTracker <- rep(0, c)
    
    # Bring probabilities into RCV calculator
    probs <- sample_probs[r, ]
    
    #Tally round 1 support totals
    roundNum <- 1
    # For each unique ranking, add the support for the top-ranked candidate
    # to the tally.
    for (i in 1:p) {
      j <- RCV[1,i]
      tally[j] <- tally[j] + probs[i]
    }
    
    # While loop to do RCV candidate elimination. The while condition ensures
    # that no candidate has a majority support.
    while (all(tally < 0.5)) {
      # Eliminate the candidate with least support
      # This uses an absolute value because we set previously eliminated
      # candidates to "-100" temporarily to prevent counting them.
      # Note m is an integer 1:c, the candidate being eliminated
      m = which.min(abs(tally))
      
      # Store the round results in rcvData. Convert back any "-100" in rcvData
      # to "0" again, meaning candidates are eliminated.
      
      rcvData[roundNum, ,r] <- tally
      rcvData[roundNum, ,r][rcvData[roundNum, ,r] == -100] <- 0
      
      # Update the round number for rcvData
      roundNum <- roundNum + 1
      
      # Update elim matrix to skip the eliminated candidate when retallying
      for (i in 1:p) {
        index <- which(RCV[, i] == m)
        elimTracker[index, i] <- 1
      }
      
      # Reset entries of tally to equal zero except for those corresponidng to
      # an eliminated candidate, which we set to -100. 
      loserTracker[m] <- -100
      tally <- loserTracker
      
      # Retally votes while skipping all eliminated candidates. 
      for (i in 1:p) {
        # Find the index of the first zero entry in each column of elimtracker
        index <- match(0, elimTracker[, i])
        # If this index exists, find the corresponding prob. in RCV and sum.
        if (!is.na(index)) {
          j <- RCV[index, i]
          tally[j] <- tally[j] + probs[i]
        }
      }
      
      # Rescale each candidate's total probability of support to account for 
      # decrease in total probability that occurs as all candidates on 
      # incomplete rankings are eliminated. 
      rescale_factor <- sum(tally[tally>0])
      tally[tally>0] <- tally[tally>0]/rescale_factor
    }
    winner[r] = which.max(tally)
    rcvData[roundNum, ,r] <- tally
    rcvData[roundNum, ,r][rcvData[roundNum, ,r] == -100] <- 0
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
preprocess_dfp <- function(raw_data, weighted = 1, prior = 0.0001) {
  #Weight and rankings per poll.
  ranking_cols <- c("weight",
                    "rank_preference_sure_1",
                    "rank_preference_sure_2",
                    "rank_preference_sure_3",
                    "rank_preference_sure_4",
                    "rank_preference_sure_5")
  
  if (weighted == 1) {
    dfp_rankings <- raw_data[, ranking_cols]
  } else {
    dfp_rankings <- raw_data[, ranking_cols]
    dfp_rankings[, "weight"] <- 1
  }
  
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
    # Ignore blank rankings (shows up as NA on combined)
    filter(!is.na(combined_rankings)) %>%
    # 
    group_by(combined_rankings) %>%
    summarise(total = sum(weight, na.rm = TRUE))
  
  # Remove weighted sums equal to zero.
  culled_weighted_sum_rankings <- weighted_sum_rankings %>% 
    filter(total != 0)
  
  # Compute the Dirichlet posterior
  posterior <- culled_weighted_sum_rankings$total + prior
  names(posterior) <- culled_weighted_sum_rankings$combined_rankings
  return(posterior)
}

# Function 9 DFP Manhattan Borough President Poll
preprocess_mbp_dfp <- function(raw_data, weighted = 1) {

  # Try dfp_weight_nyc_manhattan_da_2021_revised_standard_sms_only_v3
  # or dfp_weight_nyc_manhattan_da_2021_standard_sms_only
  relCols <- c("dfp_weight_nyc_manhattan_da_2021_standard_sms_only", 
               grep("manhattan_bp", names(raw_data), value = TRUE))
  
  if (weighted == 1) {
    dfp_mbp_rankings <- raw_data[, relCols]
    dfp_mbp_rankings <- dfp_mbp_rankings %>%
      rename(weight = dfp_weight_nyc_manhattan_da_2021_standard_sms_only)
  } else {
    dfp_mbp_rankings <- raw_data[, relCols]
    dfp_mbp_rankings <- dfp_mbp_rankings %>%
      rename(weight = dfp_weight_nyc_manhattan_da_2021_standard_sms_only)
    dfp_mbp_rankings[, "weight"] <- 1
  }
  
  # Drop empty/irrelevant ballots if they didn't rank any manhattan BP Qs.
  dfp_mbp_rankings <- dfp_mbp_rankings[rowSums(dfp_mbp_rankings[, -1] != "") > 0, ] 
  
  # Drop the ballots with zero weight
  dfp_mbp_rankings <- dfp_mbp_rankings[dfp_mbp_rankings$weight > 0,]
  
  # Combine voted and poll for second_choice and third_choice
  dfp_mbp_rankings$combined_second_choice <- ifelse(!is.na(dfp_mbp_rankings$dfp_nyc_manhattan_bp_ballot_second_choice_voted), 
                                                    dfp_mbp_rankings$dfp_nyc_manhattan_bp_ballot_second_choice_voted, 
                                                    dfp_mbp_rankings$dfp_nyc_manhattan_bp_ballot_second_choice)
  dfp_mbp_rankings$combined_third_choice <- ifelse(!is.na(dfp_mbp_rankings$dfp_nyc_manhattan_bp_ballot_third_choice_voted), 
                                                   dfp_mbp_rankings$dfp_nyc_manhattan_bp_ballot_third_choice_voted, 
                                                   dfp_mbp_rankings$dfp_nyc_manhattan_bp_ballot_third_choice)
  # Remove "No one" from second and third choices.
  dfp_mbp_rankings <- dfp_mbp_rankings %>%
    mutate(across(c(combined_second_choice, combined_third_choice),
                  ~ ifelse(. == "No one", "", .)))
  
  # treat manhattan_bp_sure, combined_second_choice, and combined_third_choice 
  # as the first, second, and third-choice rankings. 
  dfp_mbp_rankings <- dfp_mbp_rankings[, c("weight",
                                           "manhattan_bp_sure", 
                                           "combined_second_choice",
                                           "combined_third_choice")]
  
  # Get a mapping of integers to unique candidate names.
  names_list <- unique(unlist(dfp_mbp_rankings[, c("manhattan_bp_sure", 
                                                   "combined_second_choice",
                                                   "combined_third_choice")]))
  names_list <- names_list[names_list != ""]
  mbp_candidate_names <- setNames(seq_along(names_list), names_list)
  
  # convert names to numeric following candidate_names mapping
  dfp_mbp_rankings_numeric <- dfp_mbp_rankings %>%
    mutate(across(where(is.character), ~ unname(mbp_candidate_names[.x])))
  
  # Combine the numeric rankings and make blank rankings NA
  dfp_mbp_rankings_numeric$combined_rankings <- apply(dfp_mbp_rankings_numeric[, 2:4], 1, function(row)
    paste(row[!is.na(row)], collapse = "-")
  )
  dfp_mbp_rankings_numeric$combined_rankings[dfp_mbp_rankings_numeric$combined_rankings == ""] <- NA
  output <- list(dfp_mbp_rankings_numeric, mbp_candidate_names)
  return(output)
}

aggregate_mbp_rankings <- function(dfp_mbp_rankings_numeric) {
  # Sum the weights corresponding to unique rankings.
  weighted_sum_rankings <- dfp_mbp_rankings_numeric %>%
    # Ignore blank rankings (shows up as NA on combined)
    filter(!is.na(combined_rankings)) %>%
    # 
    group_by(combined_rankings) %>%
    summarise(total = sum(weight, na.rm = TRUE))
  
  # Remove weighted sums equal to zero.
  culled_weighted_sum_rankings <- weighted_sum_rankings %>% 
    filter(total != 0)
  # Sort by largest to smallest weight
  sorted_weighted_rankings <- culled_weighted_sum_rankings %>%
    arrange(desc(total))
  sorted_weighted_rankings
}
