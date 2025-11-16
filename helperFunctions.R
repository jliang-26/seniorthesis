set.seed(67)
library(MCMCpack)
library(combinat)
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

# Function 3: Sample 500 probability distributions using posterior and preserve names.
sample_probs <- function(probs_size, posterior) {
  sample_probs = rdirichlet(probs_size, posterior)
  colnames(sample_probs) = names(posterior)
  
  sample_probs
}

# Function 4: Simulates RCV.
run_rcv = function(sample_probs, candidates) {
  c = length(candidates)
  p = ncol(sample_probs)
  n = nrow(sample_probs)
  winner = rep(0, n)
  
  #Create the Rankings Matrix.
  RCV = matrix(0, nrow = c , ncol = factorial(c))
  orderings = colnames(sample_probs)
  for (i in 1:p) {
    num_order = as.numeric(strsplit(orderings[i],"-")[[1]])
    RCV[, i] = num_order
  }
  
  #Create the RCV data matrix. This stores information about candidate elims, 
  #votes at elim, and winner's votes
  rcvData <- array(0, dim = c(candidatesNum-1,candidatesNum,n))
  dimnames(rcvData) <- list(
    paste0("Round ", 1:(candidatesNum-1)), 
    paste0("Candidate ", 1:candidatesNum),
    paste0("Poll ", 1:n)
  )
  
  #Begin rounds
  for (r in 1:n) {
    tally = rep(0, c)
    #Create the elimination matrix.
    elimTracker <- matrix(0, nrow = c , ncol = factorial(c))
    #Track all losers.
    loserTracker <- rep(0, c)
    
    #Bring probabilities into RCV calculator
    probs = sample_probs[r, ]
    
    #Tally round 1
    roundNum <- 1
    for (i in 1:p) {
      j = RCV[3,i]
      tally[j] = tally[j] + probs[i]
    }
    
    #While loop to do RCV candidate elimination.
    while (all(tally < 0.5)) {
      #Find the candidate with least votes for elimination
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
      
      # Reset tally
      loserTracker[m] <- -2
      tally <- loserTracker
      
      # Retally votes while skipping all eliminated candidates
      for (i in 1:p) {
        index <- which(elimTracker[,i] == 0)[1]
        j = RCV[index,i]
        tally[j] = tally[j] + probs[i]
      }
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
