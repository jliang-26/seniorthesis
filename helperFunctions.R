set.seed(67)
library(MCMCpack)
library(combinat)
source("constants.R")
source("helperFunctions.R")

# Function 1: draw sample_size rankings from samplePopulation
poll_rankings <- function(populationSize, sample_size, samplePopulation) {
  colIndices <- sample(populationSize, size = sample_size, replace = FALSE)
  sample_rankings <- samplePopulation[,colIndices]
}

# Function 2: Compute parameters of the Dirichlet Posterior using sample_rankings
dirichlet_posterior = function(sample_rankings, candidates) {
  n = factorial(length(candidates))
  
  #Generate all possible rankings. Account for the possibility that a ranking isn't present in sample_rankings.
  all_rankings = sapply(permn(candidates), paste, collapse = "-")
  
  # Count occurrences of each ranking as a table of unique rankings.
  df = as.data.frame(sample_rankings)
  strings = apply(df, 1, paste, collapse = "-")
  counts = table(factor(strings, levels = all_rankings))
  
  # Compute posterior
  alpha_prior = rep(1, n)
  alpha_post = alpha_prior + as.numeric(counts)
  
  names(alpha_post) = all_rankings
  
  alpha_post
}

# Function 3: Simulates RCV. Function takes set of unique rankings and probabilities from 
# sample_probs and number of candidates from candidates
run_rcv = function(sample_probs, candidates) {
  c = length(candidates)
  p = ncol(sample_probs)
  n = nrow(sample_probs)
  winner = rep(0, n)
  
  #Create the RCV matrix. Row 1 is sampled probabilities, row 2 is a
  # tool to eliminate candidates, and subsequent rows are the rankings.
  RCV = matrix(0, nrow = c + 2, ncol = factorial(c))
  orderings = colnames(sample_probs)
  for (i in 1:p) {
    num_order = as.numeric(strsplit(orderings[i],"-")[[1]])
    RCV[3:(c+2), i] = num_order
  }
  
  #Begin rounds
  for (r in 1:n) {
    tally = rep(0, c)
    add = rep(0, p)
    
    #Bring probabilities into RCV calculator
    RCV[1, ] = sample_probs[r, ]
    
    #Tally round 1
    for (i in 1:p) {
      j = RCV[3,i]
      tally[j] = tally[j] + RCV[1,i]
    }
    
    #While loop to do RCV candidate elimination.
    while (all(tally < 0.5)) {
      m = which.min(tally)
      for (i in 1:p) {
        if (RCV[3, i] == m) {
          add[i] = add[i] + 1
        }
      }
      for (i in 1:p) {
        j = RCV[3 + add[i],i]
        tally[j] = tally[j] + RCV[1,i]
      }
    }
    winner[r] = which.max(tally)
  }
  #Returns the sample_size # winners.
  winner
}