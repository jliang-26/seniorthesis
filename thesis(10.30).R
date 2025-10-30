library(MCMCpack)
set.seed(67)

# Function 1: Simulate a poll of "sample_size" # of rankings for "candidates" by assigning a 
# probability of "support" to each candidate.  
simulated_data = function(candidates, support, sample_size){
  results = matrix(NA, nrow = (sample_size), ncol = length(candidates))
  for (i in 1:sample_size){
    results[i, ] = sample(candidates, size = length(candidates), replace = FALSE, prob = support)
  }
  
  results
}

# Function 2: Compute parameters of the Dirichlet Posterior using "results" from function 1 poll
dirichlet_posterior = function(results, candidates) {
  n = factorial(length(candidates))
  # Count occurrences of each ranking as a table of unique rankings.
  df = as.data.frame(results)
  strings = apply(df, 1, paste, collapse = "-")
  sequences = unique(strings)
  counts = table(factor(strings, levels = sequences))
  
  # Compute posterior
  alpha_prior = rep(1, n)
  alpha_post = alpha_prior + as.numeric(counts)
  
  names(alpha_post) = sequences
  
  alpha_post
}

# Function 3: Simulates RCV. Function takes set of unique rankings and probabilities from 
# "sample_polls" and number of candidates from "candidates"
run_rcv = function(sample_polls, candidates) {
  c = length(candidates)
  p = ncol(sample_polls)
  n = nrow(sample_polls)
  winner = rep(0, n)
  
  #Create the RCV matrix. Row 1 is sampled probabilities, row 2 is a
  # tool to eliminate candidates, and subsequent rows are the rankings.
  RCV = matrix(0, nrow = c + 2, ncol = factorial(c))
  orderings = colnames(sample_polls)
  for (i in 1:p) {
    num_order = as.numeric(strsplit(orderings[i],"-")[[1]])
    RCV[3:(c+2), i] = num_order
  }
  
  #Begin rounds
  for (r in 1:n) {
    tally = rep(0, c)
    add = rep(0, p)
    
    #Bring probabilities into RCV calculator
    RCV[1, ] = sample_polls[r, ]
    
    #Tally round 1
    for (i in 1:p) {
      j = RCV[3,i]
      tally[j] = tally[j] + RCV[1,i]
    }
    
    #While loop to do RCV canddiate elimination.
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
  #Returns the "sample_size" # winners.
  winner
}

#Overall function based on candidates, support, and sample size. 
bayes_rcv_sim = function(candidates, support, sample_size) {
  
  # Simulate rankings and compute a Dirichlet Posterior using functions 1 & 2.
  sample_rankings = simulated_data(candidates, support, sample_size)
  
  posterior = dirichlet_posterior(sample_rankings, candidates)
  
  # Sample 500 probability distributions using posterior
  sample_polls = rdirichlet(sample_size, posterior)
  colnames(sample_polls) = names(posterior)
  
  # Run RCV on probability distributions using function 3.
  winners = run_rcv(sample_polls, candidates)
  
  #Report winners by percent and count.
  counts <- table(winners)
  for (i in names(counts)) {
    cat("Candidate", i, "wins", counts[i], "times", "(", round(counts[i]/sample_size*100), "%)\n")
  }
  winners
}

# Set candidates, their support, and the poll sample size. 
candidates = c(1, 2, 3, 4)
support = c(.31, .28, .25, .16)
sample_size = 500

#Run this to get results.
winners = bayes_rcv_sim(candidates, support, sample_size)




