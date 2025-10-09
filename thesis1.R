# Simulate 10,000 rankings for n candidates by assigning a probability to each candidate.  
simulated_data = function(candidates, support){
  n_sim = 10000
  results = matrix(NA, nrow = n_sim, ncol = length(candidates))
  set.seed(67)
  for (i in 1:n_sim){
    results[i, ] = sample(candidates, size = length(candidates), replace = FALSE, prob = support)
  }
  
  results
}

# Compute paramters of the Dirichlet Posterior
dirichlet_posterior = function(results, candidates) {
  n_sequences = factorial(length(candidates))
  # Count occurrences of each ranking. 
  df = as.data.frame(results)
  strings = apply(df, 1, paste, collapse = "-")
  sequences = unique(strings)
  counts = table(factor(strings, levels = sequences))

  # Compute posterior
  alpha_prior = rep(1, n_sequences)
  alpha_post = alpha_prior + as.numeric(counts)
  
  names(alpha_post) = sequences
  
  alpha_post
}

# Set candidates and percentage of support. 
candidates = c("A", "B", "C", "D")

support = c(.4, .3, .2, .1)

# Simulate rankings and compute a Dirichlet Posterior.
sample_rankings = simulated_data(candidates, support)

posterior = dirichlet_posterior(sample_rankings, candidates)

library(MCMCpack)
# Sample 10,000 probability distributions using posterior
sample_polls = rdirichlet(10000, posterior)
colnames(sample_polls) = names(posterior)

# Sample 10,000 rankings using each probability distribution
# and count occurrences of each ranking for each sampling.
counts = t(apply(sample_polls, 1, function(p) {
  as.vector(rmultinom(1, size = 10000, prob = p))
}))
colnames(counts) = names(posterior)
head(counts)

# Simulate first round of rcv
first_rank = substr(colnames(counts), 1, 1)
counts_grouped <- t(apply(counts, 1, function(row) {
  tapply(row, first_rank, sum)
}))
counts_grouped <- counts_grouped[, c("A", "B", "C", "D")]

head(counts_grouped)

