#We night need to set seed again. IDK how it works.

# Simulate 500 rankings for n candidates by assigning a probability to each candidate.  
simulated_data = function(candidates, support){
  n_sim = 500
  results = matrix(NA, nrow = (n_sim), ncol = length(candidates))
  set.seed(67)
  for (i in 1:n_sim){
    results[i, ] = sample(candidates, size = length(candidates), replace = FALSE, prob = support)
  }
  
  results
}

# Compute parameters of the Dirichlet Posterior
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

# Set candidates and percentage of support. There are 4 candidates, named 1 through 4.
candidates = c(1, 2, 3, 4)

support = c(.4, .3, .2, .1)

# Simulate rankings and compute a Dirichlet Posterior.
sample_rankings = simulated_data(candidates, support)

posterior = dirichlet_posterior(sample_rankings, candidates)

# Sample 500 probability distributions using posterior
library(MCMCpack)
n_samples = 500
sample_polls = rdirichlet(n_samples, posterior)
colnames(sample_polls) = names(posterior)

# Simulate RCV.

#Create Variables For Total Probability for Each Candidate.
tally = c(A = 0, B = 0, C= 0, D= 0)
add = 0

# Create the RCV matrix and fill with candidate orderings
RCV = matrix(0, nrow = length(candidates) + 2, ncol = factorial(length(candidates)))
orderings = names(posterior)

# Fill matrix with each unique ranking.
for (j in 1:length(posterior)) {
  num_order = as.numeric(strsplit(orderings[j],"-")[[1]])
  RCV[2:(length(candidates)+1), j] = num_order
}
