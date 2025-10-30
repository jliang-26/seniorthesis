# Simulate 500 rankings for n candidates by assigning a probability to each candidate.  
simulated_data = function(candidates, support){
  n_sim = 500
  results = matrix(NA, nrow = n_sim, ncol = length(candidates))
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

# Set candidates and percentage of support. 
candidates = c("A", "B", "C", "D")

support = c(.4, .3, .2, .1)

# Simulate rankings and compute a Dirichlet Posterior.
sample_rankings = simulated_data(candidates, support)

posterior = dirichlet_posterior(sample_rankings, candidates)

# Sample 500 probability distributions using posterior
library(MCMCpack)
n_samples = 500
sample_polls = rdirichlet(n_samples, posterior)
colnames(sample_polls) = names(posterior)

# Simulate RCV. Note I pulled this directly from ChatGPT.

# Fast RCV function (kept outside loop)
run_rcv_fast <- function(ballots) {
  candidates <- unique(as.vector(ballots))
  eliminated <- character(0)
  
  repeat {
    remaining <- setdiff(candidates, eliminated)
    first_choices <- apply(ballots, 1, function(x) x[which(!x %in% eliminated)[1]])
    tally <- table(factor(first_choices, levels = remaining))
    tally_num <- as.numeric(tally)
    names(tally_num) <- remaining
    
    total_votes <- sum(tally_num)
    if (any(tally_num > total_votes / 2)) {
      winner <- names(which.max(tally_num))
      return(list(
        winner = winner,
        votes = tally_num[which.max(tally_num)],
        method = "majority"
      ))
    }
    
    lowest <- names(which.min(tally_num))
    eliminated <- c(eliminated, lowest)
    remaining_after <- setdiff(candidates, eliminated)
    if (length(remaining_after) == 1) {
      winner <- remaining_after
      final_votes <- tally_num[remaining_after]
      if (is.na(final_votes)) final_votes <- 0
      return(list(
        winner = winner,
        votes = final_votes,
        method = "elimination"
      ))
    }
  }
}

# Main sampling + RCV simulation
sample_rcv <- function(sample_polls, n_ballots = 1000) {
  n_samples <- nrow(sample_polls)
  
  results <- data.frame(
    winner = character(n_samples),
    votes = numeric(n_samples),
    method = character(n_samples),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:n_samples) {
    ballots_strings <- sample(
      names(sample_polls[i, ]),
      size = n_ballots,
      replace = TRUE,
      prob = sample_polls[i, ]
    )
    
    ballots <- do.call(rbind, strsplit(ballots_strings, "-"))
    colnames(ballots) <- paste0("Rank_", 1:ncol(ballots))
    
    out <- run_rcv_fast(ballots)
    results[i, ] <- c(out$winner, out$votes, out$method)
  }
  
  return(results)
}

# Run it
output <- sample_rcv(sample_polls)

