library(combinat)
library(MCMCpack)

support = c(.31, .28, .25, .16)
candidatesNum = 4
candidates <- 1:candidatesNum
sample_rankings <- sapply(1:500, function(i) {
  sample(candidates, size = length(candidates), replace = FALSE, prob = support)
})

poll_rankings <- function(populationSize, sample_size, samplePopulation) {
  colIndices <- sample(populationSize, size = sample_size, replace = FALSE)
  rankings <- samplePopulation[,colIndices]
}

polls <- replicate(20, poll_rankings(500, 20, sample_rankings))

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

posteriorTest1 <- dirichlet_posterior(polls[,,1], candidates)
posteriorTest2 <- dirichlet_posterior(polls[,,2], candidates)

posteriors <- t(lapply(1:dim(polls)[3], function(i) {
  dirichlet_posterior(polls[,,i], candidates)
}))

probs_size = 20
# Sample 500 probability distributions using posterior and preserve names.
sample_probs <- function(probs_size, posterior) {
  sample_probs = rdirichlet(probs_size, posterior)
  colnames(sample_probs) = names(posterior)
  
  sample_probs
}

posteriorProbsTest <- sample_probs(probs_size, posteriors[[1]])

posteriorProbs[[1]] <- sample_probs(probs_size, posteriors[[1]])

# All remaining code
#Overall function based on candidates, support, and sample size. 
bayes_rcv_sim = function(candidates, support, sample_size) {
  
  # Simulate rankings and compute a Dirichlet Posterior using functions 1 & 2.
  sample_rankings = poll_rankings(candidates, support, sample_size)
  
  posterior = dirichlet_posterior(sample_rankings, candidates)
  
  # Sample 500 probability distributions using posterior and preserve names.
  sample_probs <- function(probs_size, posterior) {
    sample_probs = rdirichlet(probs_size, posterior)
    colnames(sample_probs) = names(posterior)  
  }
  
  
  # Run RCV on probability distributions using function 3.
  winners = run_rcv(sample_probs, candidates)
  
  #Report winners by percent and count. Green out before running loop in line 120. 
  counts = table(winners)
  for (i in names(counts)) {
    cat("Candidate", i, "wins", counts[i], "times", "(", round(counts[i]/probs_size*100), "%)\n")
  }
  winners
}

#Run this to get results.
winners = bayes_rcv_sim(candidates, support, probs_size)


#Run winners function 500 times:
results = matrix(0, nrow = 500, ncol = 4)
colnames(results) = paste0("P", 1:4)
for (i in 1:500) {
  winners = bayes_rcv_sim(candidates, support, probs_size)
  counts = table(factor(winners, levels = 1:4))
  results[i, ] = (counts/length(winners))  
}

head(results)
colMeans(results)
