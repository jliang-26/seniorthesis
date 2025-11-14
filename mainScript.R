set.seed(67)
library(MCMCpack)
library(combinat)
source("constants.R")
source("helperFunctions.R")


#Generate a simple population of size 100k based on support
samplePopulation <- sapply(1:populationSize, function(i) {
  sample(candidates, size = length(candidates), replace = FALSE, prob = support)
})

#Poll sample_size rankings from samplePopulation, repeat pollCount number of times
polls <- replicate(pollCount, poll_rankings(populationSize, sample_size, samplePopulation))


#Overall function based on candidates, support, and sample size. 
bayes_rcv_sim = function(candidates, support, sample_size) {
  
  # Simulate rankings and compute a Dirichlet Posterior using functions 1 & 2.
  sample_rankings = poll_rankings(candidates, support, sample_size)
  
  posterior = dirichlet_posterior(sample_rankings, candidates)
  
  # Sample 500 probability distributions using posterior
  sample_probs = rdirichlet(sample_size, posterior)
  colnames(sample_probs) = names(posterior)
  
  # Run RCV on probability distributions using function 3.
  winners = run_rcv(sample_probs, candidates)
  
  #Report winners by percent and count. Green out before running loop in line 120. 
  counts = table(winners)
  for (i in names(counts)) {
    cat("Candidate", i, "wins", counts[i], "times", "(", round(counts[i]/sample_size*100), "%)\n")
  }
  winners
}

#Run this to get results.
winners = bayes_rcv_sim(candidates, support, sample_size)


#Run winners function 500 times:
results = matrix(0, nrow = 500, ncol = 4)
colnames(results) = paste0("P", 1:4)
for (i in 1:500) {
  winners = bayes_rcv_sim(candidates, support, sample_size)
  counts = table(factor(winners, levels = 1:4))
  results[i, ] = (counts/length(winners))  
}

head(results)
colMeans(results)


