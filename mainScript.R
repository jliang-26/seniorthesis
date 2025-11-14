library(MCMCpack)
library(combinat)
set.seed(67)


#Overall function based on candidates, support, and sample size. 
bayes_rcv_sim = function(candidates, support, sample_size) {
  
  # Simulate rankings and compute a Dirichlet Posterior using functions 1 & 2.
  sample_rankings = simulated_data(candidates, support, sample_size)
  
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

# Set candidates, their support, and the poll sample size. 
candidates = c(1, 2, 3, 4)
support = c(.31, .28, .25, .16)
#support = c(.49, .2, .2, .11)
sample_size = 500

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


