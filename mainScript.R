source("constants.R")
source("helperFunctions.R")


#Step one: Generate a simple population of size 100k based on support
samplePopulation <- sapply(1:populationSize, function(i) {
  sample(candidates, size = length(candidates), replace = FALSE, prob = support)
})

#Step two: Poll sample_size rankings from samplePopulation, repeat pollCount number of times
polls <- replicate(pollCount, poll_rankings(populationSize, sample_size, samplePopulation))

#Step three: Compute a Dirichlet posterior for each poll. Optionally specify prior.
posteriors <- t(lapply(1:pollCount, function(i) {
  dirichlet_posterior(polls[,,i], candidates)
}))

#Step four: sample from Dirichlet posteriors and compute RCV winners.
posteriorProbs <- vector("list", pollCount)
rcvOutputs <- vector("list", pollCount)
for (i in 1:pollCount) {
  posteriorProbs[[i]] <- sample_probs(probs_size, posteriors[[i]])
  rcvOutputs[[i]] <- run_rcv(posteriorProbs[[i]], candidates)
}

#Step five: interpret RCV results.
allWinners <- collectWinners(rcvOutputs)
roundInfo <- lapply(rcvOutputs, function(x) x[[2]])

