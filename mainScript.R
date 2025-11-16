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

#Step five: gather RCV results.

#Each column of pollWins gives the candidate win percentages for one poll. 
#allWins gives the win percentages over the entire simulation (all polls).
pollWins <- winPercents(rcvOutputs)[[1]]
allWins <- winPercents(rcvOutputs)[[2]]

#roundInfo has the candidate vote tallies for each round of each sample of each poll
#for the entire simulation. roundInfo[[k][i,,j] gives the candidate votes for 
#the ith round of the jth sample of the kth poll.
roundInfo <- lapply(rcvOutputs, function(x) x[[2]])