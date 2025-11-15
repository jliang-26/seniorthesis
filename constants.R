#The size of the population we are polling from
populationSize = 100000

#The sample size of each poll
sample_size = 500

#The number of polls
pollCount = 500

#The number of samplings from the posterior
probs_size = 500

#The number of candidates and a list of candidates
candidatesNum = 4
candidates <- 1:candidatesNum

#The simple support for each candidate. Length must match candidatesNum.
support = c(.31, .28, .25, .16)

