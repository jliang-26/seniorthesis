#The size of the population we are polling from
populationSize = 100000

#The sample size of each poll
sample_size = 500

#The number of polls
pollCount = 500

#The number of samplings from the posterior
probs_size = 500

#The number of candidates and a list of candidates
candidatesNum = 8
candidates <- 1:candidatesNum

#The simple support for each candidate. Length must match candidatesNum.
support = c(.31, .28, .25, .16)

# For dfp poll
candidate_names <- c(
  "Eric Adams" = 1,
  "Kathryn Garcia" = 2,
  "Maya Wiley" = 3,
  "Andrew Yang" = 4,
  "Scott Stringer" = 5,
  "Dianne Morales" = 6,
  "Ray McGuire" = 7,
  "Shaun Donovan" = 8
)

# For real primary results
primary_candidates <- c(
  "Eric L. Adams" = 1,
  "Kathryn A. Garcia" = 2,
  "Maya D. Wiley" = 3,
  "Andrew Yang" = 4,
  "Scott M. Stringer" = 5,
  "Dianne Morales" = 6,
  "Raymond J. McGuire" = 7,
  "Shaun Donovan" = 8,
  "Art Chang" = 9,
  "Paperboy Love Prince" = 10,
  "Joycelyn Taylor" = 11,
  "Aaron S. Foldenauer" = 12,
  "Isaac Wright Jr." = 13
)

