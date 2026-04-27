#The size of the population we are polling from
populationSize = 100000

#The sample size of each poll
sample_size = 500

#The number of polls
pollCount = 500

#The number of samplings from the posterior
probs_size = 500

#The number of candidates and a list of candidates
candidatesNum = 14
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
  "Shaun Donovan" = 8,
  "A different candidate" = 9
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
  "Isaac Wright Jr." = 13,
  "Write-in" = 14
)

# For manhattan bp:
mbp_candidates <- c(
"Mark Levine" = 1, 
"Brad Hoylman" = 2, 
"Elizabeth Caputo" = 3,
"Benjamin Kallos" = 4,
"Lindsey Boylan" = 5,
"Kimberly Watkins" = 6 
)

# For comptroller
comptroller_names <- c("Brian Benjamin" = 6, 
                       "Brad Lander" = 1, 
                       "Kevin Parker" = 5, 
                       "Reshma Patel" = 8, 
                       "Terri Liftin" = 9, 
                       "Zach Iscol" = 7, 
                       "Corey Johnson" = 2, 
                       "Michelle Caruso-Cabrera" = 3, 
                       "David Weprin" = 4) 
sorted_comp <- names(sort(comptroller_names))
