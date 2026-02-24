library(combinat)
library(MCMCpack)

ex_RCV <- rcvData[1:7,,1]

sample_probs <- probs
candidates <- 1:8

# Function 4: Simulates RCV.
run_rcv = function(sample_probs, candidates) {
  # c is the number of candidates (8)
  c = length(candidates)
  # p is the number of unique rankings (807)
  p = ncol(sample_probs)
  # n is the number of samplings (500)
  n = nrow(sample_probs)
  # this stores the RCV winner for each sampling
  winner = rep(NA, n)
  
  # Create the c x p Rankings Matrix which vectorizes each unique ranking
  # from sample_probsl. Empty spots on a ranking are stored as zeroes.
  orderings <- colnames(sample_probs)
  
  split_rankings <- strsplit(orderings, "-")
  
  RCV <- sapply(split_rankings, function(x) {
    v <- as.numeric(x)
    length(v) <- c
    v[is.na(v)] <- 0
    v
  })
  
  #Create the RCV data matrix. This stores information about candidate elims, 
  #votes at elim, and winner's votes
  rcvData <- array(0, dim = c(c-1,c,n))
  dimnames(rcvData) <- list(
    paste0("Round ", 1:(c-1)), 
    paste0("Candidate ", 1:c),
    paste0("Poll ", 1:n)
  )
  
  #Begin rounds
  for (r in 1:n) {
    # tally gives the total support for each candidate.
    tally = rep(0, c)
    # Create an indicator matrix which tracks eliminations corresponding in
    # size to the RCV matrix. Entries associated with eliminated candidates
    # will be replaced with 1s.
    elimTracker <- matrix(0, nrow = c , ncol = p)
    # Put 1 in elimTracker for all the places in RCV == 0 to skip them.
    # RCV == 0 for incomplete rankings Ex: if only ranking one candidate,
    # the next 7 spots will be empty meaning RCV == 0.
    elimTracker[RCV == 0] <- 1
    
    # Track all losers.
    loserTracker <- rep(0, c)
    
    # Bring probabilities into RCV calculator
    probs <- sample_probs[r, ]
    
    #Tally round 1 support totals
    roundNum <- 1
    # For each unique ranking, add the support for the top-ranked candidate
    # to the tally.
    for (i in 1:p) {
      j <- RCV[1,i]
      tally[j] <- tally[j] + probs[i]
    }


    
    # While loop to do RCV candidate elimination. The while condition ensures
    # that no candidate has a majority support.
    while (all(tally < 0.5)) {
      # Eliminate the candidate with least support
      # This uses an absolute value because we set previously eliminated
      # candidates to "-100" temporarily to prevent counting them.
      # Note m is an integer 1:c, the candidate being eliminated
      m = which.min(abs(tally))
      
      # Store the round results in rcvData. Convert back any "-100" in rcvData
      # to "0" again, meaning candidates are eliminated.
      rcvData[roundNum, ,r] <- tally
      rcvData[roundNum, ,r][rcvData[roundNum, ,r] == -100] <- 0
      
      # Update the round number for rcvData
      roundNum <- roundNum + 1
      
      # Update elim matrix to skip the eliminated candidate when retallying
      for (i in 1:p) {
        index <- which(RCV[, i] == m)
        elimTracker[index, i] <- 1
      }
      
      # Reset entries of tally to equal zero except for those corresponidng to
      # an eliminated candidate, which we set to -100. 
      loserTracker[m] <- -100
      tally <- loserTracker
      
      # Retally votes while skipping all eliminated candidates. 
      for (i in 1:p) {
        # Find the index of the first zero entry in each column of elimtracker
        index <- match(0, elimTracker[, i])
        # If this index exists, find the corresponding prob. in RCV and sum.
        if (!is.na(index)) {
          j <- RCV[index, i]
          tally[j] <- tally[j] + probs[i]
        }
      }
      
      # Rescale each candidate's total probability of support to account for 
      # decrease in total probability that occurs as all candidates on 
      # incomplete rankings are eliminated. 
      rescale_factor <- sum(tally[tally>0])
      tally[tally>0] <- tally[tally>0]/rescale_factor
    }
    winner[r] = which.max(tally)
    rcvData[roundNum, ,r] <- tally
    rcvData[roundNum, ,r][rcvData[roundNum, ,r] == -100] <- 0
  }
  return(list(winner, rcvData))
}

#############################################################################

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
