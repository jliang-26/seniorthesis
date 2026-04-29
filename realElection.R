source("constants.R")
source("helperFunctions.R")

primary_ballots <- read.csv("Datasets/nyc_mayor_dem_primary_ballots_ranked.csv")

### I NEED TO REDO THIS TO ACCOUNT FOR INCORRECTLY RANKED BALLOTS LATER ###
### Blank ballots and ballots with repeated candidates.

# Later to exactly recreate RCV following the ballots. Lowkey a waste of time now
# We can assume that gaps, or undercounts, are just skipped. 

cands <- c("Eric L. Adams",
           "Maya D. Wiley",
           "Kathryn A. Garcia")

# Compare real RCV results, pruning and not pruning.
data_numeric <- preprocess_primary(raw_data = primary_ballots,
                                   prune = TRUE,
                                   candidates = cands)
posterior <- realPosterior(data_numeric)
probs <- sample_probs(probs_size = 500, posterior = posterior)
results <- run_rcv(probs, 1:3)

post_probs <- t(as.matrix(posterior/sum(posterior)  ))                                     

real_results <- run_rcv(post_probs, 1:14)
real_rounds <- as.data.frame(real_results[[2]])
names(real_rounds) <- names(primary_candidates)

# Pruned results
data_numeric_pruned <- preprocess_primary(raw_data = primary_ballots, 
                                   prune = TRUE,
                                   candidates = cands)
posterior_pruned <- realPosterior(data_numeric_pruned)
post_probs_pruned <- t(as.matrix(posterior_pruned/sum(posterior_pruned))) 
real_results_pruned <- run_rcv(post_probs_pruned, 1:3)
real_rounds_pruned <- as.data.frame(real_results_pruned[[2]])
names(real_rounds_pruned) <- c("Eric L. Adams", NA,
                               "Maya D. Wiley")

# 75,153 Unique Ballots Were Cast
unique_rankings_count <- sum(!duplicated(primary_ballots[,2:6]))

total_possible_rankings <- sapply(1:5, FUN = function(x) {
  choose(13, x)*factorial(x)
})

# There are 173,485 possible ballot rankings given 13 candidates and rank up to 5.
possible_rankings_count <- sum(total_possible_rankings)
# Below outputs 0.433196. This means 43.3% of possible rankings were seen.
unique_rankings_count/possible_rankings_count

# 16787 ballots repeat candidates.
repeated_candidates <- apply(primary_ballots, 1, function(x) {
  x <- x[x != ""]
  any(duplicated(x))
})

repeat_candidate_ballots <- primary_ballots[repeated_candidates == TRUE,]

# 10062 ballots duplicate candidates but rank more than one candidate.
ballot_errors <- apply(primary_ballots, 1, function(x) {
  x <- x[x != ""]
  any(duplicated(x)) & length(unique(x)) > 2
})

error_ballots <- primary_ballots[ballot_errors == TRUE,]

empty_ballots <- apply(primary_ballots, 1, function(x) {
  x <- x[2:6]
  all(x == "")
})

# The top 10 most frequent ballots are below as "top_ten".
# "most_common" gives complete frequency table.
tab <- as.data.frame(table(data.frame(primary_ballots[,2:6])))
most_common <- tab[order(-tab$Freq), ]
top_ten <- head(most_common, n = 10)

# There are 16629 ballots with blank gaps. For example primary_ballots[574,] 
# which has empty rank_1 but ranked candidates 2-4. 
list_of_gap_ballots <- sapply(1:4, FUN = function(x) {
  results <- apply(primary_ballots, 1, FUN = function(y) {
    y[x + 1] == "" & y[x + 2] != ""
  })
  #sum(results == TRUE)
  which(results == TRUE)
})
sum(sapply(1:4, FUN = function(x) {
  length(list_of_gap_ballots[[x]])
}))

# Gives a list of the number of ballots that ranked 1:5 candidates, not including blanks.
#                   1        2         3         4         5 
# This equals: 127610   111666    154863    116104    434784
#   By %      (13.50)  (11.82)   (16.39)   (12.29)   (46.00)

ballots_by_num_of_ranked_cands <- sapply(1:5, FUN = function(x) {
  results <- apply(primary_ballots, 1, FUN = function(y) {
    y <- y[2:6]
    y <- y[y != ""]
    length(unique(y)) == x
  })
  sum(results == TRUE)
})

100*ballots_by_num_of_ranked_cands/sum(ballots_by_num_of_ranked_cands)


# 917941 ballots ranked at least one of the top 4 candidates.
# There were 997871 total ballots cast, and 52844 blank ballots.
# So only non-blank 27086 ballots did not include one of the top 4 candidates.
top_4 <- c("Eric L. Adams",
           "Kathryn A. Garcia",
           "Maya D. Wiley",
           "Andrew Yang")
top_4_indicator <- apply(primary_ballots, 1, FUN = function(x) {
  any(top_4 %in% x)
})
sum(top_4_indicator == TRUE)
###############################################################################
# Simulating polls from the real electorate
# Compare changing sample size and number of polls.
# Need to adjust priors based on upr
# Compare # of polls: 100 and 500.
# Compare sample size n = 2000 and n = 200
cands <- c("Eric L. Adams",
           "Maya D. Wiley",
           "Kathryn A. Garcia",
           "Andrew Yang",
           "Scott M. Stringer")
samplePolls <- function(numPolls, pollSize, cands, primary_ballots) {

  sample_polls <- sapply(1:numPolls, FUN = function(x) {
    primary_ballots[sample(nrow(primary_ballots), size = pollSize),]
  }, simplify = FALSE)
  
  sample_results <- sapply(1:numPolls, FUN = function(i) {
    data_numeric <- preprocess_primary(raw_data = sample_polls[[i]],
                                       prune = FALSE, 
                                       candidates = cands)
    # Ensure the prior size is 15% of the poll size.
    n <- n_distinct(data_numeric$combined_rankings, na.rm = TRUE)
    posterior <- realPosterior(data_numeric, prior = .15*pollSize/n)
    probs <- sample_probs(probs_size = 500, posterior = posterior)
    results <- run_rcv(probs, 1:14)
    results
  })
  
  winners <- sample_results[1,]
  roundResults <- sample_results[2,]
  round23 <- roundResults[[23]]
  
  sample_winners <- t(sapply(1:numPolls, FUN = function(i) {
    table <- table(winners[i])
    results <- c(table[1], table[2], table[3], table[4], table[5])
    ind <- ifelse(sum(results, na.rm = TRUE)==500, TRUE, FALSE)
    results <- c(results, ind)
    results
  }))
  colnames(sample_winners) <- c("Adams", 
                                     "Garcia", 
                                     "Wiley",
                                     "Yang",
                                     "Stringer")
  sample_winners[is.na(sample_winners)] <- 0
  sample_winners
}

# Test for poll sizes 100 to 2000, take 100 polls of each.
#grid_pollSizes <- sapply(1:20, FUN = function(i) {
#  samplePolls(numPolls = 100, pollSize = 100*i, cands, primary_ballots)
#})
numPolls <- 500
#p = 100
grid_props <- sapply(seq(100,1500, by = 100), FUN = function(p){
  sample_polls <- sapply(1:numPolls, FUN = function(x) {
    primary_ballots[sample(nrow(primary_ballots), size = p 
                           # replace with pollSize outside of the grid.
    ),]
  }, simplify = FALSE)
  
  sample_results <- sapply(1:numPolls, FUN = function(i) {
    data_numeric <- preprocess_primary(raw_data = sample_polls[[i]],
                                       prune = TRUE, 
                                       candidates = cands)
    # Ensure the prior size is 15% of the poll size.
    n <- n_distinct(data_numeric$combined_rankings, na.rm = TRUE)
    posterior <- realPosterior(data_numeric, prior = .15*p/n)
    # Compute the "real" RCV winner for each poll.
    probs <- sample_probs(probs_size = probs_size, posterior = posterior)
    results <- run_rcv(probs, 1:5)
    results[[1]]
  })
  
  # The output is a 500 x numPolls. Each column is results from one poll.
  sample_results
})

# Used ChatGPT here. Gets the avg. and sd of proportions across poll sizes.
get_proportions <- function(col) {
  m <- matrix(col, nrow = probs_size, ncol = numPolls)
  
  counts <- sapply(1:5, function(k) {
    colSums(m == k)
  })
  
  # counts: 100 x 3 → transpose to 3 x 100
  props <- t(counts) / probs_size
  
  list(
    mean = rowMeans(props),
    sd   = apply(props, 1, sd)
  )
}

sample_props <- lapply(1:15, function(j) get_proportions(grid_props[, j]))

means <- sapply(sample_props, `[[`, "mean")  # 3 x 10
sds   <- sapply(sample_props, `[[`, "sd")    # 3 x 10
se <- sds/sqrt(100)

###############################################################################
cands <- c("Eric L. Adams",
           "Maya D. Wiley",
           "Kathryn A. Garcia",
           "Andrew Yang",
           "Scott M. Stringer")
numPolls <- 500
#p = 100
grid_props <- sapply(seq(100,1500, by = 100), FUN = function(p){
  sample_polls <- sapply(1:numPolls, FUN = function(x) {
    primary_ballots[sample(nrow(primary_ballots), size = p 
                           # replace with pollSize outside of the grid.
    ),]
  }, simplify = FALSE)
  
  sample_results <- sapply(1:numPolls, FUN = function(i) {
    data_numeric <- preprocess_primary(raw_data = sample_polls[[i]],
                                       prune = TRUE, 
                                       candidates = cands)
    # Ensure the prior size is 15% of the poll size.
    n <- n_distinct(data_numeric$combined_rankings, na.rm = TRUE)
    posterior <- realPosterior(data_numeric, prior = .15*p/n)
    # Compute the "real" RCV winner for each poll.
    probs <- sample_probs(probs_size = probs_size, posterior = posterior)
    results <- run_rcv(probs, 1:5)
    results[[1]]
  })
  
  # The output is a 500 x numPolls. Each column is results from one poll.
  sample_results
})

# Used ChatGPT here. Gets the avg. and sd of proportions across poll sizes.
get_proportions <- function(col) {
  m <- matrix(col, nrow = probs_size, ncol = numPolls)
  
  counts <- sapply(1:5, function(k) {
    colSums(m == k)
  })
  
  # counts: 100 x 3 → transpose to 3 x 100
  props <- t(counts) / probs_size
  
  list(
    mean = rowMeans(props),
    sd   = apply(props, 1, sd)
  )
}

sample_props <- lapply(1:15, function(j) get_proportions(grid_props[, j]))

means <- sapply(sample_props, `[[`, "mean")  # 3 x 10
sds   <- sapply(sample_props, `[[`, "sd")    # 3 x 10
se <- sds/sqrt(100)

###############################################################################
winners <- sample_results[1,]
winners
pruned <- sapply(1:100, FUN = function(iter){
  table(winners[,iter])
})
# Compare the point estimates and confidence intervals.
pruned_results <- sapply(1:3, FUN = function(y) {
  results_pruned <- sapply(1:100, FUN = function(x) {
    pruned[[x]][y]/500
  })
})

# The mean and CI of the win proportions repeated 100 times
sapply(1:3, FUN = function(x) {
  mean(pruned_results[,x], na.rm = TRUE)
})

#sapply(1:3, FUN = function(x) {
#  c(mean(pruned_results[,x], na.rm = TRUE) - 1.96*
#      sqrt(var(pruned_results[,x], na.rm = TRUE)/100), 
#    mean(pruned_results[,x], na.rm = TRUE) + 1.96*
#      sqrt(var(pruned_results[,x], na.rm = TRUE)/100))

winners_100_200 <- samplePolls(numPolls = 100, pollSize = 200, cands = cands,
                               primary_ballots = primary_ballots)
winners_500_200 <- samplePolls(numPolls = 500, pollSize = 200, cands = cands,
                               primary_ballots = primary_ballots)
winners_100_2000 <- samplePolls(numPolls = 100, pollSize = 2000, cands = cands,
                                primary_ballots = primary_ballots)
winners_500_2000 <- samplePolls(numPolls = 500, pollSize = 2000, cands = cands,
                                primary_ballots = primary_ballots)

c(quantile(winners_100_200[,1], c(0.025, 0.975)),
quantile(winners_100_200[,2], c(0.025, 0.975)),
quantile(winners_100_200[,3], c(0.025, 0.975)))

boxplot(winners_100_200[,1:3],
        names = colnames(winners_100_200[,1:3]),
        main = "Boxplots of Wins with 100 Polls of Sample Size 200",
        ylab = "Wins")

c(quantile(winners_500_200[,1], c(0.025, 0.975)),
quantile(winners_500_200[,2], c(0.025, 0.975)),
quantile(winners_500_200[,3], c(0.025, 0.975)))

boxplot(winners_500_200[,1:3],
        names = colnames(winners_500_200[,1:3]),
        main = "Boxplots of Wins with 500 Polls of Sample Size 200",
        ylab = "Wins")

c(quantile(winners_100_2000[,1], c(0.025, 0.975)),
quantile(winners_100_2000[,2], c(0.025, 0.975)),
quantile(winners_100_2000[,3], c(0.025, 0.975)))

boxplot(winners_100_2000[,1:3],
        names = colnames(winners_100_2000[,1:3]),
        main = "Boxplots of Wins with 100 Polls of Sample Size 2000",
        ylab = "Wins")

c(quantile(winners_500_2000[,1], c(0.025, 0.975)),
quantile(winners_500_2000[,2], c(0.025, 0.975)),
quantile(winners_500_2000[,3], c(0.025, 0.975)))

boxplot(winners_500_2000[,1:3],
        names = colnames(winners_500_2000[,1:3]),
        main = "Boxplots of Wins with 500 Polls of Sample Size 2000",
        ylab = "Wins")

###############################################################################
# Take 500 polls of size 1000 and see how often it yields a bad prune
numPolls = 100
pollSize = 500

# The pruned list to check against
cand_three <- c(1,2,3)
cand_four <- c(cand_three, 4)
cand_five <- c(cand_four, 5)

grid_prune <- sapply(seq(100,1000, by = 100), FUN = function(p){
  sample_polls <- sapply(1:numPolls, FUN = function(x) {
    primary_ballots[sample(nrow(primary_ballots), size = p 
                           # replace with pollSize outside of the grid.
                           ),]
  }, simplify = FALSE)
  
  sample_results <- sapply(1:numPolls, FUN = function(i) {
    data_numeric <- preprocess_primary(raw_data = sample_polls[[i]],
                                       prune = FALSE, 
                                       candidates = cands)
    # Ensure the prior size is 15% of the poll size.
    n <- n_distinct(data_numeric$combined_rankings, na.rm = TRUE)
    posterior <- realPosterior(data_numeric, prior = .15*p/n)
    # Compute the "real" RCV winner for each poll.
    probs <- t(as.matrix(posterior/sum(posterior)))
    results <- run_rcv(probs, 1:14)
    results
  })
  
  winners <- sample_results[1,]
  roundResults <- sample_results[2,]
  
  rounds <- roundResults[[i]]
  
  cand_results <- t(sapply(1:numPolls, FUN = function(i){

    # Get the rounds with 3, 4, 5 candidates left.
    three_round <- which(rowSums(rounds != 0) == 3)
    four_round <- which(rowSums(rounds != 0) == 4)
    five_round <- which(rowSums(rounds != 0) == 5)
    
    # Which candidates are left in the rounds with 3, 4, 5 candidates?
    three_cands <- which(rounds[three_round,,] != 0)
    four_cands <- which(rounds[four_round,,] != 0)
    five_cands <- which(rounds[five_round,,] != 0)
    
    # Check if prunings match poll results
    c(identical(sort(as.numeric(unname(three_cands))), 
                sort(as.numeric(unname(cand_three)))),
      identical(sort(as.numeric(unname(four_cands))), 
                sort(as.numeric(unname(cand_four)))),
      identical(sort(as.numeric(unname(five_cands))), 
                sort(as.numeric(unname(cand_five)))))
  }))
  
  # Check how many polls fail pruning to last 3, 4, 5 candidates respectively
  c("Prune 3" = mean(cand_results[,1]), 
    "Prune 4" = mean(cand_results[,2]),
    "Prune 5" = mean(cand_results[,3]))
})

colnames(grid_prune) <- seq(100, 1000, by = 100)

grid_prune <- as.data.frame(grid_prune)

###############################################################################
# Spreads for a poll of size 200 #

winners <- do.call(rbind, sample_results_200[1,])

# Compute the average spread
round_results <- sample_results_200[2,]
percents <- apply(round_results[[1]], 2, max)
margins <- 2*percents - 1

second_place <- sapply(1:500, FUN = function(j){
  result <- order(round_results[[1]][,j], decreasing = TRUE)[2]
  result
})


tab <- data.frame(Margin = margins, 
                  Winner = winners[1,], 
                  Second = second_place)

# Computing average margin of victory + a 95 CI for each candidate's wins. 
# These are results from the first poll with n = 200
# estimates: Adams: 0.0768111, Garcia: 0.04770592, Wiley: 0.0392596
# CI: Adams: [0.02283425, 0.13078795] Garcia: [-0.1439759,  0.2393877]
#     Wiley: [-0.0343391,  0.1128583]
adams_est <- mean(tab$Margin[tab$Winner==1])
adams_est
c(adams_est - 1.96*2*sqrt(adams_est*(1-adams_est)/sum(tab$Winner==1)), 
  adams_est + 1.96*2*sqrt(adams_est*(1-adams_est)/sum(tab$Winner==1)))

garcia_est <- mean(tab$Margin[tab$Winner==2])
garcia_est
c(garcia_est - 1.96*2*sqrt(garcia_est*(1-garcia_est)/sum(tab$Winner==2)), 
  garcia_est + 1.96*2*sqrt(garcia_est*(1-garcia_est)/sum(tab$Winner==2)))

wiley_est <- mean(tab$Margin[tab$Winner==3])
wiley_est
c(wiley_est - 1.96*2*sqrt(wiley_est*(1-wiley_est)/sum(tab$Winner==3)), 
  wiley_est + 1.96*2*sqrt(wiley_est*(1-wiley_est)/sum(tab$Winner==3)))
###############################################################################

### Maybe do this as a background job when we have time. ###
# Trying to run with N = 10000 
sample_polls_10000 <- sapply(1:1000, FUN = function(x) {
  primary_ballots[sample(nrow(primary_ballots), size = 10000),]
}, simplify = FALSE)

# Sample 1,000 polls each with N = 10,000, unpruned.
# Goal is to see if there are any instances where someone other than 
# Adams, Wiley, Garcia win. 
# Or maybe even if someone other than them is final 3.
sample_results_10000 <- sapply(1:1000, FUN = function(i) {
  data_numeric <- preprocess_primary(raw_data = sample_polls_10000[[i]],
                                     prune = FALSE)
  posterior <- realPosterior(data_numeric, prior = .1)
  probs <- sample_probs(probs_size = 500, posterior = posterior)
  results <- run_rcv(probs, 1:14)
  results[1]
})

# Check if there are more than three columns in the table.

sample_winners_20000 <- t(sapply(1:500, FUN = function(i) {
  table <- table(sample_results_20000[[i]])
  results <- c(table[1], table[2], table[3])
  results
}))
colnames(sample_winners_20000) <- c("Adams", "Garcia", "Wiley")
sample_winners_20000[is.na(sample_winners_20000)] <- 0

c(min(sample_winners_20000[,1]), max(sample_winners_20000[,1]))
c(min(sample_winners_20000[,2]), max(sample_winners_20000[,2]))
c(min(sample_winners_20000[,3]), max(sample_winners_20000[,3]))

boxplot(sample_winners_20000,
        names = colnames(sample_winners_20000),
        main = "Boxplots of Wins over Sample Size 20000",
        ylab = "Wins")
quantile(sample_winners_20000[,1], c(0.025, 0.975))
quantile(sample_winners_20000[,2], c(0.025, 0.975))
quantile(sample_winners_20000[,3], c(0.025, 0.975))
###############################################################################
# Get the proportion of Bootstrap polls that agree on candidates.
pollSize = 500
poll <- primary_ballots[sample(nrow(primary_ballots), size = pollSize),]

B = 1500

# Over 1500 bootstrap samples, get the final 2, 3, 4, ... candidates left
results <- sapply(1:B, FUN = function(x){
  # Goal is to get a list of vectors -- candidates in each round and compare. 
  boot_sample <- sample(poll, size = nrow(poll), replace = TRUE)
  
  # Since the polls are sampled from the CVR, we use preprocess_primary.
  # Alternatively, we can apply this to the real DFP poll.
  data_numeric <- preprocess_primary(raw_data = boot_sample,
                                     prune = FALSE)
  # Ensure the prior size is 15% of the poll size.
  n <- n_distinct(data_numeric$combined_rankings, na.rm = TRUE)
  # Be careful about pollSize here.
  posterior <- realPosterior(data_numeric, prior = .15*pollSize/n)
  # Compute the "real" RCV winner for each poll.
  probs <- t(as.matrix(posterior/sum(posterior)))
  results <- run_rcv(probs, 1:14)
  rounds <- results[[2]]
  round_by_round <- rev(sapply(1:nrow(rounds), FUN = function(i) {
    which(rounds[i,,] != 0)
  }))
  round_by_round
})

# Note ChatGPT wrote parts of this function
match_proportions <- sapply(1:(nrow(results) - 1), FUN = function(i){
  # For simplicity, ignore the last round.
  each_round <- results[i,]
  round_matrix <- if (i == nrow(results)) {
    t(sapply(each_round, function(x) {
      c(x, NA)[1:14] }))
  } else {
    do.call(rbind, each_round)
  }
  
  dimnames(round_matrix) <- NULL
  
  uniq <- unique(round_matrix)
  
  counts <- apply(uniq, 1, function(row) {
    sum(apply(round_matrix, 1, function(x) 
      all((x == row) | (is.na(x) & is.na(row)))))
  })
  
  out <- as.data.frame(cbind(uniq, count = counts))
  
  # Account for different orderings of the same set of candidates
  vals <- out[ , setdiff(names(out), "count")]
  canon <- t(apply(vals, 1, sort))
  keys <- apply(canon, 1, function(x) {
    x[is.na(x)] <- "NA"
    paste(x, collapse = "-")
  })
  res <- aggregate(out$count, by = list(key = keys), FUN = sum)
  names(res)[2] <- "total_count"
  res <- res[order(res$total_count, decreasing = TRUE), ]
  
  # Return percent of matches
  comply_rate <- res$total_count[1]/1500
  comply_rate
})
