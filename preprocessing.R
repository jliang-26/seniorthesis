source("constants.R")
source("helperFunctions.R")

# To do: named list of all 500 runs, transpose rows sand columns.

# This script preprocesses the dfp final NYC 2021 poll. 
dfp_raw_data <- read.csv("Datasets/dfp_nyc_pre_election_2021_final_v2_e5_0020040_micro.csv")

#####
# Hypothetical mMoE for mayor. Doesn't match the reported MoE, so unlikely
s <- sd(dfp_raw_data$weight)
m <- mean(dfp_raw_data$weight)
CV <- s/m
deff <- 1 + CV^2
n = nrow(dfp_raw_data) - sum(dfp_raw_data$weight == 0)
neff <- n/deff
MoE <- 1.96/(2*sqrt(neff))
#####

# This gives the posterior dirichlet distribution for all unique ballots.
#   preprocess_dfp(raw_data = dfp_raw_data, 
#                   weighted = 1,
#                   race = "mayor",
#                   prune = FALSE,
#                   candidates = NA
#                   prior = prior)
# weighted: whether or not we use the weighted or unweighted responses
# race: "mayor" or "comptroller"
# prune: If you plan to prune candidates, prune = TRUE
# Note that if prune is true, you also have to set candidates.
# candidates: character vector; specify which candidates remain.
# prior: specify a prior. Default is flat (1)

# Specify the candidates for pruning (only do this if prune = TRUE)
cands <- c("Eric Adams",
           "Kathryn Garcia",
           "Maya Wiley"
           #"Andrew Yang",
           #"Scott Stringer"
           )

posterior <- preprocess_dfp(raw_data = dfp_raw_data, 
                            weighted = 1,
                            prune = FALSE,
                            candidates = cands,
                            prior = 0
                            )
post_probs_pruned <- t(as.matrix(posterior/sum(posterior)))


# Probs is a 500 x 807 matrix. Each row corresponds to one sample of 
# probabilities from the Dirichlet posterior given the 807 unique rankings
probs <- sample_probs(probs_size = probs_size, posterior = posterior)

rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)[[1]]
table(rcvOutputs[[1]])
# Sort by ascending weight.
# sorted_weighted_rankings <- culled_weighted_sum_rankings %>%
#   arrange(desc(total))

# Compare if there is a difference in win percentage between pruned and unpruned.
# Maybe don't record round data to speed up the process. IDK..?
posterior <- preprocess_dfp(raw_data = dfp_raw_data, 
                            weighted = 1,
                            prune = FALSE)
non_pruned <- sapply(1:500, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = posterior)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)
  table(rcvOutputs[1])
})

posterior_pruned <- preprocess_dfp(raw_data = dfp_raw_data, 
                            weighted = 1,
                            prune = TRUE,
                            candidates = cands)
pruned <- sapply(1:100, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = posterior_pruned)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)
  table(rcvOutputs[[1]])
})

# Compare the point estimates and confidence intervals.
pruned_results <- sapply(1:3, FUN = function(y) {
  results_pruned <- sapply(1:100, FUN = function(x) {
    pruned[[x]][y]/500
  })
})

#non_pruned_results <- sapply(1:3, FUN = function(y) {
#  results_non_pruned <- sapply(1:500, FUN = function(x) {
#    non_pruned[[x]][y]/500
#  })
#})

# The mean of the win proportions repeated 100 times
sapply(1:3, FUN = function(x) {
  mean(pruned_results[,x], na.rm = TRUE)
})

# The SD of the win proportions repeated over 100 times
sapply(1:3, FUN = function(x) {
  sqrt(var(pruned_results[,x], na.rm = TRUE)) 
})

# Therefore, the CI:
sapply(1:2, FUN = function(x) {
  c(mean(pruned_results[,x], na.rm = TRUE) - 1.96*
      sqrt(var(pruned_results[,x], na.rm = TRUE)/100), 
    mean(pruned_results[,x], na.rm = TRUE) + 1.96*
      sqrt(var(pruned_results[,x], na.rm = TRUE)/100))
})

# For 500 prob. samples from posterior, 
# Mean is 0.563960000 0.428300000 0.007897959
# SD is 0.022511016 0.021996097 0.003835509
# CI is 

m <- c(0.563960000, 0.428300000, 0.007897959)

sd <- c(0.022511016, 0.021996097, 0.003835509)

as.matrix(c(m - 1.96*sd/sqrt(100), m + 1.96*sd/sqrt(100)))

# Scaling factor
C = sum(posterior)
cands <- c("Eric Adams",
           "Kathryn Garcia",
           "Maya Wiley"
           #"Andrew Yang",
           #"Scott Stringer"
           )

posterior <- preprocess_dfp(raw_data = dfp_raw_data, 
                            weighted = 1,
                            prune = TRUE,
                            candidates = cands
                            )
post_probs <- t(as.matrix(posterior/sum(posterior)))
rcv_results <- sapply(1:100, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = posterior)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = 1:3)
})
winners <- rcv_results[1,]
pruned <- sapply(1:100, FUN = function(iter){
  table(winners[iter])
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

sapply(1:3, FUN = function(x) {
  c(mean(pruned_results[,x], na.rm = TRUE) - 1.96*
      sqrt(var(pruned_results[,x], na.rm = TRUE)/100), 
    mean(pruned_results[,x], na.rm = TRUE) + 1.96*
      sqrt(var(pruned_results[,x], na.rm = TRUE)/100))
})

# Rounds
rounds <- rcv_results[2,]

# For each of the 100 draws of 500, proportion of times Garcia in final round
final_two <- sapply(1:100, FUN = function(iter){
  round <- rounds[[iter]]
  # This gives the proportion of times Garcia is in final round.
  g <- sapply(1:500, FUN = function(r){
    ifelse(sum(which(round[2,,r] != 0)) == 4, 0 # 0 if Wiley
           , 1) # 1 if Garcia
  })
  mean(g)
})

# mean and CI for this result
c(mean(final_two) - 1.96*
    sqrt(var(final_two)/100), 
  mean(final_two) + 1.96*
    sqrt(var(final_two)/100))
# Times Garcia in final round (Garcia wins)
# [1] 0.45232
# Times Wiley in final round (Adams wins)
# [1] 0.54768
# [1] 0.4474063 0.4572337

# Where the final matchup is Adams v. Garcia:         
#           [,1]     [,2]
#[1,] 0.06266397 0.933264
#[2,] 0.06673603 0.937336

# Where the final matchup is Adams v. Wiley:
#          [,1]        [,2]
#[1,] 0.9917172 0.006803297
#[2,] 0.9932828 0.008348218

# Final results from method (CIs)
#           [,1]      [,2]        [,3]
# [1,] 0.5532412 0.4304948 0.006975430
# [2,] 0.5624388 0.4397452 0.008328917

# Think of the Adams win percent as:
# 0.45*0.065+ 0.55*0.99

# Garcia win prob as:
# 0.45*0.93

# Wiley win prob as:
# 0.55*0.007

#sapply(1:3, FUN = function(x) {
#  abs(mean(non_pruned_results[,x], na.rm = TRUE)) 
#})
  
sapply(1:3, FUN = function(x) {
  abs(sqrt(var(pruned_results[,x], na.rm = TRUE)) - 
        sqrt(var(non_pruned_results[,x], na.rm = TRUE)))
})

# See what happens with an incorrect pruning.
improper_cands <- c("Eric Adams",
           "Kathryn Garcia",
           "Maya Wiley",
           "Scott Stringer")

post_bad_prune <- preprocess_dfp(raw_data = dfp_raw_data, 
                                   weighted = 1,
                                   prune = TRUE,
                                   candidates = improper_cands)
bad_prune <- sapply(1:500, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = post_bad_prune)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)
  table(rcvOutputs)
})

bad_prune_results <- sapply(1:3, FUN = function(y) {
  results_bad_pruned <- sapply(1:500, FUN = function(x) {
    bad_prune[[x]][y]/500
  })
})

sapply(1:3, FUN = function(x) {
  abs(mean(bad_prune_results[,x], na.rm = TRUE)) 
})

# sum(dfp_ballots_by_num_of_ranked_cands) = 1291.501.
# sum(unweighted) = 1552.
unweighted <- c(123, 121, 245, 210, 853)
100*unweighted/sum(unweighted)
100*dfp_ballots_by_num_of_ranked_cands/sum(dfp_ballots_by_num_of_ranked_cands)
#                          1        2        3       4         5
# Unweighted results:    123      121      245      210      853
#   By %              (7.93)   (7.80)  (15.79)  (13.53)  (54.96)
# Weighted results: 158.7877 123.5258 198.9588 169.0119 641.2163
#   By %             (12.29)   (9.56)  (15.41)  (13.09)  (49.65)

################################################################################
# BOOTSTRAP THE RESULTS OF MAYOR.
# Use the weights as the probability of being sampled in the bootstrap
# For each sample of the bootstrap, I draw N = 353 times from the poll w/ replacement.
mayor_ranking_cols <- c("weight",
                  "rank_preference_sure_1",
                  "rank_preference_sure_2",
                  "rank_preference_sure_3",
                  "rank_preference_sure_4",
                  "rank_preference_sure_5")
dfp_rankings <- raw_data[, mayor_ranking_cols]

# convert names to numeric following names mapping
dfp_rankings_numeric <- dfp_rankings %>%
  mutate(across(where(is.character), ~ unname(candidate_names[.x])))

# Combine the numeric rankings and make fully blank rankings NA
dfp_rankings_numeric$combined_rankings <- apply(dfp_rankings_numeric[, 2:6], 1, function(row)
  paste(row[!is.na(row)], collapse = "-")
)
dfp_rankings_numeric$combined_rankings[dfp_rankings_numeric$combined_rankings == ""] <- NA
dfp_rankings_numeric <- dfp_rankings_numeric[!is.na(dfp_rankings_numeric$combined_rankings), ]

bootSamples <- sapply(1:1000, FUN = function(i) {
  sample(dfp_rankings_numeric$combined_rankings[1:nrow(dfp_rankings_numeric)], 
         replace = TRUE, 
         prob = dfp_rankings_numeric$weight/sum(dfp_rankings_numeric$weight))
})

unique_dfp_mbp_rankings <- unique(dfp_rankings_numeric$combined_rankings)

# Compute the RCV winner for each sample from the bootstrap. 
bootResults <- sapply(1:ncol(bootSamples), FUN = function(i) {
  # bootCounts returns how many times each possible ranking appears.
  # combined_rankings gives all possible rankings that might show up in bootstrap
  bootCounts = t(as.matrix(table(factor(bootSamples[,i], 
                                        levels = unique_dfp_mbp_rankings))))
  
  # Rescale counts to sum to 1 so that it fits in the RCV function & compute winner.
  rcv_input <- bootCounts/sum(bootCounts)
  bootWinner <- run_rcv(rcv_input, candidates = 1:14)
  bootWinner[[1]]
})

# With the correct weights, I get over 100,000 samples,
#     2     5 
# 95.809%  4.191%
# (Levine  Hoylman)

table(bootResults)

############################################
##    Manhattan Borough President Poll    ##
############################################
# First, get the RCV results under the poll.
dfp_mbp_poll_numeric <- preprocess_mbp_dfp(dfp_raw_data, 
                                           weighted = 1,
                                           prune = FALSE,
                                           candidates = NA)
mbp_true_poll <- aggregate_mbp_rankings(dfp_mbp_poll_numeric,
                                       prior = 0)

mbp_posterior <- mbp_rankings %>%
  dplyr::pull(total, name = combined_rankings)

true_mbp_probs <- t(as.matrix(mbp_posterior/sum(mbp_posterior)))
true_mbp_poll <- run_rcv(true_mbp_probs, 1:6)
# MoE of 5\% if computed the same way as for Mayor.
#        Candidate 1 Candidate 2 Candidate 3 Candidate 4 Candidate 5 Candidate 6
#Round 1   0.3319623   0.2835119  0.08311916   0.1853124  0.09337261  0.02272162
#Round 2   0.3396804   0.2901035  0.08505167   0.1896209  0.09554351  0.00000000
#Round 3   0.3649099   0.3096810  0.00000000   0.2210943  0.10431478  0.00000000
#Round 4   0.4103737   0.3459328  0.00000000   0.2436935  0.00000000  0.00000000
#Round 5   0.5312985   0.4687015  0.00000000   0.0000000  0.00000000  0.00000000
# This returns the total weight for each unique ranking that appears in the poll
# sorted_mbp_rankings gives the weight table
# There are 451 unique poll responses (rows)
# The sum of weights is 450.5042

mbp_prune = c("Mark Levine",
              "Benjamin Kallos")
dfp_mbp_poll_numeric <- preprocess_mbp_dfp(dfp_raw_data, 
                                           weighted = 1,
                                           prune = TRUE,
                                           candidates = mbp_prune)
mbp_rankings <- aggregate_mbp_rankings(dfp_mbp_poll_numeric,
                                       #prior = 0)
                                       prior = 0.1*451/55)

mbp_posterior <- mbp_rankings %>%
  dplyr::pull(total, name = combined_rankings)

true_mbp_probs <- t(as.matrix(mbp_posterior/sum(mbp_posterior)))

# Run the Bayesian method 100 times and get average win proportions.
mbp_results <- sapply(1:100, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = mbp_posterior)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)
})

mbp_winners <- mbp_results[1,]

mbp_wins <- sapply(1:100, FUN = function(iter){
  table(mbp_winners[iter])
})

# Compare the point estimates and confidence intervals.
mbp_win_table <- sapply(1:2, FUN = function(y) {
  results_pruned <- sapply(1:100, FUN = function(x) {
    mbp_wins[[x]][y]/500
  })
})

# The mean of the win proportions repeated 100 times
sapply(1:2, FUN = function(x) {
  mean(mbp_win_table[,x], na.rm = TRUE)
})

# The SD of the win proportions repeated over 100 times
sapply(1:3, FUN = function(x) {
  sqrt(var(mbp_win_table[,x], na.rm = TRUE)) 
})

# Therefore, the CI:
sapply(1:2, FUN = function(x) {
  c(mean(mbp_win_table[,x], na.rm = TRUE) - 1.96*
      sqrt(var(mbp_win_table[,x], na.rm = TRUE)/100), 
    mean(mbp_win_table[,x], na.rm = TRUE) + 1.96*
      sqrt(var(mbp_win_table[,x], na.rm = TRUE)/100))
})

# Win proportions
#[,1]      [,2]
#[1,] 0.8499439 0.1454416
#[2,] 0.8545361 0.1500384

# Average
# [1] 0.85224 0.14774 

# Check whether final round matchup is always Levine Hoylman
mbp_rounds <- mbp_results[2,]

# For each of the 100 draws of 500, get the final round matchups
mbp_final_two <- sapply(1:100, FUN = function(iter){
  round <- mbp_rounds[[iter]]
  # This gives the proportion of times Garcia is in final round.
  g <- sapply(1:500, FUN = function(r){
    x <- max(which(rowSums(round[,,r] != 0) > 0))
    sum(which(round[x,,r] != 0))
    # If there is a final-round 2 cand matchup
    #if (sum(round[x,,r] != 0) == 2) {
      # if the matchup is Lev-Hoyl
    #  if (sum(which(round[x,,r] != 0)) == 3) {
    #    temp = 2
    #    indices = NA
      # If the matchup is instead Levine Kallos
    #  } else if (sum(which(round[x,,r] != 0)) == 5){
    #    temp = 1
    #    indices = NA
    #  } else {
    #    temp = 0
        #indices <- which(round[x,,r] != 0)
    #    indices = NA
    #  }
    # If there is 3-cand final round.
    #} else if (sum(round[x,,r] != 0) == 3) {
    #  temp = sum(which(round[x,,r] != 0))
    #  indices <- which(round[x,,r] != 0)
    #}
    #data.frame(temp = temp, indices = indices)
  })
  #mean(g)
  g
})

# Unique values here are 3, 5, 7, 6
# 3: Lev-Hoyl
# 5: Lev-Kallos
# 7: Lev-Hoyl-Kallos
# 6: Hoyl-Kallos
# Switch 6 with any
# which(mbp_final_two == 6, arr.ind = TRUE)

combos <- c(3, 5, 6, 7)

result <- sapply(combos, function(v) {
  colMeans(mbp_final_two == v)
})

result <- as.data.frame(result)
colnames(result) <- combos

#      3      5      6      7 
# 0.9926 0.0069 0.0001 0.0004 

# mean and CI for this result
c(mean(final_two) - 1.96*
    sqrt(var(final_two)/100), 
  mean(final_two) + 1.96*
    sqrt(var(final_two)/100))

###############################################################################
# Bootstrap the pruning list for mbp.
# [1] 0.7140000 0.8806667 0.9066667 1.0000000
# We can only prune out one candidate (Watkins)
B = 1500

poll <- dfp_mbp_poll_numeric

# Over 1500 bootstrap samples, get the final 2, 3, 4, ... candidates left
results <- sapply(1:B, FUN = function(x){
  # Goal is to get a list of vectors -- candidates in each round and compare. 
  boot_sample <- poll[
    sample(
      seq_len(nrow(poll)), 
      size = nrow(poll),
      replace = TRUE, 
      prob = poll$weight
    ),
  ]
  
  # Since the polls are sampled from the CVR, we use preprocess_primary.
  # Alternatively, we can apply this to the DFP poll.
  proc <- aggregate_mbp_rankings(boot_sample)
  processed <- proc %>%
    dplyr::pull(total, name = combined_rankings)
  
  # Compute the "real" RCV winner for each poll.
  probs <- t(as.matrix(processed/sum(processed)))
  results <- run_rcv(probs, 1:6)
  rounds <- results[[2]]
  round_by_round <- rev(sapply(1:nrow(rounds), FUN = function(i) {
    which(rounds[i,,] != 0)
  }))
  round_by_round
})

match_proportions <- sapply(1:(nrow(results) - 1), FUN = function(i){
  # For simplicity, ignore the last round.
  each_round <- results[i,]
  round_matrix <- do.call(rbind, each_round)
  
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


######################## Preliminary Analysis of the data #####################
# For reference, the number of ballots that ranked 1, 2, 3 candidates:
rankCount <- lengths(regmatches(dfp_mbp_poll_numeric[[1]]$combined_rankings, 
                                gregexpr("-", 
                                         dfp_mbp_poll_numeric[[1]]$combined_rankings)))
tapply(dfp_mbp_poll_numeric[[1]]$weight, rankCount, sum)/sum(tapply(dfp_mbp_poll_numeric[[1]]$weight, rankCount, sum))

# Check the first-choice results of our data to see if it matches published poll results.
# 2          5          4          3          1          6 
# 0.34677920 0.27123162 0.19466739 0.08962680 0.07449325 0.02320174 
# Levine, Hoylman, Kallos, Boylan, Caputo, Watkins
# This matches closely to the actual results.

weights_by_cand <- sort(tapply(sorted_mbp_rankings$total, 
       substr(sorted_mbp_rankings$combined_rankings, 1, 1), sum), 
       decreasing = TRUE)
weights_by_cand/sum(weights_by_cand)

# Get all other possible orderings and assign a weight of 0.5 if unobserved.
# Definitely need to edit this later if we want to generalize.
all_rankings <- as.matrix(permutations(6, 3))
combined_rankings <- apply(all_rankings, 1, function(row)
  paste(row[!is.na(row)], collapse = "-")
)
all_rankings2 <- as.matrix(permutations(6, 2))
rankings <- apply(all_rankings2, 1, function(row)
  paste(row[!is.na(row)], collapse = "-")
)
combined_rankings <- c(combined_rankings, rankings, c(1:6))

missing_rankings <- setdiff(combined_rankings, sorted_mbp_rankings$combined_rankings)

missing_weights <- data.frame(
  combined_rankings = missing_rankings,
  total = 0.5
)
# Complete_rankings gives a complete tibble with 156 unique "combined_rankings"
# and their weights, 0.5 assigned to orderings that don't appear in the poll.
complete_rankings <- rbind(sorted_mbp_rankings, missing_weights)

# Compute the posterior based on a prior size of 10% of total poll weight.
prior <- 0.1*sum(complete_rankings$total)/nrow(complete_rankings)

dfp_mbp_posterior <- complete_rankings$total + prior
names(dfp_mbp_posterior) <- complete_rankings$combined_rankings

# Alternatively, test different values of the Dirichlet prior size.
priors <- seq(from = 0.001, to = 1, length.out = 50)

grid_data <- complete_rankings$total
names(grid_data) <- complete_rankings$combined_rankings

winners_grid <- sapply(priors, FUN = function(prior) {
  posterior <- grid_data + prior
  
  
  dfp_mbp_probs <- sample_probs(probs_size = probs_size, 
                                posterior = posterior)
  
  rcvOutputs <- run_rcv(sample_probs = dfp_mbp_probs, candidates = 1:6)
  rcvOutputs[[1]]
})

# Go to visualizations to create a plot of this.
tab <- table(col(winners_grid), winners_grid)

# The first-choice posterior is given by (ignoring rankings)
#   Caputo   Levine  Hoylman   Boylan  Watkins   Kallos
# 30.32746 85.42098 82.79713 33.08336 16.46704 62.10249

# After fixing weights, the first-choice posterior is :
#          2          5          4          3          1          6 
# 0.30246364 0.24342031 0.18691711 0.10773767 0.09791968 0.06154159
first_choice_candidates <- substr(names(dfp_mbp_posterior), 1, 1)
first_choice_posterior <- tapply(dfp_mbp_posterior, first_choice_candidates, sum)
names_mapping <- dfp_mbp_poll_numeric[2]

first_choice_probs <- sample_probs(probs_size = 500,
                                   posterior = first_choice_posterior)

# The results are (Levine, Hoylman, Kallos)
#     2     3    6 
#   266   232    2
# 53.2% 46.4% .04%
# This gives which of the candidates accumulated the highest probability over each 
# sampling from the Dirichlet posterior
# It can be interpreted as the probability of winning a first-to-the-post election.
first_choice_results <- run_rcv(sample_probs = first_choice_probs, candidates = 1:6)



# Bootstrapping the Manhattan Borough President Polls
# 1) Sample from the ballots with replacement nrow(ballot) times.
# 2) Compute the RCV winner 
# 3) Repeat 10,000 times and get win proportions?
# First, bootstrap by sampling 1e5 times a poll w/ replacement.
# In each sample, I draw the number of 
# Drop the rows where the poll ballot is just "NA". These were "Not sure".
# 353 rows remain. The official poll N is 353.
# The total weight is now ~353
dfp_mbp_poll<- dfp_mbp_poll_numeric[[1]]
dfp_mbp_poll <- dfp_mbp_poll[!is.na(dfp_mbp_poll$combined_rankings), ]

# Use the weights as the probability of being sampled in the bootstrap
# For each sample of the bootstrap, I draw N = 353 times from the poll w/ replacement.
bootSamples <- sapply(1:1e5, FUN = function(i) {
  sample(dfp_mbp_poll$combined_rankings[1:nrow(dfp_mbp_poll)], 
         replace = TRUE, 
         prob = dfp_mbp_poll$weight/sum(dfp_mbp_poll$weight))
})

unique_dfp_mbp_rankings <- unique(dfp_mbp_poll$combined_rankings)

# Compute the RCV winner for each sample from the bootstrap. 
bootResults <- sapply(1:ncol(bootSamples), FUN = function(i) {
  # bootCounts returns how many times each possible ranking appears.
  # combined_rankings gives all possible rankings that might show up in bootstrap
  bootCounts = t(as.matrix(table(factor(bootSamples[,i], 
                                        levels = unique_dfp_mbp_rankings))))
  
  # Rescale counts to sum to 1 so that it fits in the RCV function & compute winner.
  rcv_input <- bootCounts/sum(bootCounts)
  bootWinner <- run_rcv(rcv_input, candidates = 1:6)
  bootWinner[[1]]
})

# With the correct weights, I get over 100,000 samples,
#     2     5 
# 95.809%  4.191%
# (Levine  Hoylman)

table(bootResults)

###########################################
######         Real Ballots          ######
###########################################
primary_ballots <- read.csv("Datasets/nyc_mayor_dem_primary_ballots_ranked.csv")

### I NEED TO REDO THIS TO ACCOUNT FOR INCORRECTLY RANKED BALLOTS LATER ###
### Blank ballots and ballots with repeated candidates.
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

B = 1500

partproc_dfp <- function(dfp_raw_data){
  ranking_cols <- c("weight",
                    "rank_preference_sure_1",
                    "rank_preference_sure_2",
                    "rank_preference_sure_3",
                    "rank_preference_sure_4",
                    "rank_preference_sure_5")
  dfp_rankings <- dfp_raw_data[, ranking_cols]
  dfp_rankings
}

poll <- partproc_dfp(dfp_raw_data)

procproc_dfp <- function(boot_sample) {
  dfp_rankings_numeric <- boot_sample %>%
    mutate(across(where(is.character), ~ unname(candidate_names[.x])))
  
  # Combine the numeric rankings and make fully blank rankings NA
  dfp_rankings_numeric$combined_rankings <- apply(dfp_rankings_numeric[, 2:6], 
                                                  1, function(row)
                                                    paste(row[!is.na(row)], collapse = "-")
  )
  dfp_rankings_numeric$combined_rankings[dfp_rankings_numeric$combined_rankings == ""] <- NA
  
  weighted_sum_rankings <- dfp_rankings_numeric %>%
    # Ignore blank rankings (shows up as NA on combined)
    filter(!is.na(combined_rankings)) %>%
    # 
    group_by(combined_rankings) %>%
    summarise(total = sum(weight, na.rm = TRUE))
  
  # Remove weighted sums equal to zero.
  culled_weighted_sum_rankings <- weighted_sum_rankings %>% 
    filter(total != 0)
  
  # Compute the Dirichlet posterior
  posterior <- culled_weighted_sum_rankings$total
  names(posterior) <- culled_weighted_sum_rankings$combined_rankings
  posterior
}

# Over 1500 bootstrap samples, get the final 2, 3, 4, ... candidates left
results <- sapply(1:B, FUN = function(x){
  # Goal is to get a list of vectors -- candidates in each round and compare. 
  boot_sample <- poll[
    sample(
      seq_len(nrow(poll)), 
      size = nrow(poll),
      replace = TRUE, 
      prob = poll$weight
    ),
  ]
  
  # Since the polls are sampled from the CVR, we use preprocess_primary.
  # Alternatively, we can apply this to the DFP poll.
  processed <- procproc_dfp(boot_sample)
  
  # Compute the "real" RCV winner for each poll.
  probs <- t(as.matrix(processed/sum(processed)))
  results <- run_rcv(probs, 1:14)
  rounds <- results[[2]]
  round_by_round <- rev(sapply(1:nrow(rounds), FUN = function(i) {
    which(rounds[i,,] != 0)
  }))
  round_by_round
})

match_proportions <- sapply(1:(nrow(results) - 1), FUN = function(i){
  # For simplicity, ignore the last round.
  each_round <- results[i,]
  round_matrix <- do.call(rbind, each_round)
  
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

###############################################################################
#############################Comptroller#######################################
###############################################################################
# Bootstrap the comptroller poll
B = 1500

partproc_dfp <- function(dfp_raw_data){
  ranking_cols <- c("weight",
                    "comptroller_rank_preference_sure_1",
                    "comptroller_rank_preference_sure_2",
                    "comptroller_rank_preference_sure_3",
                    "comptroller_rank_preference_sure_4",
                    "comptroller_rank_preference_sure_5"
  )
  dfp_rankings <- dfp_raw_data[, ranking_cols]
  dfp_rankings
}

poll <- partproc_dfp(dfp_raw_data)

procproc_dfp <- function(boot_sample) {
  dfp_rankings_numeric <- boot_sample %>%
    mutate(across(where(is.character), ~ unname(comptroller_names[.x])))
  
  # Combine the numeric rankings and make fully blank rankings NA
  dfp_rankings_numeric$combined_rankings <- apply(dfp_rankings_numeric[, 2:6], 
                                                  1, function(row)
                                                    paste(row[!is.na(row)], collapse = "-")
  )
  dfp_rankings_numeric$combined_rankings[dfp_rankings_numeric$combined_rankings == ""] <- NA
  
  weighted_sum_rankings <- dfp_rankings_numeric %>%
    # Ignore blank rankings (shows up as NA on combined)
    filter(!is.na(combined_rankings)) %>%
    # 
    group_by(combined_rankings) %>%
    summarise(total = sum(weight, na.rm = TRUE))
  
  # Remove weighted sums equal to zero.
  culled_weighted_sum_rankings <- weighted_sum_rankings %>% 
    filter(total != 0)
  
  # Compute the Dirichlet posterior
  posterior <- culled_weighted_sum_rankings$total
  names(posterior) <- culled_weighted_sum_rankings$combined_rankings
  posterior
}

# Over 1500 bootstrap samples, get the final 2, 3, 4, ... candidates left
results <- sapply(1:B, FUN = function(x){
  # Goal is to get a list of vectors -- candidates in each round and compare. 
  boot_sample <- poll[
    sample(
      seq_len(nrow(poll)), 
      size = nrow(poll),
      replace = TRUE, 
      prob = poll$weight
    ),
  ]
  
  # Since the polls are sampled from the CVR, we use preprocess_primary.
  # Alternatively, we can apply this to the DFP poll.
  processed <- procproc_dfp(boot_sample)
  
  # Compute the "real" RCV winner for each poll.
  probs <- t(as.matrix(processed/sum(processed)))
  results <- run_rcv(probs, 1:9)
  rounds <- results[[2]]
  round_by_round <- rev(sapply(1:nrow(rounds), FUN = function(i) {
    which(rounds[i,,] != 0)
  }))
  round_by_round
})

match_proportions <- sapply(1:(nrow(results) - 1), FUN = function(i){
  # For simplicity, ignore the last round.
  each_round <- results[i,]
  round_matrix <- do.call(rbind, each_round)
  
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
# [1] 0.9013333 0.6706667 0.7953333 0.9946667 0.9986667 0.9586667 1.0000000
# Suggests we prune to 5 candidates. Note that there may be path dep.
# /uncertainty with the 3/4 candidates. Maybe we mention this but talk about
# How it's not relevant to the final result.
# 5 candidate list is 
cands <- c("Brad Lander" = 1, 
  "Kevin Parker" = 5, 
  "Corey Johnson" = 2, 
  "Michelle Caruso-Cabrera" = 3, 
  "David Weprin" = 4
  ) 
###############################################################################
# Run 100 draws of the RCV simulation, optionally prune with bootstrap results.

# Specify the candidates for pruning
comp_cands <- c("Brad Lander",
           "Kevin Parker", 
           "Corey Johnson", 
           "Michelle Caruso-Cabrera", 
           "David Weprin"
) 

#cols <- c("weight",
#  "comptroller_rank_preference_sure_1")

#cols <- c("weight", "comptroller_rank_preference_sure_1")

#testing_df <- dfp_raw_data[,cols]
#colnames(testing_df) <- c("weight", "cand")
#testing_df <- testing_df[testing_df$cand != "", ]
#aggregate(dfp_weight_nyc_dem_primary_2021_borough_weights_standard_sms_only_first_round_election_night_actuals ~ comptroller_rank_preference_sure_1, data = testing_df, sum)

# Get the real posterior
comp_post <- preprocess_dfp(raw_data = dfp_raw_data, 
                            weighted = 1,
                            race = "comptroller",
                            prune = FALSE,
                            candidates = comp_cands,
                            prior = 0
)
post_probs_pruned <- t(as.matrix(comp_post/sum(comp_post)))
test <- run_rcv(sample_probs = post_probs_pruned, candidates = 1:9)
colnames(test[[2]]) = sorted_comp
# 445 unique rankings, total weight of ~1024 non blank responses

# Check poll responses by candidates ranked
# Count hyphens
hyphen_count <- lengths(regmatches(names(comp_post), 
                                   gregexpr("-", names(comp_post))))

# Aggregate
result <- tapply(comp_post, hyphen_count, sum)
result <- result/sum(result)
result

comp_post <- preprocess_dfp(raw_data = dfp_raw_data, 
                            weighted = 1,
                            race = "comptroller",
                            prune = TRUE,
                            candidates = comp_cands,
                            #prior = 0.1*1024/445
                            # rescale prior after pruning.
                            prior = 0.1*978/118
                            )

comp_results <- sapply(1:100, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = comp_post)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = 1:9)
})
winners <- comp_results[1,]
wins <- sapply(1:100, FUN = function(iter){
  table(winners[iter])
})
# Compare the point estimates and confidence intervals.
win_results <- sapply(1:5, FUN = function(y) {
  results_pruned <- sapply(1:100, FUN = function(x) {
    wins[[x]][y]/500
  })
})

# The mean and CI of the win proportions repeated 100 times
sapply(1:5, FUN = function(x) {
  mean(win_results[,x], na.rm = TRUE)
})

sapply(1:5, FUN = function(x) {
  c(mean(win_results[,x], na.rm = TRUE) - 1.96*
      sqrt(var(win_results[,x], na.rm = TRUE)/100), 
    mean(win_results[,x], na.rm = TRUE) + 1.96*
      sqrt(var(win_results[,x], na.rm = TRUE)/100))
})
# This suggests Lander with 99% win prob, Johnson with 1%
# [1] 0.994260000 0.006172043
# The CIs for the 100 draws
#           [,1]        [,2] 
# [1,] 0.9935008 0.005453272 
# [2,] 0.9950192 0.006890814

# Not that it really matters, pruned head-to-head with Lander Johnson
# Same as before because the only final matchup is between them.
# [1] 0.998100000 0.003015873 

# Rounds
rounds <- comp_results[2,]

# For each of the 100 draws of 500, proportion of times Land-John in final round
final_two <- sapply(1:100, FUN = function(iter){
  round <- rounds[[iter]]
  # This gives the proportion of times Garcia is in final round.
  g <- sapply(1:500, FUN = function(r){
    ifelse(sum(which(round[2,,r] != 0)) == 3, 0 # 0 if Wiley
           , 1) # 1 if Garcia
  })
  mean(g)
})

# mean and CI for this result
c(mean(final_two) - 1.96*
    sqrt(var(final_two)/100), 
  mean(final_two) + 1.96*
    sqrt(var(final_two)/100))
# Land-John in final round every single time.
# [1] 1

###############################################################################
# See what happens with an incorrect pruning.
improper_cands <- c("Eric Adams",
                    "Kathryn Garcia",
                    "Maya Wiley",
                    "Scott Stringer")

post_bad_prune <- preprocess_dfp(raw_data = dfp_raw_data, 
                                 weighted = 1,
                                 prune = TRUE,
                                 candidates = improper_cands)
bad_prune <- sapply(1:500, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = post_bad_prune)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)
  table(rcvOutputs)
})

bad_prune_results <- sapply(1:3, FUN = function(y) {
  results_bad_pruned <- sapply(1:500, FUN = function(x) {
    bad_prune[[x]][y]/500
  })
})

sapply(1:3, FUN = function(x) {
  abs(mean(bad_prune_results[,x], na.rm = TRUE)) 
})

# sum(dfp_ballots_by_num_of_ranked_cands) = 1291.501.
# sum(unweighted) = 1552.
unweighted <- c(123, 121, 245, 210, 853)
100*unweighted/sum(unweighted)
100*dfp_ballots_by_num_of_ranked_cands/sum(dfp_ballots_by_num_of_ranked_cands)
#                          1        2        3       4         5
# Unweighted results:    123      121      245      210      853
#   By %              (7.93)   (7.80)  (15.79)  (13.53)  (54.96)
# Weighted results: 158.7877 123.5258 198.9588 169.0119 641.2163
#   By %             (12.29)   (9.56)  (15.41)  (13.09)  (49.65)

################################################################################
#################### Bootstrap for comptroller##################################
################################################################################
posterior <- preprocess_dfp(raw_data = dfp_raw_data, 
                            weighted = 1,
                            prune = FALSE)
non_pruned <- sapply(1:500, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = posterior)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)
  table(rcvOutputs[1])
})

posterior_pruned <- preprocess_dfp(raw_data = dfp_raw_data, 
                                   weighted = 1,
                                   prune = TRUE,
                                   candidates = cands)
pruned <- sapply(1:100, FUN = function(iter){
  probs <- sample_probs(probs_size = probs_size, posterior = posterior_pruned)
  rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)
  table(rcvOutputs[[1]])
})

### Maybe can cut everything after this###
# Compare the point estimates and confidence intervals.
pruned_results <- sapply(1:3, FUN = function(y) {
  results_pruned <- sapply(1:100, FUN = function(x) {
    pruned[[x]][y]/500
  })
})

#non_pruned_results <- sapply(1:3, FUN = function(y) {
#  results_non_pruned <- sapply(1:500, FUN = function(x) {
#    non_pruned[[x]][y]/500
#  })
#})

# The mean of the win proportions repeated 100 times
sapply(1:3, FUN = function(x) {
  mean(pruned_results[,x], na.rm = TRUE)
})

# The SD of the win proportions repeated over 100 times
sapply(1:3, FUN = function(x) {
  sqrt(var(pruned_results[,x], na.rm = TRUE)) 
})

# Therefore, the CI:
sapply(1:2, FUN = function(x) {
  c(mean(pruned_results[,x], na.rm = TRUE) - 1.96*
      sqrt(var(pruned_results[,x], na.rm = TRUE)/100), 
    mean(pruned_results[,x], na.rm = TRUE) + 1.96*
      sqrt(var(pruned_results[,x], na.rm = TRUE)/100))
})

# For 500 prob. samples from posterior, 
# Mean is 0.563960000 0.428300000 0.007897959
# SD is 0.022511016 0.021996097 0.003835509
# CI is 

m <- c(0.563960000, 0.428300000, 0.007897959)

sd <- c(0.022511016, 0.021996097, 0.003835509)

as.matrix(c(m - 1.96*sd/sqrt(100), m + 1.96*sd/sqrt(100)))
### and before this ###

