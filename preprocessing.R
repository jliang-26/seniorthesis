source("constants.R")
source("helperFunctions.R")

# To do: named list of all 500 runs, transpose rows sand columns.

# This script preprocesses the dfp final NYC 2021 poll. 
dfp_raw_data <- read.csv("Datasets/dfp_nyc_pre_election_2021_final_v2_e5_0020040_micro.csv")

# This gives the posterior dirichlet distribution for all unique ballots.
# Optionally, specify a prior. preprocess_dfp(raw_data = dfp_raw_data, weighted = 1, prior = prior)
posterior <- preprocess_dfp(raw_data, weighted = 1)

# Probs is a 500 x 807 matrix. Each row corresponds to one sample of 
# probabilities from the Dirichlet posterior given the 807 unique rankings
probs <- sample_probs(probs_size = probs_size, posterior = posterior)

rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)

# Sort by ascending weight.
# sorted_weighted_rankings <- culled_weighted_sum_rankings %>%
#   arrange(desc(total))

ranking_cols <- c("weight",
                  "rank_preference_sure_1",
                  "rank_preference_sure_2",
                  "rank_preference_sure_3",
                  "rank_preference_sure_4",
                  "rank_preference_sure_5")

dfp_rankings <- dfp_raw_data[, ranking_cols]

dfp_ballots_by_num_of_ranked_cands <- sapply(1:5, FUN = function(x) {
  results <- apply(dfp_rankings, 1, FUN = function(y) {
    weight <- y[1]
    y <- y[2:6]
    y <- y[y != ""]
    if(length(unique(y)) == x) {
      weight
    } else {
      0
    }
  })
  sum(results)
})

dfp_ballots_by_num_of_ranked_cands <- sapply(1:5, FUN = function(x) {
  results <- as.numeric(apply(dfp_rankings, 1, FUN = function(y) {
    weight <- y[1]
    y <- y[2:6]
    y <- y[y != ""]
    
    # length(unique(y)) == x
    if(length(unique(y)) == x) {
      weight
    } else {
      0
    }
  }))
  
  sum(results)
  # sum(results == TRUE)
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



############################################
##    Manhattan Borough President Poll    ##
############################################

# This returns the total weight for each unique ranking that appears in the poll
# dfp_mbp_poll_numeric[1] gives the raw poll before weights
# dfp_mbp_poll_numeric[2] gives the name-to-integer mapping for reference.
# sorted_mbp_rankings gives the weight table
# There are 451 unique poll responses (rows)
# The sum of weights is 450.5042
dfp_mbp_poll_numeric <- preprocess_mbp_dfp(dfp_raw_data, weighted = 1)
sorted_mbp_rankings <- aggregate_mbp_rankings(dfp_mbp_poll_numeric[[1]])


# For reference, the number of ballots that ranked 1, 2, 3 candidates:
rankCount <- lengths(regmatches(dfp_mbp_poll_numeric[[1]]$combined_rankings, 
                                gregexpr("-", dfp_mbp_poll_numeric[[1]]$combined_rankings)))
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