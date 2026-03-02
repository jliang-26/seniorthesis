source("constants.R")
source("helperFunctions.R")

# To do: named list of all 500 runs, transpose rows sand columns.

# This script preprocesses the dfp final NYC 2021 poll. 
raw_data <- read.csv("Datasets/dfp_nyc_pre_election_2021_final_v2_e5_0020040_micro.csv")

# This gives the posterior dirichlet distribution for all unique ballots.
# Optionally, specify a prior. preprocess_dfp(raw_data = raw_data, weighted = 1, prior = prior)
posterior <- preprocess_dfp(raw_data, weighted = 1)

# Probs is a 500 x 807 matrix. Each row corresponds to one sample of 
# probabilities from the Dirichlet posterior given the 807 unique rankings
probs <- sample_probs(probs_size = probs_size, posterior = posterior)

rcvOutputs <- run_rcv(sample_probs = probs, candidates = candidates)

# Sort by ascending weight.
# sorted_weighted_rankings <- culled_weighted_sum_rankings %>%
#   arrange(desc(total))


### Manhattan Borough President Poll ###

# This returns the total weight for each unique ranking that appears in the poll
# sorted_mbp_rankings[1] gives the weight table
# sorted_mbp_rankings[2] gives the name-to-integer mapping for reference.
sorted_mbp_rankings <- preprocess_mbp_dfp(raw_data)

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

missing_rankings <- setdiff(combined_rankings, sorted_mbp_rankings[[1]]$combined_rankings)

missing_weights <- data.frame(
  combined_rankings = missing_rankings,
  total = 0.5
)
# Complete_rankings gives a complete tibble with 156 unique "combined_rankings"
# and their weights, 0.5 assigned to orderings that don't appear in the poll.
complete_rankings <- rbind(sorted_mbp_rankings[[1]], missing_weights)

# Play around with different values of the Dirichlet prior.
priors <- seq(from = 0.001, to = 1, length.out = 50)

dfp_mbp_posterior <- complete_rankings$total
names(dfp_mbp_posterior) <- complete_rankings$combined_rankings

winners_grid <- sapply(priors, FUN = function(prior) {
  posterior <- dfp_mbp_posterior + prior
  
  dfp_mbp_probs <- sample_probs(probs_size = probs_size, 
                                posterior = posterior)
  
  rcvOutputs <- run_rcv(sample_probs = dfp_mbp_probs, candidates = 1:6)
  rcvOutputs[[1]]
})
# Go to visualizations to create a plot of this.
tab <- table(col(winners_grid), winners_grid)


### Real Ballots  ###
primary_ballots <- read.csv("Datasets/nyc_mayor_dem_primary_ballots_ranked.csv")

# 75,153 Unique Ballots Were Cast
sum(!duplicated(primary_ballots[,2:6]))

# The top 10 most frequent ballots are below as "top_ten".
# "most_common" gives complete frequency table.
tab <- as.data.frame(table(data.frame(primary_ballots[,2:6])))
most_common <- tab[order(-tab$Freq), ]
top_ten <- head(most_common, n = 10)

# This gives frequency for ballots that ranked diff. numbers of candidates
# Still a work in progress.
grid_ranked <- sapply(2:6, FUN = function(x) {
  if (x == 6) {
    ranked <- primary_ballots[primary_ballots[,6] != "", 2:6]
    tab <- as.data.frame(table(data.frame(ranked[,1:5])))
    most_common <- tab[order(-tab$Freq), ]
    most_common
  } else {
    ranked <- primary_ballots[primary_ballots[,x:6] != "", 2:6]
    tab <- as.data.frame(table(data.frame(ranked[,1:5])))
    most_common <- tab[order(-tab$Freq), ]
    most_common
  }
})
top_ten <- head(most_common, n = 10)


# This gives frequency for ballots that ranked at least 4 candidates.
ranked_4 <- primary_ballots[primary_ballots[,5] != "", 2:6]
tab_4 <- as.data.frame(table(data.frame(ranked_4[,1:5])))
most_common_4 <- tab_4[order(-tab_4$Freq), ]
top_ten_4 <- head(most_common_4, n = 10)

# 
top_4 <- c("Eric L. Adams",
           "Kathryn A. Garcia",
           "Maya D. Wiley",
           "Andrew Yang")
sapply()