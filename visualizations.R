source("mainScript.R")
library(ggplot2)
library(tidyr)

df <- as.data.frame(t(pollWins))
df_long <- pivot_longer(df,
                        cols = everything(),
                        names_to = "variable",
                        values_to = "value")

ggplot(df_long, aes(x = factor(variable), y = value)) +
  geom_boxplot(width = 0.5) +
  labs(x = "Candidates",
       y = "Win Percentage",
       title = "Candidate Win Percentage Over Simulation") + 
  theme_minimal() +
  stat_summary(fun = function(x) quantile(x, 0.25),
               geom = "text", aes(label = round(..y.., 2)),
               vjust = -0.5, color = "blue") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun = median,
               geom = "text", aes(label = round(..y.., 2)),
               vjust = -0.5) +
  stat_summary(fun = function(x) quantile(x, 0.75),
               geom = "text", aes(label = round(..y.., 2)),
               vjust = -0.5, color = "red")

# Takes 'tab' of priors grid from dfp's Manhattan Borough President RCV sim.
cand_2 <- tab[,1]/500
cand_3 <- tab[,2]/500
cand_6 <- tab[,3]/500
plot(x = priors, y = cand_2, col = 'blue',
     xlab = 'Prior Size',
     ylab = 'Win Percent',
     main = 'Prior Size vs. Win Percent (Corrected Weights)',
     ylim = c(0,1))
model2 <- lm(cand_2 ~ priors)
abline(model2, col = 'blue')
points(x = priors, y = cand_3, col = 'green')
model3 <- lm(cand_3 ~ priors)
abline(model3, col = 'green')
points(x = priors, y = cand_6, col = 'red')
model6 <- lm(cand_6 ~ priors)
abline(model6, col = 'red')
legend('center', 
       legend = c('Levine', 'Hoylman', 'Kallos'),
       col = c('blue', 'red', 'green'),
       lty = 1)

# Plot the number of unique orderings (%) vs. percent of unchanged ballots
# as we prune number of candidates.



# Plot the proportions from the grid of polls on the CVR over sample size.
x <- seq(100, 1500, by = 100)
cols = c("#7FD070",
         "#707ED0",
         "#D0707E")

# Add CIs around plot
alpha_col <- adjustcolor(cols, alpha.f = 0.2)

# Plot avg. win proportions.
matplot(
  x,
  t(means[1:3,]),   # transpose so columns = lines
  type = "l",
  lty = 1,
  lwd = 2,
  col = cols,
  ylim = c(0,0.8),
  xlab = "Sample Size",
  ylab = "Mean Win Probs.",
  #main = "Mean Win Probabilities of Simulated Polls Across Sample Sizes"
)

for (k in 1:3) {
  upper <- means[k, ] + 1.96 * se[k, ]
  lower <- means[k, ] - 1.96 * se[k, ]
  
  polygon(
    c(x, rev(x)),
    c(upper, rev(lower)),
    col = alpha_col[k],
    border = NA
  )
}

for (k in 1:3) {
  lines(x, means[k, ], col = cols[k], lwd = 2)
}

legend(
  "topleft",
  legend = c("Adams", "Garcia", "Wiley"),
  col = cols,
  lty = 1,
  lwd = 2,
  bty = "n"
)


# Plot sds
matplot(
  x,
  t(sds[1:3,]),   # transpose so columns = lines
  type = "l",
  lty = 1,
  lwd = 2,
  col = cols,
  ylim = c(0,0.4),
  xlab = "Sample Size",
  ylab = "Standard Deviation of Win Probs.",
  #main = "Mean Win Probabilities of Simulated Polls Across Sample Sizes"
)

legend(
  "topleft",
  legend = c("Adams", "Garcia", "Wiley"),
  col = cols,
  lty = 1,
  lwd = 2,
  bty = "n"
)


# Paired plot:
CVR <- c(13.5, 11.8, 16.4, 12.3, 46.0)
DFP <- c(12.3, 9.6, 15.4, 13.1, 49.7)

mat <- rbind(CVR, DFP)

bp <- barplot(
  mat,
  beside = TRUE,
  col = c("#707ED0", "#D0707E"),
  names.arg = c(seq(1:5)),
  ylim = c(0, max(mat) * 1.2),
  legend.text = c("CVR", "DFP Poll"),
  args.legend = list(x = "topleft", bty = "n")
)
text(
  x = bp,
  y = mat,
  labels = mat,
  pos = 3,      # above bars
  cex = 0.8
)

# Pruning plot
data <- data.frame(
  Prune = c("Top 3 Cands.", "Top 4 Cands.", "Top 5 Cands."),
  `100`  = c(0.926, 0.904, 0.594),
  `200`  = c(0.986, 0.974, 0.806),
  `300`  = c(1.000, 0.990, 0.912),
  `400`  = c(1.000, 1.000, 0.932),
  `500`  = c(1.000, 1.000, 0.952),
  `600`  = c(1.000, 1.000, 0.992),
  `700`  = c(1.000, 1.000, 0.984),
  `800`  = c(1.000, 1.000, 0.998),
  `900`  = c(1.000, 1.000, 0.992),
  `1000` = c(1.000, 1.000, 0.994),
  check.names = FALSE
)

# Pivot to long format
data_long <- data %>%
  pivot_longer(cols = -Prune, names_to = "SampleSize", values_to = "Value") %>%
  mutate(SampleSize = as.integer(SampleSize))

# Plot
ggplot(data_long, aes(x = SampleSize, y = Value, color = Prune, group = Prune)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Top 3 Cands." = "#7FD070",
                                "Top 4 Cands." = "#707ED0",
                                "Top 5 Cands." = "#D0707E")) +
  scale_x_continuous(breaks = seq(100, 1000, by = 100)) +
  scale_y_continuous(limits = c(0.5, 1.01),
                     breaks = seq(0.5, 1.0, by = 0.1)) +
  geom_hline(yintercept = .99, linetype = "dotted", color = "black", size = 1) +
  labs(
    #title = "Pruning Accuracy by Sample Size Across Simulated Polls",
    x = "Sample Size",
    y = "Accuracy",
    color = "Pruning Level"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid = element_blank()
  )
