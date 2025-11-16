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

