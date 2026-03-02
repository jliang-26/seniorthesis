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
     main = 'Prior Size vs. Win Percent',
     ylim = c(0,.6))
model2 <- lm(cand_2 ~ priors)
abline(model2, col = 'blue')
points(x = priors, y = cand_3, col = 'red')
model3 <- lm(cand_3 ~ priors)
abline(model3, col = 'red')
points(x = priors, y = cand_6, col = 'green')
model6 <- lm(cand_6 ~ priors)
abline(model, col = 'green')
legend('center', 
       legend = c('Levine', 'Hoylman', 'Kallos'),
       col = c('blue', 'red', 'green'),
       lty = 1)
