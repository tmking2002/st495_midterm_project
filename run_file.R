library(magrittr)
library(dplyr)

# Read data

url <- "https://raw.githubusercontent.com/tmking2002/st491_midterm_project/main/scoreboard_dataset.RDS"

scoreboard <- readRDS(url(url)) %>% 
  mutate(score_diff = team1_runs - team2_runs) %>% 
  select(date, team1, team1_rpi, team2, team2_rpi, team1_runs, team2_runs, score_diff) %>% 
  arrange(date)

# Find stats for bar plot

stats <- scoreboard %>% 
  mutate(rpi_diff = team1_rpi - team2_rpi,
         rpi_range = cut(rpi_diff, seq(-.5,.5,.1))) %>% 
  group_by(rpi_range) %>% 
  summarise(avg_margin = mean(score_diff)) %>% 
  ungroup()

# Create synthetic data

set.seed(123)

avg_rpi <- .5
sd_rpi <- sd(c(scoreboard$team1_rpi, scoreboard$team2_rpi))

n = 10000

synthetic_data <- data.frame(team1_rpi = rnorm(n, avg_rpi, sd_rpi),
                             team2_rpi = rnorm(n, avg_rpi, sd_rpi))

synthetic_data$run_diff <- 0 + 10 * synthetic_data$team1_rpi - 
  10 * synthetic_data$team2_rpi

# Conduct least squares regression on synthetic data

n = 1000
p = 3
sigma = .25

beta_synthetic = c(0, 10, -10)
x = cbind( 1, unlist(synthetic_data[1]), unlist(synthetic_data[2]))

y = as.matrix(unlist(synthetic_data[3]))

beta_hat_mat_synthetic <- solve(t(x) %*% x) %*% t(x) %*% y

# Find MSE of least squares

y <- as.numeric(scoreboard$team1_runs - scoreboard$team2_runs)
x <- cbind(1,unlist(scoreboard$team1_rpi), unlist(scoreboard$team2_rpi))

beta_hat_mat <- solve(t(x) %*% x) %*% t(x) %*% y

estimates <- beta_hat_mat[1] + scoreboard$team1_rpi * beta_hat_mat[2] + 
  scoreboard$team2_rpi * beta_hat_mat[3]

mse <- mean((estimates - (scoreboard$team1_runs - scoreboard$team2_runs)) ^ 2)

save.image(file = "run_file_output.RData")
