library(magrittr)
library(dplyr)
library(tidyr)

# Read data

url <- "https://raw.githubusercontent.com/tmking2002/st491_midterm_project/main/scoreboard_dataset.RDS"

scoreboard <- readRDS(url(url)) %>% 
  mutate(score_diff = team1_runs - team2_runs) %>% 
  select(date, team1, team1_rpi, team2, team2_rpi, team1_runs, team2_runs, score_diff, season) %>% 
  arrange(date) %>% 
  drop_na(team1, team2)

scoreboard_2022 <- scoreboard %>% 
  filter(season == 2022)

scoreboard_2023 <- scoreboard %>% 
  filter(season == 2023)


# Find stats for bar plot

stats <- scoreboard_2022 %>% 
  mutate(rpi_diff = team1_rpi - team2_rpi,
         rpi_range = cut(rpi_diff, seq(-.5,.5,.1))) %>% 
  group_by(rpi_range) %>% 
  summarise(avg_margin = mean(score_diff)) %>% 
  ungroup()

# Create synthetic data

set.seed(123)

avg_rpi <- .5
sd_rpi <- sd(c(scoreboard_2022$team1_rpi, scoreboard_2022$team2_rpi))

n = 10000

synthetic_data_linear <- data.frame(team1_rpi = rnorm(n, avg_rpi, sd_rpi),
                                    team2_rpi = rnorm(n, avg_rpi, sd_rpi))

synthetic_data_linear$run_diff <- 0 + 10 * synthetic_data_linear$team1_rpi - 
  10 * synthetic_data_linear$team2_rpi

# Conduct least squares regression on synthetic data

n = 1000
p = 3
sigma = .25

beta_synthetic_linear = c(0, 10, -10)
x = cbind( 1, unlist(synthetic_data_linear[1]), unlist(synthetic_data_linear[2]))

y = as.matrix(unlist(synthetic_data_linear[3]))

beta_hat_mat_synthetic_linear  <- solve(t(x) %*% x) %*% t(x) %*% y

# Find MSE of least squares

y <- as.numeric(scoreboard_2022$score_diff)
x <- cbind(1,unlist(scoreboard_2022$team1_rpi), unlist(scoreboard_2022$team2_rpi))

beta_hat_mat_linear <- solve(t(x) %*% x) %*% t(x) %*% y

estimates_linear <- beta_hat_mat_linear[1] + 
  scoreboard_2022$team1_rpi * beta_hat_mat_linear[2] + 
  scoreboard_2022$team2_rpi * beta_hat_mat_linear[3]

rmse_linear <- 
  sqrt(mean((estimates_linear - (scoreboard_2022$team1_runs - scoreboard_2022$team2_runs)) ^ 2))

# Create synthetic data for squared data

avg_rpi_squared <- .5 ^ 2
sd_rpi_squared <- sd(c(scoreboard_2022$team1_rpi^2, scoreboard_2022$team2_rpi^2))

n = 10000

synthetic_data_squared <- 
  data.frame(team1_rpi_squared = rnorm(n, avg_rpi_squared, sd_rpi_squared),
             team2_rpi_squared = rnorm(n, avg_rpi_squared, sd_rpi_squared))

synthetic_data_squared$run_diff <- 
  0 + 20 * synthetic_data_squared$team1_rpi_squared - 
  20 * synthetic_data_squared$team2_rpi_squared

# Conduct least squares regression on synthetic data

n = 1000
p = 3
sigma = .25

beta_synthetic_squared = c(0, 20, -20)
x = cbind( 1, unlist(synthetic_data_squared[1]), unlist(synthetic_data_squared[2]))

y = as.matrix(unlist(synthetic_data_squared[3]))

beta_hat_mat_synthetic_squared <- solve(t(x) %*% x) %*% t(x) %*% y

# Find MSE of least squares

y <- as.numeric(scoreboard_2022$score_diff)
x <- cbind(1,unlist(scoreboard_2022$team1_rpi^2), unlist(scoreboard_2022$team2_rpi^2))

beta_hat_mat_squared <- solve(t(x) %*% x) %*% t(x) %*% y

estimates_squared <- beta_hat_mat_squared[1] + 
  scoreboard_2022$team1_rpi^2 * beta_hat_mat_squared[2] + 
  scoreboard_2022$team2_rpi^2 * beta_hat_mat_squared[3]

rmse_squared <- 
  sqrt(mean((estimates_squared - (scoreboard_2022$team1_runs - scoreboard_2022$team2_runs)) ^ 2))

# Test on 2023 Data

scoreboard_2023$pred <- beta_hat_mat_squared[1] + 
  scoreboard_2023$team1_rpi^2 * beta_hat_mat_squared[2] + 
  scoreboard_2023$team2_rpi^2 * beta_hat_mat_squared[3]

scoreboard_2023$pred_win <- scoreboard_2023$pred > 0
scoreboard_2023$team1_win <- 
  scoreboard_2023$team1_runs > scoreboard_2023$team2_runs

py_hat <- 
  pnorm(scoreboard_2023$pred, mean = 0, sd = sd(scoreboard_2023$score_diff))

y <- scoreboard_2023$team1_win

# How do these values change for different thresholds?
grid = seq( 0, 1, by=.001)
FPR = rep( NA, length(grid))
TPR = rep( NA, length(grid))
for(k in 1:length(grid)){
  y_hat = (py_hat > grid[k])
  FPR[k] = sum(y_hat[y == 0] == 1) / sum(y == 0)
  TPR[k] = sum(y_hat[y == 1] == 1) / sum(y == 1)
}

success_rate <- mean(scoreboard_2023$pred_win == scoreboard_2023$team1_win)

upsets <- scoreboard_2023 %>% 
  mutate(resid = abs(pred - score_diff)) %>% 
  arrange(desc(resid)) %>% 
  head(n = 5) %>% 
  select(date, team1, team2, score_diff, pred, resid)

save.image(file = "run_file_output.RData")
