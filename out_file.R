library(magrittr)
library(ggplot2)
library(dplyr)

load("run_file_output.RData")

run_diff_by_rpi_plot <- ggplot(stats, aes(x = rpi_range, y = avg_margin)) + 
  geom_bar(stat = "identity", fill = "#ff6961") +
  theme_minimal() +
  labs(x = "RPI Difference",
       y = "Win%",
       title = "Run Differential by Difference in RPI Coefficients") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Linear Plots

linear_model_plot <- 
  ggplot(scoreboard_2022 %>% mutate(pred = estimates_linear), 
         aes(x=pred,y=score_diff)) +
  geom_point() +
  geom_smooth(method = 'lm', color = "red", formula = 'y ~ x') +
  theme_minimal() +
  labs(x = "Predicted Score Differential",
       y = "Actual Score Differential",
       title = "Linear Model Estimate vs. Actual") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

linear_model_residual_plot <- 
  ggplot(scoreboard_2022 %>% mutate(pred = estimates_linear), 
         aes(x=pred,y=(score_diff - pred))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(x = "Predicted Score Differential",
       y = "Residual",
       title = "Linear Model Estimate vs. Residual") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

linear_model_residual_hist <- 
  ggplot(scoreboard_2022 %>% mutate(pred = estimates_linear), 
         aes(x=(score_diff - pred))) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(x = "Predicted Score Differential",
       y = "Residual",
       title = "Squared Model Residual Histogram") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Squared Plots

squared_model_plot <- 
  ggplot(scoreboard_2022 %>% mutate(pred = estimates_squared), 
         aes(x=pred,y=score_diff)) +
  geom_point() +
  geom_smooth(method = 'lm', color = "red", formula = 'y ~ x') +
  theme_minimal() +
  labs(x = "Predicted Score Differential",
       y = "Actual Score Differential",
       title = "Squared Model Estimate vs. Actual") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

squared_model_residual_plot <- 
  ggplot(scoreboard_2022 %>% mutate(pred = estimates_squared), 
         aes(x=pred,y=(score_diff - pred))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(x = "Predicted Score Differential",
       y = "Residual",
       title = "Squared Model Estimate vs. Residual") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

squared_model_residual_hist <- 
  ggplot(scoreboard_2022 %>% mutate(pred = estimates_squared), 
         aes(x=(score_diff - pred))) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(x = "Predicted Score Differential",
       y = "Residual",
       title = "Squared Model Residual Histogram") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Test on 2023 Data

## ROC Curve

roc_curve <- ggplot(data.frame(FPR, TPR), aes(x = FPR, y = TPR)) +
  geom_line(color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlim(0,1) + 
  ylim(0,1) +
  theme_minimal() +
  labs(title = "Game Outcomes ROC Curve") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

## Pred vs. Actual Score Diff

model_plot_2023 <- 
  ggplot(scoreboard_2023, 
         aes(x=pred,y=score_diff)) +
  geom_point() +
  geom_smooth(method = 'lm', color = "red", formula = 'y ~ x') +
  theme_minimal() +
  labs(x = "Predicted Score Differential",
       y = "Actual Score Differential",
       title = "Squared Model Estimate vs. Actual") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

save.image(file = "out_file_output.RData")