library(magrittr)
library(ggplot2)

load("run_file_output.RData")

run_diff_by_rpi_plot <- ggplot(stats, aes(x = rpi_range, y = avg_margin)) + 
  geom_bar(stat = "identity", fill = "#ff6961") +
  theme_minimal() +
  labs(x = "RPI Difference",
       y = "Win%",
       title = "Run Differential by Difference in RPI Coefficients") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

save.image(file = "out_file_output.RData")