source("get_current_rpi.R")

# Read in all 
scoreboard_2022 <- readRDS("ncaa_scoreboard_2022.RDS")
scoreboard_2023 <- readRDS("ncaa_scoreboard_2023.RDS")

rpi_2022 <- get_current_rpi(scoreboard_2022) %>% 
  select(team_name, rpi_coef)

rpi_2023 <- get_current_rpi(scoreboard_2023) %>% 
  select(team_name, rpi_coef)

scoreboard_2022_final <- scoreboard_2022 %>% 
  filter(team1_runs != team2_runs) %>% 
  mutate(team1_win = team1_runs > team2_runs) %>% 
  merge(rpi_2022, by.x = "team1",by.y = "team_name") %>% 
  rename(team1_rpi = rpi_coef) %>% 
  merge(rpi_2022, by.x = "team2",by.y = "team_name") %>% 
  rename(team2_rpi = rpi_coef) %>% 
  filter(!str_detect(team1,"#") & !str_detect(team2,"#")) %>% 
  mutate(season = 2022)

scoreboard_2023_final <- scoreboard_2023 %>% 
  filter(team1_runs != team2_runs) %>% 
  mutate(team1_win = team1_runs > team2_runs) %>% 
  merge(rpi_2023, by.x = "team1",by.y = "team_name") %>% 
  rename(team1_rpi = rpi_coef) %>% 
  merge(rpi_2023, by.x = "team2",by.y = "team_name") %>% 
  rename(team2_rpi = rpi_coef) %>% 
  mutate(season = 2023)

final_scoreboard <- rbind(scoreboard_2022_final, scoreboard_2023_final)

saveRDS(final_scoreboard, file = "scoreboard_dataset.RDS")
