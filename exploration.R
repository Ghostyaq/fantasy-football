rm(list = ls())

library(nflreadr)
library(tidyverse)

raw <- load_pbp(2026)
data <- data.frame()

for (row in 1:nrow(raw)) {
    i <- raw[row, ]
    
    result <- case_when(
        !is.na(i$safety) && i$safety != 0 ~ safety(i), # SAFETY
        i$play_type == "extra_point" ~ extra_point(i), # EXTRA POINT
        i$pass_touchdown == 1 ~ pass_td(i),
        i$play_type == "pass" && i$yards_gained > 0 ~ successful_pass(i), # RECEPTION & 25 YARDS

        
    )
        data.frame(
            player_id = NA,
            player_name = NA,
            fantasy_points = NA
        )
    
    data <- rbind(data, blank)
}
