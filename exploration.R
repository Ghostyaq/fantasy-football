rm(list = ls())

library(nflreadr)
library(tidyverse)

raw <- load_pbp(2026)
data <- data.frame()

for (row in 1:nrow(raw)) {
    i <- raw[row, ]
    
    blank <- if (!is.na(i$play_type) && i$play_type == "run") {
        data.frame(
            rusher_id = i$rusher_player_id,
            rusher_name = i$rusher_player_name,
            rushing_yards = i$rushing_yards,
            fantasy_id = i$fantasy_id
        )
    } else {
        data.frame(
            rusher_id = NA,
            rusher_name = NA,
            rushing_yards = NA,
            fantasy_id = NA
        )
    }
    
    data <- rbind(data, blank)
}
