rm(list = ls())

library(nflreadr)
library(tidyverse)

source("function-calculations.R")

raw <- load_pbp(2026)
data <- data.frame()

for (row in 1:nrow(raw)) {
    i <- raw[row, ]
    
    mixed <- if (!is.na(i$safety) & (i$safety != 0)) {
        safety(i)
    } else if (!is.na(i$extra_point_result) & (i$extra_point_result == "good")) {
        extra_point(i)
    } else if (!is.na(i$pass_touchdown) & (i$pass_touchdown == 1)) {
        pass_td(i)
    } else if (!is.na(i$complete_pass) & (i$complete_pass == 1)) {
        successful_pass(i)
    } else if (!is.na(i$interception) & (i$interception == 1)) {
        interception_off(i)
    } else if (!is.na(i$run_touchdown) & (i$run_touchdown == 1)) {
        run_td(i)
    } else if (!is.na(i$play_type) & !is.na(i$yards_gained) & (i$play_type == "run") & (i$yards_gained > 0)) {
        successful_run(i)
    } else if (!is.na(i$two_point_attempt) & (i$return_touchdown == 1)) {
        return_td(i)
    } else if (!is.na(i$two_point_attempt) & (i$two_point_attempt == 1)) {
        two_points(i)
    } else if (!is.na(i$fumble) & (i$fumble == 1) & (i$fumble_lost == 0)) {
        fumble(i)
    } else {
        NULL
    }
        
    defense <- if (!is.na(i$sack) & (i$sack == 1)) {
        sack(i)
    } else if (!is.na(i$interception) & (i$interception == 1)) {
        interception_def(i)
    } else if (!is.na(i$desc) & grepl("Punt blocked", i$desc)) {
        punt_block(i)
    } else if (!is.na(i$field_goal_attempt) & (field_goal_attempt == "blocked")) {
        fg_block(i)
    } else {
        NULL
    }
    
    game <- if(i$desc == "END GAME") ~ endgame_calcs(i),
        TRUE ~ NULL
    )
    
    combined <- rbind(mixed, defense, game)
    
    if (is.null(combined)) {
        combined <- data.frame(
            player_id = NA,
            player_name = NA,
            fantasy_points = NA,
            play_type = NA
        )
    }
    
    data <- rbind(data, combined)
}