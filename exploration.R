rm(list = ls())

library(nflreadr)
library(tidyverse)

raw <- load_pbp(2026)
data <- data.frame()

for (row in 1:nrow(raw)) {
    i <- raw[row, ]
    
    mixed <- case_when(
        !is.na(i$safety) && i$safety != 0 ~ safety(i), # SAFETY
        i$extra_point_result == "good" ~ extra_point(i), # EXTRA POINT
        i$pass_touchdown == 1 ~ pass_td(i), # QB 4 points RECIEVER 6 points
        i$complete_pass == 1 ~ successful_pass(i), # RECEPTION & 25 YARDS THROWN & 10 YARD RECEPTION
        i$interception == 1 ~ interception_off(i), # INTERCEPTION
        i$play_type == "run" && i$yards_gained > 0 ~ successful_run(i), # RUN & TD
        i$return_touchdown == 1 ~ return_td(i), # RETURN TOUCHDOWN WTF IS THIS
        i$two_point_attempt == 1 ~ two_points(i), #  success / failure / safety
        i$fumble == 1 && i$fumble_lost == 0 ~ fumble(i), # FUMBLE
        .default = NULL
    )
    
    defense <- case_when(
        i$sack == 1 ~ sack(i), #SACK
        i$interception == 1 ~  interception_def(i), # DEFENSIVE INTERCEPTION
        grepl("Punt blocked", i$desc) ~ punt_block(i), # PUNT BLOCK
        field_goal_attempt == "blocked" ~ fg_block(i), # FIELD GOAL BLOCK
        .default = NULL
    )
    
    game <- case_when(
        i$desc == "END GAME" ~ endgame_calcs(i),
        .default = NULL
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