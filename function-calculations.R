library(tidyverse)

safety <- function(row) {
    data.frame(
        player_id = row$safety_player_id,
        player_name = row$safety_player_name,
        fantasy_points = 4,
        play_type = "safety"
    )
}

extra_point <- function(row) {
    data.frame(
        player_id = row$kicker_player_id,
        player_name = row$kicker_player_name,
        fantasy_points = 1,
        play_type = "extra point"
    )
}

pass_td <- function(row) {
    qb <- data.frame(
        player_id = row$passer_player_id,
        player_name = row$passer_player_name,
        fantasy_points = 4,
        play_type = "passed TD (QB)"
    )
    
    receiver <- data.frame(
        player_id = row$receiver_player_id,
        player_name = row$receiver_player_name,
        fantasy_points = 6,
        play_type = "passed TD (receiver)"
    )
    
    rbind(qb, receiever)
}

successful_pass <- function(row) {
    qb <- data.frame(
        player_id = row$passer_id,
    )
}