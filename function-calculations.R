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
        player_id = row$passer_player_id,
        player_name = row$passer_player_name,
        fantasy_points = 1 + row$yards_gained / 25,
        play_type = "pass (QB)"
    )
    
    receiver <- data.frame(
        player_id = row$receiver_player_id,
        player_name = row$receiver_player_name,
        fantasy_points = 1 + row$yards_gained / 10,
        play_type = "passed TD (receiver)"
    )
    
    rbind(qb, receiver)
}

interception_off <- function(row) {
    data.frame(
        player_id = row$passer_player_id,
        player_name = row$passer_player_name,
        fantasy_points = -2,
        play_type = "interception (QB)"
    )
}

run_td <- function(row) {
    data.frame(
        player_id = row$rusher_player_id,
        player_name = row$rusher_player_name,
        fantasy_points = 6,
        play_type = "rushed TD"
    )
}

successful_run <- function(row) {
    data.frame(
        player_id = row$rusher_player_id,
        player_name = row$rusher_player_name,
        fantasy_points = row$yards_gained / 10,
        play_type = "rushed (receiver)"
    )
}

return_td <- function(row) {
    data.frame(
        player_id = 0,
        player_name = row$defteam,
        fantasy_points = 6,
        play_type = "return TD"
    )
}

two_points <- function(row) {
    if (row$two_point_conv_result == "failure") {
        return(
            data.frame(
                player_id = NA, 
                player_name = NA, 
                fantasy_points = 0, 
                play_type = "failed 2pts"
                )
        )
    }
    
    if (!is.na(row$receiver_played_id)) {
        qb <- data.frame(
            player_id = row$passer_player_id,
            player_name = row$passer_player_name,
            fantasy_points = 2,
            play_type = "pass 2pts (QB)"
        )
        
        receiver <- data.frame(
            player_id = row$receiver_player_id,
            player_name = row$receiver_player_name,
            fantasy_points = 2,
            play_type = "pass 2pts (receiver)"
        )
        
        return(rbind(qb, receiver))
    }
    
    if (!is.na(row$rusher_player_id)) {
        return(
            data.frame(
                player_id = row$rusher_player_id,
                player_name = row$rusher_player_name,
                fantasy_points = 2,
                play_type = "pass 2pts (rusher)"
            )
        )
    }
    
    data.frame(
        player_id = 1,
        player_name = NA,
        fantasy_points = 0,
        play_type = "wtf 2pts"
    )
}

fumble <- function(row) {
    data.frame(
        player_id = row$fumbled_1_player_id,
        player_name = row$fumbled_1_player_name,
        fantasy_points = -2,
        play_type = "person who fumbled"
    )
}

sack <- function(row) {
    data.frame(
        player_id = 0,
        player_name = row$defteam,
        fantasy_points = 1,
        play_type = "sack"
    )
}

interception_def <- function(row) {
    data.frame(
        player_id = 0,
        player_name = row$defteam,
        fantasy_points = 2,
        play_type = "interception (defense)"
    )
}

punt_block <- function(row) {
    data.frame(
        player_id = 0,
        player_name = row$defteam,
        fantasy_points = 2,
        play_type = "punt block"
    )
}

fg_block <- function(row) {
    data.frame(
        player_id = 0,
        player_name = row$defteam,
        fantasy_points = 2,
        play_type = "fg block"
    )
}

endgame_calcs <- function(row) {
    data.frame(
        player_id = row$def_team,
        player_name = row$def_team
    )
    
    temp <- function(off, def) {
        case_when(
            def == 0 ~ 10,
            def < 7 ~ 7,
            def < 14 ~ 4,
            def < 21 ~ 1,
            def < 28 ~ 0,
            def < 35 ~ -1,
            def >= 35 ~ -4
        )
    }
}