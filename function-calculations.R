library(tidyverse)

safety <- function(row) {
    data.frame(
        player_id = row$safety_player_id,
        player_name = row$safety_player_name,
        fantasy_points = 4,
        play_type = "safety",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

extra_point <- function(row) {
    data.frame(
        player_id = row$kicker_player_id,
        player_name = row$kicker_player_name,
        fantasy_points = 1,
        play_type = "extra point",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

pass_td <- function(row) {
    qb <- data.frame(
        player_id = row$passer_player_id,
        player_name = row$passer_player_name,
        fantasy_points = 4 + row$yards_gained / 25,
        play_type = "passed TD (QB)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
    
    receiver <- data.frame(
        player_id = row$receiver_player_id,
        player_name = row$receiver_player_name,
        fantasy_points = 6 + row$yards_gained / 10 + 1,
        play_type = "passed TD (receiver)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
    
    rbind(qb, receiver)
}

successful_pass <- function(row) {
    qb <- data.frame(
        player_id = row$passer_player_id,
        player_name = row$passer_player_name,
        fantasy_points = row$yards_gained / 25,
        play_type = "pass (QB)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
    
    receiver <- data.frame(
        player_id = row$receiver_player_id,
        player_name = row$receiver_player_name,
        fantasy_points = 1 + row$yards_gained / 10,
        play_type = "pass (receiver)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
    
    rbind(qb, receiver)
}

interception_off <- function(row) {
    data.frame(
        player_id = row$passer_player_id,
        player_name = row$passer_player_name,
        fantasy_points = -2,
        play_type = "interception (QB)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

run_td <- function(row) {
    data.frame(
        player_id = row$rusher_player_id,
        player_name = row$rusher_player_name,
        fantasy_points = 6 + row$yards_gained / 10,
        play_type = "rushed TD",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

successful_run <- function(row) {
    data.frame(
        player_id = row$rusher_player_id,
        player_name = row$rusher_player_name,
        fantasy_points = row$yards_gained / 10,
        play_type = "rushed (receiver)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

return_td <- function(row) {
    data.frame(
        player_id = 'team',
        player_name = row$defteam,
        fantasy_points = 6,
        play_type = "return TD",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

two_points <- function(row) {
    return_df <- if (!is.na(row$two_point_conv_result) & row$two_point_conv_result == "failure") {
        data.frame(
            player_id = NA, 
            player_name = NA, 
            fantasy_points = 0, 
            play_type = "failed 2pts",
            play_id = paste0(row$game_id, "_", row$play_id)
            )
    } else if (!is.na(row$receiver_player_id)) {
        qb <- data.frame(
            player_id = row$passer_player_id,
            player_name = row$passer_player_name,
            fantasy_points = 2,
            play_type = "pass 2pts (QB)",
            play_id = paste0(row$game_id, "_", row$play_id)
        )
        
        receiver <- data.frame(
            player_id = row$receiver_player_id,
            player_name = row$receiver_player_name,
            fantasy_points = 2,
            play_type = "pass 2pts (receiver)",
            play_id = paste0(row$game_id, "_", row$play_id)
        )
        
        rbind(qb, receiver)
    } else if (!is.na(row$rusher_player_id)) {
        data.frame(
            player_id = row$rusher_player_id,
            player_name = row$rusher_player_name,
            fantasy_points = 2,
            play_type = "pass 2pts (rusher)",
            play_id = paste0(row$game_id, "_", row$play_id)
        )
    } else {
        data.frame(
            player_id = 'nani',
            player_name = NA,
            fantasy_points = 0,
            play_type = "wtf 2pts",
            play_id = paste0(row$game_id, "_", row$play_id)
        )
    }
    
    return_df
}

fumble <- function(row) {
    data.frame(
        player_id = row$fumbled_1_player_id,
        player_name = row$fumbled_1_player_name,
        fantasy_points = -2,
        play_type = "person who fumbled",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

sack <- function(row) {
    data.frame(
        player_id = 'team',
        player_name = row$defteam,
        fantasy_points = 1,
        play_type = "sack",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

interception_def <- function(row) {
    data.frame(
        player_id = 'team',
        player_name = row$defteam,
        fantasy_points = 2,
        play_type = "interception (defense)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

punt_block <- function(row) {
    data.frame(
        player_id = 'team',
        player_name = row$defteam,
        fantasy_points = 2,
        play_type = "punt block",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

fg_block <- function(row) {
    data.frame(
        player_id = 'team',
        player_name = row$defteam,
        fantasy_points = 2,
        play_type = "fg block",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
}

endgame_calcs <- function(row) {
    temp <- function(def) {
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
    
    home <- data.frame(
        player_id = 'home',
        player_name = row$home_team,
        fantasy_points = temp(row$away_score),
        play_type = "endgame (home defense)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
    
    away <- data.frame(
        player_id = 'away',
        player_name = row$away_team,
        fantasy_points = temp(row$home_score),
        play_type = "endgame (away defense)",
        play_id = paste0(row$game_id, "_", row$play_id)
    )
    
    rbind(home, away)
}

mh <- function(year) {
    raw <- load_pbp(year)
    data <- pblapply(split(raw, seq_len(nrow(raw))), function(i) {
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
        } else if (!is.na(i$rush_touchdown) & (i$rush_touchdown == 1)) {
            run_td(i)
        } else if (!is.na(i$play_type) & !is.na(i$yards_gained) & (i$play_type == "run") & (i$yards_gained > 0)) {
            successful_run(i)
        } else if (!is.na(i$two_point_attempt) & (i$two_point_attempt == 1)) {
            two_points(i)
        } else {
            NULL
        }
        
        fumbles <- if (!is.na(i$fumble)&(i$fumble==1)&(i$fumble_lost==1)) {
            fumble(i) 
        } else {
            NULL
        }
        
        returns <- if (!is.na(i$return_touchdown) & (i$return_touchdown == 1)) {
            return_td(i)
        } else {
            NULL
        }
        
        defense <- if (!is.na(i$sack) & (i$sack == 1)) {
            sack(i)
        } else if (!is.na(i$interception) & (i$interception == 1)) {
            interception_def(i)
        } else if (!is.na(i$punt_blocked) & (i$punt_blocked == 1)) {
            punt_block(i)
        } else if (!is.na(i$field_goal_result) & (i$field_goal_result == "blocked")) {
            fg_block(i)
        } else {
            NULL
        }
        
        game <- if (!is.na(i$desc) & (i$desc == "END GAME")) {
            endgame_calcs(i)
        } else {
            NULL
        }
        
        combined <- rbind(mixed, fumbles, returns, defense, game)
        
        if (is.null(combined)) {
            combined <- data.frame(
                player_id = NA,
                player_name = NA,
                fantasy_points = NA,
                play_type = NA,
                play_id = NA 
            )
        }
        
        combined$year <- rep(year, nrow(combined))
        return(combined)
    }, cl = cl)
}