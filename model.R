rm(list = ls())

library(nflverse)
library(nflreadr)
library(parallel)
library(pbapply)
library(purrr)
library(tidyverse)

source("function-calculations.R")

load("data/results_2022.rda")
load("data/results_2023.rda")
load("data/results_2024.rda")
load("data/results_2025.rda")

cl <- makeCluster(min(detectCores() - 1, 1))
clusterEvalQ(cl, {
    library(nflreadr)
    library(tidyverse)
})
clusterExport(cl, ls())

results_2026 <- mh(2026) |> list_rbind() |> na.omit()
stopCluster(cl)

results <- rbind(results_2022, results_2023, results_2024, results_2025, results_2026)
summary <- results |>
    group_by(player_id, year) |>
    summarize(points = sum(fantasy_points)) |>
    pivot_wider(
        names_from = "year",
        values_from = "points"
    ) |>
    select(c(player_id, "2022", "2023", "2024", "2025", "2026"))
players <- load_player_stats(2026)

fantasy_players <- players |>
    filter(!(position_group %in% c("DL", "DB", "LB", "OL"))) |>
    select(c("player_id", "player_display_name", "position", "team")) |>
    unique()

a <- summary |>
    filter(player_id %in% fantasy_players$player_id) |> 
    right_join(fantasy_players, by = "player_id")

b <- summary |>
    pivot_longer(
        cols = c("2022", "2023", "2024", "2025", "2026"),
        names_to = "season",
        values_to = "points"
    ) |>
    mutate(season = as.numeric(season)) |>
    drop_na()

# ---- PLAYER STATS ---- 
library(nnet)

year <- 2025

data <- map_dfr((year - 3) : year, load_player_stats) |>
    select(player_id, player_display_name, position_group, season) |>
    filter(!(position_group %in% c("DL", "DB", "LB", "OL"))) |>
    unique()

age <- load_players() |>
    filter(last_season >= year) |>
    select(gsis_id, years_of_experience, draft_pick) |>
    rename(player_id = gsis_id, experience = years_of_experience) |>
    mutate(
        experience = experience - (2026 - year)
    )

injuries <- map_dfr((year - 3) : year, load_injuries) |>
    group_by(gsis_id, season) |>
    summarise(count = n()) |>
    rename(player_id = gsis_id)

data <- data |>
    left_join(injuries, by = c("player_id", "season")) |>
    left_join(b, by = c("player_id", "season")) |>
    mutate(across(everything(), function(i) {i <- ifelse(is.na(i), 0, i)})) |>
    pivot_wider(
        names_from = "season",
        values_from = c("position_group", "count", "points"),
    ) |>
    left_join(age, by = "player_id") |>
    na.omit(position_group_2025)

names(data) <- sub(paste0("_", year, "$"), "_0", names(data))
names(data) <- sub(paste0("_", year - 1, "$"), "_1", names(data))
names(data) <- sub(paste0("_", year - 2, "$"), "_2", names(data))
names(data) <- sub(paste0("_", year - 3, "$"), "_3", names(data))

nn_model <- nnet(
    points_0 ~ (
        position_group_0 + count_3 + count_2 + count_1 + points_3 + 
        points_2 + points_1 #+ #experience #+ draft_pick
        ),
    data = data, 
    size = 10,      # hidden neurons
    linout = TRUE, # regression instead of classification
    decay = 0.01,  # weight decay to reduce overfitting
    maxit = 10000,
    trace = TRUE
)

temp <- predict(nn_model, data)
data$prediction <- temp 
