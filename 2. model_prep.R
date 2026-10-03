rm(list = ls())

# ----- LOADING LIBRARIES & FUNCTIONS -----
library(glmnet)
library(randomForest)
library(e1071)
library(pls)
library(scales)
library(nnet)
library(minpack.lm)
library(MASS)

library(nflverse)
library(nflreadr)
library(parallel)
library(pbapply)
library(purrr)
library(tidyverse)

source("1. function-calculations.R")

load("data/results_2021.rda")
load("data/results_2022.rda")
load("data/results_2023.rda")
load("data/results_2024.rda")
load("data/results_2025.rda")

# ----- INITIALIZING CLUSTERS -----

cl <- makeCluster(min(detectCores() - 1, 1))
clusterEvalQ(cl, {
    library(nflreadr)
    library(tidyverse)
})
clusterExport(cl, ls())

results_2026 <- mh(2026) |> list_rbind() |> na.omit()
stopCluster(cl)

results <- rbind(results_2021, results_2022, results_2023, results_2024, results_2025, results_2026)
rm(list = c("results_2021", "results_2022", "results_2023", "results_2024", "results_2025", "results_2026"))

# ----- FANTASY POINTS SUMMARY -----

summary <- results |>
    group_by(player_id, year) |>
    summarize(points = sum(fantasy_points)) |>
    pivot_wider(
        names_from = "year",
        values_from = "points"
    ) |>
    dplyr::select(player_id, `2021`, `2022`, `2023`, `2024`, `2025`, `2026`)

longer_summary <- summary |>
    pivot_longer(
        cols = c("2021", "2022", "2023", "2024", "2025", "2026"),
        names_to = "season",
        values_to = "points"
    ) |>
    mutate(season = as.numeric(season)) |>
    drop_na()

# ---- COMBINED PLAYER STATS (AGE, POSITION, INJURIES, FANTASY POINTS) ---- 
train_year <- 2024

data <- map_dfr(2021:2026, load_player_stats) |>
    dplyr::select(player_id, player_display_name, position_group, season) |>
    filter(!(position_group %in% c("DL", "DB", "LB", "OL"))) |>
    unique()

age <- load_players() |>
    filter(last_season >= train_year) |>
    dplyr::select(gsis_id, years_of_experience, draft_pick) |>
    rename(player_id = gsis_id, experience = years_of_experience) |>
    mutate(experience = experience - (2026 - train_year))

injuries <- map_dfr(2021:2026, load_injuries) |>
    group_by(gsis_id, season) |>
    summarise(count = n()) |>
    rename(player_id = gsis_id)

data <- data |>
    left_join(injuries, by = c("player_id", "season")) |>
    left_join(longer_summary, by = c("player_id", "season")) |>
    mutate(across(everything(), function(i) {i <- ifelse(is.na(i), 0, i)})) |>
    pivot_wider(
        names_from = "season",
        values_from = c("position_group", "count", "points"),
    ) |>
    left_join(age, by = "player_id")