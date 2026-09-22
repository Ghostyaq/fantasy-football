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

cl <- makeCluster(detectCores() - 1)
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

