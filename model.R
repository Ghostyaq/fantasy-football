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
        cols = c("2022", "2023", "2024", "2025"),
        names_to = "year",
        values_to = "points"
    ) |>
    mutate(year = as.numeric(year)) |>
    drop_na()

cor(b$year, b$points)


# ---- PLAYER STATS ---- 
library(nnet)

data <- map_dfr(2022:2025, load_player_stats) |>
    select(player_id, player_display_name, position_group, season) |>
    filter(!(position_group %in% c("DL", "DB", "LB", "OL"))) |>
    unique()

injuries <- map_dfr(2022:2025, load_injuries) |>
    group_by(gsis_id, season) |>
    summarise(count = n()) |>
    rename(player_id = gsis_id)

data <- data |>
    left_join(injuries, by = c("player_id", "season")) |>
    pivot_wider(
        names_from = season,
        values_from = c("position_group", "count")
    ) |>
    left_join(summary, by = "player_id") |>
    drop_na(`2025`) |>
    mutate(across(everything(), function(i) {i <- ifelse(is.na(i), 0, i)}))

nn_model <- nnet(
    `2025` ~ (
        position_group_2022 + position_group_2023 + position_group_2024 + 
        position_group_2025 + count_2022 + count_2023 + count_2024 + `2022` + 
        `2023` + `2024`
        ),
    data = data, 
    size = 6,      # hidden neurons
    linout = TRUE, # regression instead of classification
    decay = 0.01,  # weight decay to reduce overfitting
    maxit = 5000,
    trace = TRUE
)

temp <- predict(nn_model, data)
data$prediction <- temp 
