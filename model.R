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
    dplyr::select(player_id, `2022`, `2023`, `2024`, `2025`, `2026`)
players <- load_player_stats(2026)

fantasy_players <- players |>
    filter(!(position_group %in% c("DL", "DB", "LB", "OL"))) |>
    dplyr::select(c("player_id", "player_display_name", "position", "team")) |>
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
library(glmnet)
library(randomForest)
library(e1071)
library(pls)
library(scales)
library(nnet)
library(minpack.lm)
library(MASS)

train_year <- 2025

data <- map_dfr(2022:2026, load_player_stats) |>
    dplyr::select(player_id, player_display_name, position_group, season) |>
    filter(!(position_group %in% c("DL", "DB", "LB", "OL"))) |>
    unique()

age <- load_players() |>
    filter(last_season >= train_year) |>
    dplyr::select(gsis_id, years_of_experience, draft_pick) |>
    rename(player_id = gsis_id, experience = years_of_experience) |>
    mutate(experience = experience - (2026 - train_year))

injuries <- map_dfr(2022:2026, load_injuries) |>
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
    left_join(age, by = "player_id")

train_data <- data |>
    na.omit(position_group_2025)

test_data <- data |>
    na.omit(position_group_2026)

names(train_data) <- sub(paste0("_", train_year, "$"), "_0", names(train_data))
names(train_data) <- sub(paste0("_", train_year - 1, "$"), "_1", names(train_data))
names(train_data) <- sub(paste0("_", train_year - 2, "$"), "_2", names(train_data))
names(train_data) <- sub(paste0("_", train_year - 3, "$"), "_3", names(train_data))

names(test_data) <- sub(paste0("_", 2026, "$"), "_0", names(test_data))
names(test_data) <- sub(paste0("_", 2025, "$"), "_1", names(test_data))
names(test_data) <- sub(paste0("_", 2024, "$"), "_2", names(test_data))
names(test_data) <- sub(paste0("_", 2023, "$"), "_3", names(test_data))

nn_model <- nnet(
    points_0 ~ (
        position_group_0 + count_3 + count_2 + count_1 + points_3 + 
        points_2 + points_1 + experience + draft_pick
        ),
    data = train_data, 
    size = 10,      # hidden neurons
    linout = TRUE, # regression instead of classification
    decay = 0.01,  # weight decay to reduce overfitting
    maxit = 10000,
    trace = TRUE
)

forest_model <- randomForest(
    points_0 ~ position_group_0 + count_3 + count_2 + count_1 + points_3 + 
        points_2 + points_1 + experience + draft_pick,
    data = train_data, ntree = 500, mtry = 2, importance = TRUE
)

linear_model <- lm(
    points_0 ~ (points_3 + points_2 + points_1),
    data = train_data
    )

lda_model <- lda(
    points_0 ~ (points_3 + points_2 + points_1),
    data = train_data
)

svr_model <- svm(
    points_0 ~ (points_3 + points_2 + points_1),
    data = train_data, type = "eps-regression", kernel = "radial"
)


plsr_model <- plsr(
    points_0 ~ (points_3 + points_2 + points_1),
    data = train_data, validation = "LOO", scale = FALSE
)

nn_result <- predict(nn_model, test_data)
rf_result <- predict(forest_model, test_data)
lm_result <- predict(linear_model, test_data)
lda_result <- predict(lda_model, test_data)$class
svr_result <- predict(svr_model, test_data)
plsr_result <- predict(plsr_model, test_data)

test_data$prediction_nn <- nn_result
test_data$prediction_rf <- rf_result
test_data$prediction_lm <- lm_result
test_data$prediction_lda <- lda_result
test_data$prediction_svr <- svr_result
test_data$prediction_plsr <- as.data.frame(plsr_result)$`points_0.3 comps`

test_data <- test_data |>
    rowwise() |>
    mutate(
        prediction_max = max(prediction_nn, prediction_rf)
    )

model_metrics <- function(actual, predicted) {
    rmse <- sqrt(mean((actual - predicted) ^ 2))
    mae <- mean(abs(actual - predicted))
    r2 <- 1 - sum((actual - predicted) ^ 2) / sum((actual - mean(actual)) ^ 2)
    data.frame(RMSE = rmse, MAE = mae, R2 = r2)
}

c <- model_metrics(data$count_0, data$prediction_lm)
