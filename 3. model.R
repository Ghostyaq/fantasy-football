source("2. model_prep.R")

model_metrics <- function(actual, predicted) {
    rmse <- sqrt(mean((actual - predicted) ^ 2))
    mae <- mean(abs(actual - predicted))
    r2 <- 1 - sum((actual - predicted) ^ 2) / sum((actual - mean(actual)) ^ 2)
    data.frame(RMSE = rmse, MAE = mae, R2 = r2)
}

model_training <- function(selected, data) {
    output <- data$points_0
    data <- data[selected]
    data$points_0 <- output
    
    nn_model <- nnet(
        points_0 ~ ., data = data, size = 12, linout = TRUE, 
        decay = 0.01, maxit = 1000, trace = FALSE
    )
    
    forest_model <- randomForest(points_0 ~ ., data = data, ntree = 500, mtry = 2, importance = TRUE)
    linear_model <- lm(points_0 ~ ., data = data)
    svr_model <- svm(points_0 ~ ., data = data, type = "eps-regression", kernel = "radial")
    plsr_model <- plsr(points_0 ~ ., data = data, validation = "LOO", scale = FALSE)
    
    nn_result <- predict(nn_model, data)
    rf_result <- predict(forest_model, data)
    lm_result <- predict(linear_model, data)
    svr_result <- predict(svr_model, data)
    temp <- as.data.frame(predict(plsr_model, data))
    plsr_result <- temp[[paste0("points_0.", min(3, length(temp)), " comps")]]
    
    result_list <- list(
        nn = nn_result,
        rf = rf_result,
        lm = lm_result,
        svr = svr_result,
        plsr = plsr_result
    )
    
    result <- map_df(result_list, ~ model_metrics(output, .x), .id = "model")
    result$model <- c(
        list(nn_model), list(forest_model), list(linear_model), 
        list(svr_model), list(plsr_model)
        )
    result$model_type <- c("NN", "RF", "LM", "SVR", "PLSR")
    
    return(result)
}

# QB, SPEC, WR, RB, TE
position_train <- function(position, data, train_year) {
    train_data <- data |>
        drop_na(.data[[paste0("position_group_", (train_year - 1))]]) |>
        filter(.data[[paste0("position_group_", (train_year - 1))]] == position) |>
        mutate(across(everything(), function(i) {ifelse(is.na(i), 0, i) }))
    
    test_data <- data |>
        drop_na(.data[[paste0("position_group_", train_year)]]) |>
        filter(.data[[paste0("position_group_", train_year)]] == position) |>        
        mutate(across(everything(), function(i) {ifelse(is.na(i), 0, i) }))
    
    names(train_data) <- sub(paste0("_", train_year, "$"), "_0", names(train_data))
    names(train_data) <- sub(paste0("_", train_year - 1, "$"), "_1", names(train_data))
    names(train_data) <- sub(paste0("_", train_year - 2, "$"), "_2", names(train_data))
    names(train_data) <- sub(paste0("_", train_year - 3, "$"), "_3", names(train_data))
    
    names(test_data) <- sub(paste0("_", 2025, "$"), "_0", names(test_data))
    names(test_data) <- sub(paste0("_", 2024, "$"), "_1", names(test_data))
    names(test_data) <- sub(paste0("_", 2023, "$"), "_2", names(test_data))
    names(test_data) <- sub(paste0("_", 2022, "$"), "_3", names(test_data))
    
    train_data_subset <- train_data |>
        dplyr::select(
            count_3, count_2, count_1, points_3, points_2, points_1, 
            experience, draft_pick, points_0
        )
    
    numbers <- 1:8
    all_combos <- lapply(1:8, function(x) {
        combn(numbers, x, simplify = FALSE)
    })
    all_combos <- unlist(all_combos, recursive = FALSE)
    
    result <- pblapply(
        X = all_combos,
        FUN = model_training,
        data = train_data_subset
        ) |> 
        list_rbind() |>
        rename(old_RMSE = RMSE, old_MAE = MAE, old_R2 = R2)
    
    temp <- data.frame()
    for (i in 1:nrow(result)) {
        model <- result[[i, 1]]
        prediction <- predict(model, test_data)
        df <- model_metrics(test_data$points_0, prediction)
        temp <- rbind(temp, df)
    }
    
    result <- cbind(result, temp)
    return(result)
}

qb <- position_train("QB", data, train_year) |> arrange(desc(MAE))
wr <- position_train("WR", data, train_year) |> arrange(desc(MAE))
te <- position_train("TE", data, train_year) |> arrange(desc(MAE))
spec <- position_train("SPEC", data, train_year) |> arrange(desc(MAE))
rb <- position_train("RB", data, train_year) |> arrange(desc(MAE))

position_test <- function(position, model, data) {
    result <- data |>
        drop_na(position_group_2026) |>
        filter(position_group_2026 == position) |>
        mutate(across(everything(), function(i) {ifelse(is.na(i), 0, i) }))

    names(result) <- sub(paste0("_", 2026, "$"), "_0", names(result))
    names(result) <- sub(paste0("_", 2025, "$"), "_1", names(result))
    names(result) <- sub(paste0("_", 2024, "$"), "_2", names(result))
    names(result) <- sub(paste0("_", 2023, "$"), "_3", names(result))
    
    predictions <- predict(model, newdata = result)
    result$prediction <- predictions
    
    result <- result |>
        arrange(desc(predictions))
    
    return(result)
}

data[data$player_display_name == "Brock Bowers", ]$position_group_2026 = "TE"

result <- position_test("QB", qb[[1, 1]], data) |>
    rbind(position_test("WR", wr[[2, 1]], data)) |>
    rbind(position_test("TE", te[[1, 1]], data)) |>
    rbind(position_test("RB", rb[[1, 1]], data)) |>
    rbind(position_test("SPEC", spec[[1, 1]], data)) |>
    dplyr::select(
        player_id, player_display_name, position_group_0,
        points_3, points_2, points_1, points_0, experience, draft_pick,
        prediction
    )
    

