#' Forecasting with 6 Machine-Learning models
#' @description
#' Machine-Learning Forecasting Models This function takes three arguments:
#' DATAFRAME: Your dataframes with \( n \) rows and \( p \) columns (with \( n > p \)).
#' Y_VARIABLE: Your \( Y_t \) variable, meaning the variable you're trying to predict.
#' This function uses the following models and packages: MLP models (neuralnet).
#' MARS model (earth). SVM model (e1071). Random Forest (randomForest).
#' XGB Boost (xgboost). kNN (caret).
#'
#' @param DATAFRAME an R dataframe
#' @param Y_VARIALBE the Yt variable you're trying to predict
#' @import neuralnet, earth, e1071, randomForest, xgboost, caret
#' @return RMSE for each models and the predictions
#' @export
#'
#' @examples
#' ml_models(df, "my_Y_variable")
#' ml_models(result_df, "Food")
#' ml_models(datafrale, "VIX")
#'
ml_models <- function(DATAFRAME, Y_VARIABLE){

  ## MLP model----
  set.seed(123)
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size + 1):n, ]
  y_real <- test_data[[Y_VARIABLE]]

  mlp_model <- neuralnet(formula(paste(Y_VARIABLE, "~", ".")), data = train, hidden = 1)

  p_mlp <- predict(mlp_model, test[,-1])
  #p_mlp <- as.numeric(p_mlp$net.result)

  rmse_ml <- sqrt(mean((y_real - p_mlp)^2, na.rm = TRUE))
  print(paste("RMSE for MLP:", rmse_ml))


  ## Mars model----
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train <- DATAFRAME[1:train_size, ]
  test <- DATAFRAME[(train_size+1):n, ]

  mars_model <- earth(formula(paste(Y_VARIABLE, "~", ".")), data = train)

  p_mars <- predict(mars_model, newdata = test)
  p_mars <- as.numeric(p_mars)


  rmse_mars <- sqrt(mean((y_real - p_mars)^2, na.rm = TRUE))
  print(paste("RMSE for MARS:", rmse_mars))


  ## SVM model----
  # Find optimal parameters
  svm_grid <- expand.grid(C = c(0.1, 1, 10), sigma = c(0.001, 0.01, 0.1))
  train <- DATAFRAME

  svm_model_opt <- train(
    as.formula(paste(Y_VARIABLE, "~.")), #
    data = train,
    method = "svmRadial",
    preProcess = c("center", "scale"),
    tuneGrid = svm_grid
  )

  svm_model_best <- svm(
    as.formula(paste(Y_VARIABLE, "~.")),
    data = train,
    kernel = "radial",
    sigma = svm_model_opt$bestTune$sigma,
    cost = svm_model_opt$bestTune$C,
    scale = TRUE
  )

  p_svm <- predict(svm_model_best, newdata = test)
  p_svm <- as.numeric(p_svm)

  rmse_svm <- sqrt(mean((y_real - p_svm)^2))
  print(paste("RMSE for SVM:", rmse_svm))



  ## Modèle Random Forest ----
  set.seed(123)
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train <- DATAFRAME[1:train_size, ]
  test <- DATAFRAME[(train_size+1):n, ]
  rf_model <- randomForest(formula(paste(Y_VARIABLE, "~", ".")), data = train, ntree=200)
  p_rf <- predict(rf_model, newdata = test)

  rmse_rf <- sqrt(mean((y_real - p_rf)^2))
  print(paste("RMSE Random Forest:", rmse_rf)) # 1.89


  # XGB BOOST----
  zz <- file("xgb_warnings.txt", open = "wt")
  sink(zz, type = "output")


  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train <- DATAFRAME[1:train_size, ]
  test <- DATAFRAME[(train_size + 1):n, ]
  xgb_grid <- expand.grid(
    nrounds = c(100, 200),
    eta = c(0.01, 0.1),
    max_depth = c(6, 10),
    gamma = c(0, 1),
    colsample_bytree = c(0.6, 0.8, 1),
    min_child_weight = c(1, 5),
    subsample = c(0.5, 0.75, 1)
  )
  tr_control <- trainControl(method = "cv", number = 10)
  xgb_model <- train(
    formula(paste(Y_VARIABLE, "~", ".")),
    data = train,
    method = "xgbTree",
    trControl = tr_control,
    tuneGrid = xgb_grid
  )

  set.seed(123)

  dtrain <- xgb.DMatrix(data = as.matrix(train[, -1]), label = train[[Y_VARIABLE]])
  dtest <- xgb.DMatrix(data = as.matrix(test[, -1]), label = test[[Y_VARIABLE]])

  params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eta = xgb_model$bestTune$eta,
    gamma = xgb_model$bestTune$gamma,
    max_depth = xgb_model$bestTune$max_depth,
    min_child_weight = xgb_model$bestTune$min_child_weight,
    subsample = xgb_model$bestTune$subsample,
    colsample_bytree = xgb_model$bestTune$colsample_bytree
  )

  xgb_model_opt <- xgb.train(params = params,
                             data = dtrain,
                             nrounds = 200)

  p_xgb <- predict(xgb_model_opt, newdata = dtest)
  p_xgb <- as.numeric(p_xgb)

  # Close the file and restore the output to the console
  sink()
  if (!isOpen(zz)) {
    close(zz)
  }

  # Print results
  rmse_xgb <- sqrt(mean((y_real - p_xgb) ^ 2))
  print(paste("RMSE for XGB:", rmse_xgb))



  # kNN Model ----
  set.seed(123)
  total_rows <- nrow(DATAFRAME)
  train_rows <- round(0.8 * total_rows)
  train_set <- DATAFRAME[1:train_rows, ]
  test_set <- DATAFRAME[(train_rows + 1):total_rows, ]
  train_labels <- train_set[[Y_VARIABLE]]
  train_data <- train_set[, -1]
  test_labels <- test_set[[Y_VARIABLE]]
  test_data <- test_set[, -1]


  max_k <- sqrt(train_rows)
  k_grid <- expand.grid(k = seq(1, max_k, by = 1))
  trControl <- trainControl(method = "cv", number = 10)
  knn_model <- train(
    formula(paste(Y_VARIABLE, "~", ".")), data = train, method = "knn", tuneGrid = k_grid, trControl = trControl)
  results <- knn_model$results
  min_rmse <- min(results$RMSE, na.rm = TRUE)

  knn_model <- train(
    formula(paste(Y_VARIABLE, "~", ".")),
    data = train_set, method = "knn", trControl = trControl, tuneGrid = expand.grid(k = 8))

  p_knn <- predict(knn_model, newdata = test_data)
  p_knn <- as.numeric(p_knn)

  rmse_knn <- sqrt(mean((y_real - p_knn)^2))
  print(paste("RMSE for kNN:", rmse_knn))

  # Store the results in the R Global Environment
  assign("p_mlp", p_mlp, envir = .GlobalEnv)
  assign("p_mars", p_mars, envir = .GlobalEnv)
  assign("p_svm", p_svm, envir = .GlobalEnv)
  assign("p_rf", p_rf, envir = .GlobalEnv)
  assign("p_xgb", p_xgb, envir = .GlobalEnv)
  assign("p_knn", p_knn, envir = .GlobalEnv)


}
