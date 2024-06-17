#' Forecasting with 6 econometrics models
#' @description
#' This function takes three arguments: DATAFRAME: Your dataframes with \( n \)
#' rows and \( p \) columns (with \( n > p \)). Y_VARIABLE: Your \( Y_t \) variable,
#' meaning the variable you're trying to predict. PERIOD: The frequency of your
#' time-series (12 for monthly, 4 for quarterly, etc...). This function uses the
#' following models and packages: ARX model with gets (gets). ARX model with auto.arima
#' (forecast). ARMAX model with auto.arima (forecast). Naive model (forecast).
#' LM model (stats). AR1 model (forecast). GAM model (mgcv).A short description...
#'
#' @param DATAFRAME an R dataframe
#' @param Y_VARIALBE the Yt variable you're trying to predict
#' @param PERIOD a frequency value, e.g. 12 for monthly data, 4 for quarterly data, etc.
#' @import gets, forecast, stats, mgcv
#' @return RMSE for each models and the predictions
#' @export
#'
#' @examples
#' eco_models(df, "my_Y_variable", 12)
#' eco_models(result_df, "Food", 4)
#' eco_models(datafrale, "VIX", 12)
#'

eco_models <- function(DATAFRAME, Y_VARIABLE, PERIOD){

  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size + 1):n, ]
  y_real <- test_data[[Y_VARIABLE]]


  # ARX model with Gets----
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size+1):n, ]
  mX_train <- data.matrix(train_data[,-1])
  mX_test <- data.matrix(test_data[,-1])
  y_train <- train_data[[Y_VARIABLE]]
  y_test <- test_data[[Y_VARIABLE]]
  model <- arx(y_train, mc = TRUE, ar = 1, mxreg = mX_train, vcov.type = "ordinary")
  n_test <- nrow(test_data)
  p_arxget <- predict(model, n.ahead = n_test, newmxreg = mX_test)
  p_arxget <- as.numeric(p_arxget)
  rmse_arxget <- sqrt(mean((y_real - p_arxget)^2, na.rm = TRUE))
  print(paste("RMSE ARX-GETS: ", rmse_arxget))


  ## ARX model with auto_arima----
  split <- round(nrow(DATAFRAME) * 0.8)
  train_df <- DATAFRAME[1:split, ]
  test_df <- DATAFRAME[(split+1):nrow(DATAFRAME), ]
  y_train <- train_df[[Y_VARIABLE]]
  xreg_train <- data.matrix(train_df[,-1])
  modelx_train <- auto.arima(y_train, max.q = 0, xreg = xreg_train, seasonal = FALSE, stationary = TRUE)
  xreg_test <- data.matrix(test_df[,-1])
  p_arx <- predict(modelx_train, newxreg = xreg_test, n.ahead = nrow(test_df))$pred
  p_arx <- as.numeric(p_arx)
  rmse_arx <- sqrt(mean((y_real - p_arx)^2, na.rm = TRUE))
  print(paste("RMSE ARX: ", rmse_arx))


  ## ARMAX model with auto_arima----
  split <- round(nrow(DATAFRAME) * 0.8)
  train_df <- DATAFRAME[1:split, ]
  test_df <- DATAFRAME[(split+1):nrow(DATAFRAME), ]
  y_train <- train_df[[Y_VARIABLE]]
  xreg_train <- data.matrix(train_df[,-1])
  modelx_train <- auto.arima(y_train, xreg = xreg_train, seasonal = FALSE, stationary = TRUE)
  j <- ncol(modelx_train$var.coef)
  tstat <- matrix(nrow=j, ncol=1)
  for(i in 1:j)
  {
    tstat[i,1] <- modelx_train$coef[i]/sqrt(modelx_train$var.coef[i,i])
  }
  tstat
  xreg_test <- data.matrix(test_df[,-1])
  p_armax <- predict(modelx_train, newxreg = xreg_test, n.ahead = nrow(test_df))$pred
  p_armax <- as.numeric(p_armax)
  rmse_armax <- sqrt(mean((y_real - p_armax)^2, na.rm = TRUE))
  print(paste("RMSE ARMAX: ", rmse_armax))



  ## Naive model----
  y_train <- ts(train_df[[Y_VARIABLE]])
  naive_model <- rwf(y_train, h=nrow(test_df))
  p_naive <- naive_model$mean
  p_naive <-  as.numeric(p_naive)
  rmse_naive <- sqrt(mean((y_real - p_naive)^2, na.rm = TRUE))
  print(paste("RMSE Naive: ", rmse_naive))


  ## LM model----
  set.seed(123)
  split <- round(nrow(DATAFRAME) * 0.8)
  train_df <- DATAFRAME[1:split, ]
  test_df <- DATAFRAME[(split + 1):nrow(DATAFRAME), ]
  model_lm <- lm(formula(paste(Y_VARIABLE, "~", ".")), data = train_df)
  p_lm <- predict(model_lm, newdata = test_df)
  p_lm <- as.numeric(p_lm)
  rmse_lm <- sqrt(mean((y_real - p_lm) ^ 2, na.rm = TRUE))
  print(paste("RMSE LM: ", rmse_lm))


  ## AR1 model----
  y <- ts(DATAFRAME[[Y_VARIABLE]])
  model_ar1 <- Arima(y, order = c(1, 0, 0), seasonal = list(order = c(0, 0, 0), period = PERIOD), lambda = 1)
  forecast_ar1 <- forecast(model_ar1, h = nrow(test_data))
  p_ar1 <- forecast_ar1$mean[1:length(y_real)]
  p_ar1 <- as.numeric(p_ar1)
  rmse_ar1 <- sqrt(mean((y_real - p_ar1)^2, na.rm = TRUE))
  print(paste("RMSE AR1: ", rmse_ar1))


  ## GAM model----
  ## Data preparation
  set.seed(123)
  n <- nrow(DATAFRAME)
  train_size <- round(0.8 * n)
  train_data <- DATAFRAME[1:train_size, ]
  test_data <- DATAFRAME[(train_size + 1):n, ]
  y_real <- test_data[[Y_VARIABLE]]

  other_vars <- setdiff(names(train), Y_VARIABLE)
  smooth_terms <- paste0("s(", other_vars, ")")
  model_formula <- reformulate(termlabels = smooth_terms, response = Y_VARIABLE)

  model_gam <- gam(model_formula, data = train)

  ## Prediction and RMSE calculation
  p_gam <- predict(model_gam, newdata = test)
  p_gam <- as.numeric(p_gam)
  y_real <- test[[Y_VARIABLE]]

  rmse_gam <- sqrt(mean((y_real - p_gam)^2, na.rm = TRUE))
  print(paste("RMSE GAM :", rmse_gam))


  # Create a list to store the results
  results <- list()

  # Store the results in the R Global Environment
  assign("y_real", y_real, envir = .GlobalEnv)
  assign("p_arxget", p_arxget, envir = .GlobalEnv)
  assign("p_arx", p_arx, envir = .GlobalEnv)
  assign("p_armax", p_armax, envir = .GlobalEnv)
  assign("p_naive", p_naive, envir = .GlobalEnv)
  assign("p_lm", p_lm, envir = .GlobalEnv)
  assign("p_ar1", p_ar1, envir = .GlobalEnv)
  assign("p_gam", p_gam, envir = .GlobalEnv)


}
