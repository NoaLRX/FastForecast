#' Merge time-series into a single dataframe
#'
#' @param first_column the name of the Y-variable column that you have to put first in the dataframe
#'
#' @return a data frame with the Y-variable (the one that your trying to predct) as the first column.
#' @export
#'
#' @examples
#' create_df(df, "my_Y_variable")
#' create_df(dataframe, "Gas_Price")
#' create_df(financial_data, "VIX")
#'

vselec <- function(Y_VARIABLE) {
  ## BestSubSet Method----
  train_size <- floor(0.8 * nrow(result_df))
  train <- result_df[1:train_size, ]

  formula <- as.formula(paste(Y_VARIABLE, "~ ."))

  leaps <- regsubsets(
    formula,
    data = train,
    nbest = 1,
    method = c("exhaustive")
  )

  res.sum <- summary(leaps)

  results <- data.frame(
    Adj.R2 = which.max(res.sum$adjr2),
    CP = which.min(res.sum$cp),
    BIC = which.min(res.sum$bic)
  )

  print(results)

  plot(leaps, scale = "adjr2", main = "Adjusted R2")
  plot(leaps, scale = "bic", main = "BIC")

  print(leaps)


  # GETS Method----
  train_size <- floor(0.8 * nrow(result_df))
  train <- result_df[1:train_size, ]
  mX <- data.matrix(train)
  modele_arx <- arx(train[[Y_VARIABLE]], mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
  modele_arx
  seuil_p_value <- 0.05
  variables <- colnames(train[, 2:13])
  VRAI <- TRUE
  while (VRAI) {
    mX <- data.matrix(train[, variables])
    modele_arx <- arx(train[[Y_VARIABLE]], mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")

    p_values <- modele_arx[["mean.results"]][["p-value"]][-c(1, 2)]
    max_p_value <- max(p_values)

    if (max_p_value > seuil_p_value) {
      variable_a_supprimer <- variables[which.max(p_values)]
      variables <- setdiff(variables, variable_a_supprimer)
    } else {
      VRAI <- FALSE
    }
  }
  arx_final <- arx(train$Food, mc = TRUE, ar = 1, mxreg = mX, vcov.type = "ordinary")
  modele_gets <- getsm(arx_final) # Avoir les coeff du modele ARX  + lunchbox test
  modele_gets

}
