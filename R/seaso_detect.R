#' Seasonality Detection
#' @description
#' This function takes no argument, and will detect any seasonality on your previously
#' created time-series. This function use the combined_test() and seasdum() functions
#' from the package seastests.
#' @import seastests.
#' @return a list of time series starting with "ts_" followed by the column name followed by "_adj"
#' @export
#'
#' @examples
#' seaso_detect()
#'
seaso_detect <- function(){

  # Initialize lists to store the results
  seasonal_combined_test <- c()
  seasonal_seasdum <- c()

  # Loop over each TS
  for (col_name in names(ts_list_adj)) {
    ts_name <- paste0("ts_", col_name, "_adj")  # Name of adjusted time series
    ts_data <- get(ts_name)  # Retrieve data from the adjusted time series

    # Apply the combined_test() test to the adjusted time series
    ct_res <- combined_test(ts_data)

    # Check each p-value and display results if at least one valid H1
    ct_results <- ct_res$stat
    ct_pvals <- ct_res$Pval
    if (ct_results == TRUE) {
      cat("\n")
      cat("\n")
      print(paste0("Combined_test results for the series ", ts_name))
      print(ct_res)
      seasonal_combined_test <- c(seasonal_combined_test, ts_name)
    }

    # Apply the seasdum() test to the adjusted time series
    sd_res <- seasdum(ts_data)

    # Check p-value and display results if < 0.05
    if (sd_res$Pval < 0.05) {
      cat("\n")
      cat("\n")
      print(paste0("Résultats du seasdum pour la série ", ts_name))
      print(sd_res)
      seasonal_seasdum <- c(seasonal_seasdum, ts_name)
    }
  }

  assign("seasonal_combined_test", seasonal_combined_test, envir = .GlobalEnv)
  assign("seasonal_seasdum", seasonal_seasdum, envir = .GlobalEnv)


  # Calculate the differences between the two lists
  only_combined_test <- setdiff(seasonal_combined_test, seasonal_seasdum)
  only_seasdum <- setdiff(seasonal_seasdum, seasonal_combined_test)
  both_tests <- union(only_combined_test, only_seasdum)

  # Save the results
  assign("only_combined_test", only_combined_test, envir = .GlobalEnv)
  assign("only_seasdum", only_seasdum, envir = .GlobalEnv)
  assign("both_tests", both_tests, envir = .GlobalEnv)

  # Show differences
  cat("\n")
  cat("\n")
  cat("Seasonality detected only in combined_test function: ", paste(only_combined_test, collapse = ", "), "\n")
  cat("\n")
  cat("\n")
  cat("Seasonality detected only in seasdum function: ", paste(only_seasdum, collapse = ", "), "\n")
  cat("\n")
  cat("\n")
  cat("Seasonality detected in both test: ", paste(both_tests, collapse = ", "), "\n")



}
