#' Seasonality Verification
#' @description
#' This function takes no argument, and will detect any seasonality on your previously
#' created and corrected time-series. This function use the combined_test() and seasdum()
#' functions from the package seastests. The main purpose of this function is to
#' ensure that there is no seasonality in the corrected series.
#' @import seastests
#' @return a list of time series starting with "ts_" followed by the column name followed by "_adj"
#' @export
#'
#' @examples
#' seaso_verif()
#'
seaso_verif <- function(){

  # Initialize lists to store the results
  seasonal_combined_test_after <- c()
  seasonal_seasdum_after <- c()

  for (ts_name in both_tests) {
    ts_data <- get(ts_name)  # Retrieve data from the adjusted time series

    # Apply the combined_test() test to the adjusted time series
    ct_res <- combined_test(ts_data)

    # Check each p-value and display results if at least one is < 0.05
    ct_pvals <- ct_res$Pval
    ct_results <- ct_res$stat
    if (ct_results == TRUE) {
      cat(bold(underline("\033[1m\033[31m", "Corrected combined_test results for the series ", ts_name, ":\033[0m\n")))
      print(ct_res)
      seasonal_combined_test_after <- c(seasonal_combined_test_after, ts_name)
    }

    # Apply the seasdum() test to the adjusted time series
    sd_res <- seasdum(ts_data)

    # Check and print results if p<0.05
    if (sd_res$Pval < 0.05) {
      cat(bold(underline("\033[1m\033[31m", "Results of Seasdum after correction for the TS: ", ts_name, ":\033[0m\n")))
      print(sd_res)
      seasonal_seasdum_after <- c(seasonal_seasdum_after, ts_name)
    }
  }


  cat(bold("\nSeries still showing seasonality according to combined_test after correction:\n"))
  print(seasonal_combined_test_after)

  cat(bold("\nSeries still showing seasonality according to seasdum after correction:\n"))
  print(seasonal_seasdum_after)
}
