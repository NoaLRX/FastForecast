#' Seasonality Correction
#' @description
#' This function takes no argument, and will correct any seasonality on your previously
#' created time-series. This function use the stl() function from the package stats.
#' @import stats
#' @return a list of time series starting with "ts_" followed by the column name followed by "_adj"
#' @export
#'
#' @examples
#' seaso_correct()
#'
seaso_correct <- function(){

  for (ts_name in both_tests) {
    ts_data <- get(ts_name)  # Retrieve time series data
    decomp <- stl(ts_data, s.window = "periodic")  # STL decomposition
    seasonal <- decomp$time.series[, "seasonal"]  # Get the seasonal component
    ts_data_adjusted <- ts_data - seasonal  # Correcting the seasonal component
    assign(ts_name, ts_data_adjusted, envir = .GlobalEnv)  # Update the adjusted time series
  }
}
