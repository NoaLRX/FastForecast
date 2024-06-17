#' Stationarity Treatment
#' @description
#' This function takes no argument, and will detect and correct, if necessary,
#' the time series that are not stationary. The function detect stationarity with
#' the adf.test() function from the package tseries.
#' @import tseries
#' @return a list of time series starting with "ts_" followed by the column name followed by "_adj"
#' @export
#'
#' @examples
#' statio()
#'

statio <- function() {
  # Stationarity----
  for (i in names(ts_list_adj)) {
    ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
    adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
    adf_result

    # Check if P-Value > 0.05
    if (adf_result$p.value > 0.05) {
      # Apply a difference to the time series if it is not stationary
      assign(ts_name, diff(get(ts_name)), envir = .GlobalEnv)
      print(paste("The TS", ts_name, "has been differentiated"))
    }
    else {
      print(paste("The TS", ts_name, "is stationary"))
    }
  }

  # Differentiate a second time if necessary
  for (i in names(ts_list_adj)) {
    ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
    adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
    adf_result
    # Check if P-Value > 0.05
    if (adf_result$p.value > 0.05) {
      print(paste("The TS", ts_name, "is still not stationary"))
      # Apply a difference to the time series if it is not stationary
      assign(ts_name, diff(get(ts_name)), envir = .GlobalEnv)
      print(paste("The TS", ts_name, "has been differentiated a second time"))
    }
  }


  for (i in names(ts_list_adj)) {
    ts_name <- paste0("ts_", i, "_adj")  # Name of the TS
    adf_result <- adf.test(get(ts_name))  # Apply ADF test for stationnarity
    adf_result
    if (adf_result$p.value > 0.05) {
      # Check if P-Value > 0.05
      cat(bold(
        underline(
          "\033[1m\033[31m",
          "The time serie ",
          ts_name,
          "is still not stationnary",
          ":\033[0m\n"
        )
      ))
    }
  }

}
