#' Correct Time-Series from atypical points
#' @description
#' This function takes no argument, and will correct your previously created time-series
#' and correct any atypical points. This function use the tso() function from the package tsoutliers
#' @import tsoutliers
#' @return a list of time series starting with "ts_" followed by the column name followed by "_adj"
#' @export
#'
#' @examples
#' atypical_tso()
#'
atypical_tso <- function() {
  ts_list_adj <- list()  # Initialize the new list

  for (col_name in names(ts_list)) {
    ts_name <- paste0("ts_", col_name)
    ts_name_tso <- paste0("tso_", col_name)
    ts_name_adj <- paste0("ts_", col_name, "_adj")

    ts_data <- get(ts_name)

    # Try ARIMA models
    arima_fit <- tryCatch({
      fit <- tso(ts_data)
      cat("\033[1m\033[31m", "TSO for", col_name, ":\033[0m\n")
      print(fit)
      assign(ts_name_tso, fit)
      assign(ts_name_adj, fit$yadj, envir = .GlobalEnv)

      # Add the adjusted series to ts_list_adj
      ts_list_adj[[col_name]] <- fit$yadj

      # Check if there are atypical points before plotting
      if (!is.null(fit$outliers) && nrow(fit$outliers) > 0) {
        plot(fit)
        title(main = paste("TSO for", col_name))
      }

      TRUE
    }, error = function(e) {
      cat("\033[1m\033[31m", "Error for", col_name, ":\033[0m\n")
      cat(e$message, "\n")
      FALSE
    })

    # If ARIMA adjustment fails, go to the next iteration
    if (!arima_fit)
      next

  }

  # Assign ts_list_adj to the global environment
  assign("ts_list_adj", ts_list_adj, envir = .GlobalEnv)
}
