#' Aligning Time-Series length
#' @description
#' This function takes no argument, and will align your corrected time series that
#' may have different length, because of differentiations.
#' @return a list of time series starting with "ts_" followed by the column name followed
#' by "_adj", but now each time-series should have the same length.
#' @export
#'
#' @examples
#' allign_ts()
#'
allign_ts <- function() {
  series_list <- ls(pattern = "^ts_.*_adj$", envir = .GlobalEnv, all.names = TRUE)
  series_list <- series_list[series_list != "ts_list_adj"]
  assign("series_list", series_list, envir = .GlobalEnv)

  min_length <- Inf
  for (series_name in series_list) {
    series_obj <- get(series_name, envir = .GlobalEnv)
    series_lengths <- sapply(series_obj, length)
    min_length <- min(min_length, min(series_lengths, na.rm = TRUE))
  }

  for (series_name in series_list) {
    series_obj <- get(series_name, envir = .GlobalEnv)
    series_trimmed <- lapply(series_obj, function(x) {
      start_index <- length(x) - min_length + 1
      start_time <- time(x)[start_index]
      window(x, start = start_time)
    })
    assign(series_name, series_trimmed, envir = .GlobalEnv)
  }
}
