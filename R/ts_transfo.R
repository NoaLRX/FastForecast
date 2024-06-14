#' Transform dataframe columns into Time Series
#'
#' @param DATAFRAME an R dataframe
#' @param YEAR a Year value, e.g. 2019
#' @param MONTH a Month value, e.g. 05 for May
#' @param FREQUENCY a frequency value, e.g. 12 for monthly data, 4 for quarterly data, etc.
#'
#' @return a list of time series starting with "ts_" followed by the column name
#' @export
#'
#' @examples
#' ts_transfo(df, 2019, 05, 12)
#' ts_transfo(df, 1994, 06, 4)
#'
ts_transfo <- function(DATAFRAME, YEAR, MONTH, FREQUENCY) {

  # Initialize the list to store the time-series
  ts_list <- list()

  # Loop over each column of the dataframe
  for (i in 1:ncol(DATAFRAME)) {
    col_name <- names(DATAFRAME)[i]
    ts_name <- paste0("ts_", col_name)
    ts_data <- ts(data = DATAFRAME[, col_name], start = c(YEAR, MONTH), frequency = FREQUENCY)
    ts_list[[col_name]] <- ts_data
    assign(ts_name, ts_data, envir = .GlobalEnv)

  }
  assign("ts_list", ts_list, envir = .GlobalEnv)
  return(ts_list)
}
