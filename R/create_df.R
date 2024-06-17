#' Merge time-series into a single dataframe
#'
#' @param first_column the name of the Y-variable column that you have to put first in the dataframe
#' @description This function will create a single data frame with each time series as columns, with the Y-variable as the first variable.
#' @return a data frame with the Y-variable (the one that your trying to predct) as the first column.
#' @export
#'
#' @examples
#' create_df(df, "my_Y_variable")
#' create_df(dataframe, "Gas_Price")
#' create_df(financial_data, "VIX")
#'

create_df <- function(first_column = NULL) {
  ts_to_df <- function(ts_obj, series_name) {
    if (is.null(ts_obj$y)) {
      df <- as.data.frame(ts_obj)
    } else {
      df <- as.data.frame(ts_obj$y)
    }
    df$Year <- as.numeric(rownames(df))

    if (ncol(df) == 2) {
      names(df)[1] <- series_name
      df$Quarter <- "Annual"
    } else {
      df_long <- tidyr::pivot_longer(
        df,
        cols = -Year,
        names_to = "Quarter",
        values_to = series_name
      )
      df <- df_long
    }

    return(df)
  }

  first_series <- get(series_list[1])
  result_df <- ts_to_df(first_series, series_list[1])

  for (series_name in series_list[-1]) {
    ts_obj <- get(series_name)

    df <- ts_to_df(ts_obj, series_name)

    result_df <- merge(result_df,
                       df,
                       by = c("Year", "Quarter"),
                       all = TRUE)
  }

  result_df <- result_df[order(result_df$Year, result_df$Quarter), ]

  # Réorganiser les colonnes si first_column est spécifié
  if (!is.null(first_column) && first_column %in% names(result_df)) {
    col_order <- c("Year", "Quarter", first_column)
    remaining_cols <- setdiff(names(result_df), col_order)
    result_df <- result_df[, c(col_order, remaining_cols)]
  } else {
    result_df <- result_df[, c("Year", "Quarter", series_list)]
  }

  result_df <- result_df[, !names(result_df) %in% "Year"]
  result_df <- result_df[, !names(result_df) %in% "Quarter"]
  new_names <- names(result_df)
  new_names <- gsub("^ts_", "", new_names)
  new_names <- gsub("_adj$", "", new_names)
  names(result_df) <- new_names
  assign("result_df", result_df, envir = .GlobalEnv)
  View(result_df)
}
