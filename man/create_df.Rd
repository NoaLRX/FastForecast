% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_df.R
\name{create_df}
\alias{create_df}
\title{Merge time-series into a single dataframe}
\usage{
create_df(first_column = NULL)
}
\arguments{
\item{first_column}{the name of the Y-variable column that you have to put first in the dataframe}
}
\value{
a data frame with the Y-variable (the one that your trying to predct) as the first column.
}
\description{
This function will create a single data frame with each time series as columns, with the Y-variable as the first variable.
}
\examples{
create_df(df, "my_Y_variable")
create_df(dataframe, "Gas_Price")
create_df(financial_data, "VIX")

}
