#' FastForecast - Fast and Accurate Forecasting of Time Series Data
#'
#' This package provides a fast and accurate forecasting method for time series data.
#' It corrects for atypical points, handles seasonality and stationarity, and generates
#' forecasting models using econometric and machine-learning models.
#'
#' @docType package
#' @name FastForecast
#' @aliases FastForecast-package
#'
#' @author Noa Le Roux
#' @details
#' The goal of FastForecast is to provide a fast and accurate forecasting method
#' for time series data. This package takes a dataframe of several time series as
#' input, and can correct for atypical points, seasonality and stationarity. It
#' then generates forecasting models using 6 econometric models and 6 Machine-Learning
#' models. It also allows you to display series graphically, calculate forecast
#' quality indicators and display them in table form as well as graphically.


#'
#' @importFrom dplyr %>%
#' @import forecast
#' @import randomForest
#' @import xgboost
#' @import tsoutliers
#' @import seastests
#' @import stats
#' @import tseries
#' @import leaps
#' @import gets
#' @import mgcv
#' @import neuralnet
#' @import earth
#' @import e1071
#' @import caret
#'
NULL
