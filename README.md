
- [FastForecast](#fastforecast)
- [1 Installation](#1-installation)
  - [1.1 Help & Documentation](#11-help--documentation)
- [2 Functions](#2-functions)
  - [2.1 Time-Series transformation](#21-time-series-transformation)
  - [2.2 Atypical Points Correction](#22-atypical-points-correction)
  - [2.3 Seasonality Detection](#23-seasonality-detection)
  - [2.4 Seasonality Correction](#24-seasonality-correction)
  - [2.5 Seasonality Verification](#25-seasonality-verification)
  - [2.6 Stationarity Treatment](#26-stationarity-treatment)
  - [2.7 Aligning Time-Series length](#27-aligning-time-series-length)
  - [2.8 Creating a dataframe with each
    time-series](#28-creating-a-dataframe-with-each-time-series)
  - [2.9 Variables Selection](#29-variables-selection)
  - [2.10 Econometrics Forecasting
    Models](#210-econometrics-forecasting-models)
    - [2.10.1 Machine-Learning Forecasting
      Models](#2101-machine-learning-forecasting-models)

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- TOC_PLACEHOLDER -->

# FastForecast

<!-- badges: start -->
<!-- badges: end -->

The goal of FastForecast is to provide a fast and accurate forecasting
method for time series data. This package takes a **dataframe** of
several time series as input, and can correct for **atypical points**,
**seasonality** and **stationarity**. It then generates forecasting
models using **6 econometric models** and **6 Machine-Learning models**.
It also allows you to display series graphically, calculate forecast
quality indicators and display them in table form as well as
graphically.

# 1 Installation

You can install the development version of FastForecast from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("NoaLRX/FastForecast")
```

## 1.1 Help & Documentation

``` r
help(package="FastForecast")

# Get the list of functions available from the package
ls("package:FastForecast")
```

# 2 Functions

Here are the different functions that you can use in this package.

## 2.1 Time-Series transformation

``` r
library(FastForecast)
ts_transfo <- function(DATAFRAME, YEAR, MONTH, FREQUENCY)
```

This function transforms the $n$ columns of a dataframe into $n$
individuals Time Series.

Each time series will take the name `"ts_"` followed by the name of the
column in the dataframe.

To use this function you need to pass a dataframe as the first argument,
a year, month and frequency as the last argument.

<u>For example:</u>

``` r
ts_transfo(land_w, 2013, 01, 4) # Start the 1st January 2013, with quarterly data
ts_transfo(land_w, 1996, 05, 12) # Start the 5 May 1996, with monthly data
```

## 2.2 Atypical Points Correction

This function takes no argument, and will **correct** your previously
created time-series and correct any atypical points. This function use
the `tso()` function from the package
[tsoutliers](https://www.rdocumentation.org/packages/tsoutliers/versions/0.6-8/topics/tso).

``` r
library(tsoutliers)
atypical_tso()
```

## 2.3 Seasonality Detection

This function takes no argument, and will **detect** any seasonality on
your previously created time-series. This function use the
`combined_test()` and `seasdum()` functions from the package
[seastests](https://www.rdocumentation.org/packages/seastests/versions/0.15.4).

``` r
library(seastests)
seaso_detect()
```

## 2.4 Seasonality Correction

This function takes no argument, and will **correct** any seasonality on
your previously created time-series. This function use the `stl()`
function from the package
[stats](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/stl).

``` r
library(stats)
seaso_correct()
```

## 2.5 Seasonality Verification

This function takes no argument, and will **detect** any seasonality on
your previously created and corrected time-series. This function use the
`combined_test()` and `seasdum()` functions from the package
[seastests](https://www.rdocumentation.org/packages/seastests/versions/0.15.4).

The main purpose of this function is to ensure that there is no
seasonality in the corrected series.

``` r
library(seastests)
seaso_verif()
```

## 2.6 Stationarity Treatment

This function takes no argument, and will **detect** and **correct,** if
necessary, the time series that are not stationary. The function detect
stationarity with the `adf.test()` function from the package
[tseries](https://www.rdocumentation.org/packages/tseries/versions/0.10-54/topics/adf.test).

``` r
library(tseries)
statio()
```

If after a first differentiation, the time-serie is still not
stationary, the function will differentiate a second time. If after
that, the time serie is still not stationary, the function will print a
message and stop any treatment.

## 2.7 Aligning Time-Series length

This function takes no argument, and will **align** your corrected time
series that may have different length, because of differentiations.

``` r
allign_ts()
```

## 2.8 Creating a dataframe with each time-series

This function takes one argument, the $Y_t$ variable, the one that
you’re trying to predict.

This function will create a single data frame `results_df` with each
time series as columns, with the $Y_t$ as the first variable.

``` r
create_df("my_Y_variable")
```

## 2.9 Variables Selection

This function uses two methods of variables selections. The first is the
***“BestSubSet”*** method from the
[leaps](https://cran.r-project.org/web/packages/leaps/leaps.pdf)
package. The second is the ***“Gets”*** method from the
[gets](https://cran.r-project.org/web/packages/gets/gets.pdf) package.

This function takes one argument, the $Y_t$ variable, the one that
you’re trying to predict.

``` r
vselec("my_Y_variable")
```

After using the function, you should modify your `results_df` dataframe
and remove the variables not retained by the two methods.

## 2.10 Econometrics Forecasting Models

<u>This function takes three arguments:</u>

- `DATAFRAME`: Your dataframes with $n$ rows and $p$ columns (with
  $n > p$)

- `Y_VARIABLE`: Your $Y_t$ variable, meaning the variable you’re trying
  to predict.

- `PERIOD`: The frequency of your time-series *(12 for monthly, 4 for
  quarterly…)*

``` r
eco_models(df, "My_Y_Variable", 12)
eco_models(dataframe, "Gas_Price", 4)
```

<u>This function uses the following models and packages:</u>

- ARX model with gets
  ([gets](https://cran.r-project.org/web/packages/gets/gets.pdf))

- ARX moddel with auto.arima
  ([forecast](https://www.rdocumentation.org/packages/forecast/versions/8.21.1))

- ARMAX model with auto.arima
  ([forecast](https://www.rdocumentation.org/packages/forecast/versions/8.21.1))

- Naive model
  ([forecast](https://www.rdocumentation.org/packages/forecast/versions/8.21.1))

- LM model
  ([stats](https://www.rdocumentation.org/packages/stats/versions/3.6.2))

- AR1 mode
  ([forecast](https://www.rdocumentation.org/packages/forecast/versions/8.21.1))

- GAM model
  ([mgcv](https://www.rdocumentation.org/packages/mgcv/versions/1.9-0))

### 2.10.1 Machine-Learning Forecasting Models

<u>This function takes three arguments:</u>

- `DATAFRAME`: Your dataframes with $n$ rows and $p$ columns (with
  $n > p$)

- `Y_VARIABLE`: Your $Y_t$ variable, meaning the variable you’re trying
  to predict.

``` r
ml_models(df, "My_Y_Variable")
```

<u>This function uses the following models and packages:</u>

- MLP models
  ([neuralnet](https://www.rdocumentation.org/packages/neuralnet/versions/1.44.2))

- MARS model
  ([earth](https://www.rdocumentation.org/packages/earth/versions/5.3.2))

- SVM model
  ([e1071](https://www.rdocumentation.org/packages/e1071/versions/1.1-3))

- Random Forest
  ([randomForest](https://www.rdocumentation.org/packages/randomForest/versions/4.7-1.1))

- XGB Boost
  ([xgboost](https://www.rdocumentation.org/packages/xgboost/versions/1.7.5.1))

- kNN
  ([caret](https://www.rdocumentation.org/packages/caret/versions/6.0-94))
