#' @title Preprocess time series data
#' @description
#' This function loads a time series data file,
#' and aggregates data by month.
#'
#' @param file_path Path to the CSV file containing the data.
#' @param header A logical indicating presence of a header row (defaults to TRUE).
#' @param sep Character specifying the column separator (defaults to comma ",").
#'
#' @return A data frame with Month and Amount columns.
#'
#' @examples
#' # Create a sample dataset
#' sample_data <- data.frame(
#'   Date = as.Date('2023-01-01') + 0:9,
#'   Amount = rnorm(10, mean = 100, sd = 10)
#' )
#' # Write sample data to a temporary file
#' temp_file <- tempfile(fileext = ".csv")
#' write.csv(sample_data, temp_file, row.names = FALSE)
#' # Load and preprocess the sample data
#' preprocessed_data <- load_and_preprocess_data(temp_file)
#' # Clean up the temporary file
#' unlink(temp_file)
#'
#' @export
load_and_preprocess_data <- function(file_path, header = TRUE, sep = ",") {
  library(lubridate)
  library(dplyr)

  df <- read.csv(file_path, header = header, sep = sep)
  df$Date <- mdy(df$Date)
  df <- df %>% mutate(Month = floor_date(Date, "month")) %>%
    group_by(Month) %>%
    summarize(Amount = sum(Amount))
  return(df)
}

#' @title Forecast next month's amount
#' @description
#' This function takes a time series data 'Month' column
#' and an 'Amount' column, and forecasts the amount for the next month using Automatic ARIMA model.
#'
#' @param data A data frame containing the time series data. It should have a 'Date' or 'Month' column and an 'Amount' column.
#'
#' @return A list containing:
#'   * `next_month`: Character string representing the forecasted month (e.g., "July 2024").
#'   * `forecasted_value`: Numeric value representing the forecasted amount for the next month (rounded to 2 decimal places).
#'   * `forecast_next`: An object of class `forecast` containing the detailed forecast results.
#'
#' @examples
#' # Create a sample preprocessed dataset
#' sample_data <- data.frame(
#'   Month = as.Date('2023-01-01') + months(0:9),
#'   Amount = rnorm(10, mean = 100, sd = 10)
#' )
#' # Perform forecast on the sample data
#' forecast_result <- perform_forecast(sample_data)
#' print(forecast_result$next_month)  # Output: "October 2023" (assuming the last month is September 2023)
#' print(forecast_result$forecasted_value)  # Output: The forecasted amount (e.g., 1234.56)
#'
#' @export
perform_forecast <- function(data) {
  library(forecast)
  library(lubridate)

  fit <- auto.arima(data$Amount)
  forecast_next <- forecast(fit, h = 1)
  next_month <- format(as.Date(data$Month[nrow(data)]) %m+% months(1), "%B %Y")
  forecasted_value <- round(forecast_next$mean, 2)
  list(next_month = next_month, forecasted_value = forecasted_value, forecast_next = forecast_next)
}
