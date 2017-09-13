#' MAE calculation
#'
#' Simple function to calculate MAE values
#'
#' Usually MAE values are calculated comparing the predicted values of one
#' model with the real values measured. But it can also be calculated to compare
#' between simple and complex models
#'
#' @param real Numeric vector with the "real" values
#'
#' @param predicted Numeric vector with the "predicted" values
#'
#' @export

MAE_calculator <- function(real, predicted) {
  # calculate MAE
  res <- mean(abs(real - predicted))
  # return it
  return(res)
}

#' r squared calculation
#'
#' Simple function to calculate r squared value
#'
#' Usually r squared values are calculated for the correlation between real
#' values and predicted values, but also can be calculated between simple and
#' complex model results
#'
#' @param real Numeric vector with the "real" values
#'
#' @param predicted Numeric vector with the "predicted" values
#'
#' @export

r_squared_calculator <- function(real, predicted) {
  # build the model
  model_res <- lm(real ~ predicted)
  # extract the r squared
  res <- summary(model_res)$r.squared
  # return it
  return(res)
}

#' bias calculation
#'
#' Simple function ti calculate the bias value
#'
#' Usually bias values are calculated between the real values and the predicted
#' values, but also can be calculated between simple and complex model results
#'
#' @param real Numeric vector with the "real" values
#'
#' @param predicted Numeric vector with the "predicted" values
#'
#' @export

bias_calculator <- function(real, predicted) {
  # calculate bias
  res <- mean(real - predicted)
  # return it
  return(res)
}
