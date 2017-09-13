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
  res <- mean(predicted - real)
  # return it
  return(res)
}

#' Statistics summary for a given variable
#'
#' This function calculates the different statistics for a given variable
#' accounting for the models performed (simple, complex, both)
#'
#' @param var Character with the variable name
#'
#' @param models list with the dataframes of models results, typically the
#'   result of \code{\link{saveRes}}
#'
#' @param measured_data Data frame with the "real" data
#'
#' @param soil Soil object needed to calculate SWC from W
#'
#' @export

statistics_summary <- function(var, models, measured_data, soil) {

  # if SWC
  if (var == 'SWC') {

    # check which models have been performed and get the statistics
    if (!is.null(models[['simple']])) {
      SWC_MAE_simple <- MAE_calculator(measured_data[['SWC']],
                                       models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      SWC_r_sq_simple <- r_squared_calculator(measured_data[['SWC']],
                                              models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      SWC_bias_simple <- bias_calculator(measured_data[['SWC']],
                                         models[['simple']][['W.1']] * soil$Theta_FC[[1]])
    } else {
      # if the model is not performed, the statistics are NAs
      SWC_MAE_simple <- NA
      SWC_r_sq_simple <- NA
      SWC_bias_simple <- NA
    }

    if (!is.null(models[['complex']])) {
      SWC_MAE_complex <- MAE_calculator(measured_data[['SWC']],
                                        models[['complex']][['W.1']] * soil$Theta_FC[[1]])
      SWC_r_sq_complex <- r_squared_calculator(measured_data[['SWC']],
                                               models[['complex']][['W.1']] * soil$Theta_FC[[1]])
      SWC_bias_complex <- bias_calculator(measured_data[['SWC']],
                                          models[['complex']][['W.1']] * soil$Theta_FC[[1]])
    } else {
      SWC_MAE_complex <- NA
      SWC_r_sq_complex <- NA
      SWC_bias_complex <- NA
    }

    if (!is.null(models[['simple']]) & !is.null(models[['complex']])) {
      SWC_MAE_both <- MAE_calculator(models[['complex']][['W.1']] * soil$Theta_FC[[1]],
                                     models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      SWC_r_sq_both <- r_squared_calculator(models[['complex']][['W.1']] * soil$Theta_FC[[1]],
                                            models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      SWC_bias_both <- bias_calculator(models[['complex']][['W.1']] * soil$Theta_FC[[1]],
                                       models[['simple']][['W.1']] * soil$Theta_FC[[1]])
    } else {
      SWC_MAE_both <- NA
      SWC_r_sq_both <- NA
      SWC_bias_both <- NA
    }

    # build the res object and return it!
    res <- list(
      SWC_MAE_simple = SWC_MAE_simple,
      SWC_r_sq_simple = SWC_r_sq_simple,
      SWC_bias_simple = SWC_bias_simple,
      SWC_MAE_complex = SWC_MAE_complex,
      SWC_r_sq_complex = SWC_r_sq_complex,
      SWC_bias_complex = SWC_bias_complex,
      SWC_MAE_both = SWC_MAE_both,
      SWC_r_sq_both = SWC_r_sq_both,
      SWC_bias_both = SWC_bias_both
    )

    return(res)
  }

  # if Eplanttot
  if (var == 'Eplanttot') {

    # check which models have been performed and get the statistics
    if (!is.null(models[['simple']])) {
      Eplanttot_MAE_simple <- MAE_calculator(measured_data[['Eplanttot']],
                                             models[['simple']][['Eplanttot']])
      Eplanttot_r_sq_simple <- r_squared_calculator(measured_data[['Eplanttot']],
                                                    models[['simple']][['Eplanttot']])
      Eplanttot_bias_simple <- bias_calculator(measured_data[['Eplanttot']],
                                               models[['simple']][['Eplanttot']])
    } else {
      # if the model is not performed, the statistics are NAs
      Eplanttot_MAE_simple <- NA
      Eplanttot_r_sq_simple <- NA
      Eplanttot_bias_simple <- NA
    }

    if (!is.null(models[['complex']])) {
      Eplanttot_MAE_complex <- MAE_calculator(measured_data[['Eplanttot']],
                                              models[['complex']][['Eplanttot']])
      Eplanttot_r_sq_complex <- r_squared_calculator(measured_data[['Eplanttot']],
                                                     models[['complex']][['Eplanttot']])
      Eplanttot_bias_complex <- bias_calculator(measured_data[['Eplanttot']],
                                                models[['complex']][['Eplanttot']])
    } else {
      Eplanttot_MAE_complex <- NA
      Eplanttot_r_sq_complex <- NA
      Eplanttot_bias_complex <- NA
    }

    if (!is.null(models[['simple']]) & !is.null(models[['complex']])) {
      Eplanttot_MAE_both <- MAE_calculator(models[['complex']][['Eplanttot']],
                                           models[['simple']][['Eplanttot']])
      Eplanttot_r_sq_both <- r_squared_calculator(models[['complex']][['Eplanttot']],
                                                  models[['simple']][['Eplanttot']])
      Eplanttot_bias_both <- bias_calculator(models[['complex']][['Eplanttot']],
                                             models[['simple']][['Eplanttot']])
    } else {
      Eplanttot_MAE_both <- NA
      Eplanttot_r_sq_both <- NA
      Eplanttot_bias_both <- NA
    }

    # build the res object and return it!
    res <- list(
      Eplanttot_MAE_simple = Eplanttot_MAE_simple,
      Eplanttot_r_sq_simple = Eplanttot_r_sq_simple,
      Eplanttot_bias_simple = Eplanttot_bias_simple,
      Eplanttot_MAE_complex = Eplanttot_MAE_complex,
      Eplanttot_r_sq_complex = Eplanttot_r_sq_complex,
      Eplanttot_bias_complex = Eplanttot_bias_complex,
      Eplanttot_MAE_both = Eplanttot_MAE_both,
      Eplanttot_r_sq_both = Eplanttot_r_sq_both,
      Eplanttot_bias_both = Eplanttot_bias_both
    )

    return(res)
  }

  # TODO transpiration by cohort

}
