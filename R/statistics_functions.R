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
  res <- mean(abs(real - predicted), na.rm = TRUE)
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

  # check if variable exists
  if (all(is.na(real)) | all(is.na(predicted))) {
    return(NA)
  }
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
  res <- mean(predicted - real, na.rm = TRUE)
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
#' @param meteo_data Data frame with the meterological measures, for Temperature stats
#'
#' @export

statistics_summary <- function(var, models, measured_data,
                               soil = NULL, meteo_data = NULL) {

  # if SWC
  if (var == 'SWC') {

    # get the measured layers
    swc_vars <- c(
      'SWC',
      names(measured_data)[stringr::str_detect(names(measured_data), '^SWC_[0-9]$')]
    )

    # check which models have been performed and get the statistics
    if (!is.null(models[['simple']])) {

      SWC_MAE_simple <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          MAE_calculator(measured_data[[x]],
                         models[['simple']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      SWC_r_sq_simple <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          r_squared_calculator(measured_data[[x]],
                               models[['simple']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      SWC_bias_simple <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          bias_calculator(measured_data[[x]],
                          models[['simple']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      # SWC_MAE_simple <- MAE_calculator(measured_data[['SWC']],
      #                                  models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      # SWC_r_sq_simple <- r_squared_calculator(measured_data[['SWC']],
      #                                         models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      # SWC_bias_simple <- bias_calculator(measured_data[['SWC']],
      #                                    models[['simple']][['W.1']] * soil$Theta_FC[[1]])

    } else {
      # if the model is not performed, the statistics are NAs
      SWC_MAE_simple <- NA
      SWC_r_sq_simple <- NA
      SWC_bias_simple <- NA
    }

    if (!is.null(models[['complex']])) {

      SWC_MAE_complex <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          MAE_calculator(measured_data[[x]],
                         models[['complex']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      SWC_r_sq_complex <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          r_squared_calculator(measured_data[[x]],
                               models[['complex']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      SWC_bias_complex <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          bias_calculator(measured_data[[x]],
                          models[['complex']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      # SWC_MAE_complex <- MAE_calculator(measured_data[['SWC']],
      #                                   models[['complex']][['W.1']] * soil$Theta_FC[[1]])
      # SWC_r_sq_complex <- r_squared_calculator(measured_data[['SWC']],
      #                                          models[['complex']][['W.1']] * soil$Theta_FC[[1]])
      # SWC_bias_complex <- bias_calculator(measured_data[['SWC']],
      #                                     models[['complex']][['W.1']] * soil$Theta_FC[[1]])
    } else {
      SWC_MAE_complex <- NA
      SWC_r_sq_complex <- NA
      SWC_bias_complex <- NA
    }

    if (!is.null(models[['simple']]) & !is.null(models[['complex']])) {

      SWC_MAE_both <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          MAE_calculator(models[['complex']][[paste0('W.', index)]] * soil[['Theta_FC']][index],
                         models[['simple']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      SWC_r_sq_both <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          r_squared_calculator(models[['complex']][[paste0('W.', index)]] * soil[['Theta_FC']][index],
                               models[['simple']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      SWC_bias_both <- vapply(
        swc_vars,
        function(x) {
          index <- which(swc_vars == x)
          bias_calculator(models[['complex']][[paste0('W.', index)]] * soil[['Theta_FC']][index],
                          models[['simple']][[paste0('W.', index)]] * soil[['Theta_FC']][index])
        },
        numeric(1)
      )

      # SWC_MAE_both <- MAE_calculator(models[['complex']][['W.1']] * soil$Theta_FC[[1]],
      #                                models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      # SWC_r_sq_both <- r_squared_calculator(models[['complex']][['W.1']] * soil$Theta_FC[[1]],
      #                                       models[['simple']][['W.1']] * soil$Theta_FC[[1]])
      # SWC_bias_both <- bias_calculator(models[['complex']][['W.1']] * soil$Theta_FC[[1]],
      #                                  models[['simple']][['W.1']] * soil$Theta_FC[[1]])
    } else {
      SWC_MAE_both <- NA
      SWC_r_sq_both <- NA
      SWC_bias_both <- NA
    }

    # build the res object and return it!
    res <- data.frame(
      stringsAsFactors = FALSE,
      Layer = swc_vars,
      R_sq_simple = round(SWC_r_sq_simple, 3),
      Bias_simple = round(SWC_bias_simple, 3),
      MAE_simple = round(SWC_MAE_simple, 3),
      R_sq_complex = round(SWC_r_sq_complex, 3),
      Bias_complex = round(SWC_bias_complex, 3),
      MAE_complex = round(SWC_MAE_complex, 3),
      R_sq_both = round(SWC_r_sq_both, 3),
      Bias_both = round(SWC_bias_both, 3),
      MAE_both = round(SWC_MAE_both, 3)
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

  # if transpiration by cohort
  if (var == 'E_by_Cohort') {

    # check which models have been performed and get the statistics
    if (!is.null(models[['simple']])) {
      # cohorts in the model res
      pred_cohorts_simple <- as.character(
        na.omit(stringr::str_extract(names(models[['simple']]), '^E_.+'))
      )
      # cohorts in measured data
      meas_cohorts_simple <- as.character(
        na.omit(stringr::str_extract(names(measured_data), '^E_.+'))
      )
      # statistics
      Esp_MAE_simple <- purrr::map2_dbl(
        meas_cohorts_simple, pred_cohorts_simple,
        ~ MAE_calculator(measured_data[[.x]], models[['simple']][[.y]])
      )

      Esp_r_sq_simple <- purrr::map2_dbl(
        meas_cohorts_simple, pred_cohorts_simple,
        ~ r_squared_calculator(measured_data[[.x]], models[['simple']][[.y]])
      )

      Esp_bias_simple <- purrr::map2_dbl(
        meas_cohorts_simple, pred_cohorts_simple,
        ~ bias_calculator(measured_data[[.x]], models[['simple']][[.y]])
      )
    } else {
      Esp_MAE_simple <- NA
      Esp_r_sq_simple <- NA
      Esp_bias_simple <- NA
    }

    if (!is.null(models[['complex']])) {
      # cohorts in the model res
      pred_cohorts_complex <- as.character(
        na.omit(stringr::str_extract(names(models[['complex']]), '^E_.+'))
      )
      # cohorts in measured data
      meas_cohorts_complex <- as.character(
        na.omit(stringr::str_extract(names(measured_data), '^E_.+'))
      )
      # statistics
      Esp_MAE_complex <- purrr::map2_dbl(
        meas_cohorts_complex, pred_cohorts_complex,
        ~ MAE_calculator(measured_data[[.x]], models[['complex']][[.y]])
      )

      Esp_r_sq_complex <- purrr::map2_dbl(
        meas_cohorts_complex, pred_cohorts_complex,
        ~ r_squared_calculator(measured_data[[.x]], models[['complex']][[.y]])
      )

      Esp_bias_complex <- purrr::map2_dbl(
        meas_cohorts_complex, pred_cohorts_complex,
        ~ bias_calculator(measured_data[[.x]], models[['complex']][[.y]])
      )
    } else {
      Esp_MAE_complex <- NA
      Esp_r_sq_complex <- NA
      Esp_bias_complex <- NA
    }

    if (!is.null(models[['simple']]) & !is.null(models[['complex']])) {
      # cohorts in the model res
      cohorts_complex <- as.character(
        na.omit(stringr::str_extract(names(models[['complex']]), '^E_.+'))
      )
      # cohorts in measured data
      cohorts_simple <- as.character(
        na.omit(stringr::str_extract(names(models[['simple']]), '^E_.+'))
      )
      # statistics
      Esp_MAE_both <- purrr::map2_dbl(
        cohorts_complex, cohorts_simple,
        ~ MAE_calculator(models[['complex']][[.x]], models[['simple']][[.y]])
      )

      Esp_r_sq_both <- purrr::map2_dbl(
        cohorts_complex, cohorts_simple,
        ~ r_squared_calculator(models[['complex']][[.x]], models[['simple']][[.y]])
      )

      Esp_bias_both <- purrr::map2_dbl(
        cohorts_complex, cohorts_simple,
        ~ bias_calculator(models[['complex']][[.x]], models[['simple']][[.y]])
      )

    } else {
      Esp_MAE_both <- NA
      Esp_r_sq_both <- NA
      Esp_bias_both <- NA
    }

    # names of the cohorts
    if (!is.null(models[['simple']])) {
      coh_names <- pred_cohorts_simple
    } else {
      coh_names <- pred_cohorts_complex
    }

    # build the res object and return it
    res <- list(
      Esp_MAE_simple = Esp_MAE_simple,
      Esp_r_sq_simple = Esp_r_sq_simple,
      Esp_bias_simple = Esp_bias_simple,
      Esp_MAE_complex = Esp_MAE_complex,
      Esp_r_sq_complex = Esp_r_sq_complex,
      Esp_bias_complex = Esp_bias_complex,
      Esp_MAE_both = Esp_MAE_both,
      Esp_r_sq_both = Esp_r_sq_both,
      Esp_bias_both = Esp_bias_both,
      Cohort_name = coh_names
    )

    return(res)
  }

  if (var == 'Temperature') {
    # check if compelx has been performed
    if (!is.null(models[['complex']])) {

      MAE_min_temp <- MAE_calculator(measured_data[['Temp_min']],
                                     models[['complex']][['Tcan_min']])

      MAE_max_temp <- MAE_calculator(measured_data[['Temp_max']],
                                     models[['complex']][['Tcan_max']])

      MAE_mean_temp <- MAE_calculator(measured_data[['Temp_mean']],
                                     models[['complex']][['Tcan_mean']])

      MAE_min_temp_atm <- MAE_calculator(meteo_data[['MinTemperature']],
                                         models[['complex']][['Tcan_min']])

      MAE_max_temp_atm <- MAE_calculator(meteo_data[['MaxTemperature']],
                                         models[['complex']][['Tcan_max']])

      MAE_mean_temp_atm <- MAE_calculator(meteo_data[['MeanTemperature']],
                                          models[['complex']][['Tcan_mean']])

      r_squared_min_temp <- r_squared_calculator(measured_data[['Temp_min']],
                                                 models[['complex']][['Tcan_min']])

      r_squared_max_temp <- r_squared_calculator(measured_data[['Temp_max']],
                                                 models[['complex']][['Tcan_max']])

      r_squared_mean_temp <- r_squared_calculator(measured_data[['Temp_mean']],
                                                  models[['complex']][['Tcan_mean']])

      r_squared_min_temp_atm <- r_squared_calculator(meteo_data[['MinTemperature']],
                                         models[['complex']][['Tcan_min']])

      r_squared_max_temp_atm <- r_squared_calculator(meteo_data[['MaxTemperature']],
                                         models[['complex']][['Tcan_max']])

      r_squared_mean_temp_atm <- r_squared_calculator(meteo_data[['MeanTemperature']],
                                          models[['complex']][['Tcan_mean']])

      bias_min_temp <- bias_calculator(measured_data[['Temp_min']],
                                       models[['complex']][['Tcan_min']])

      bias_max_temp <- bias_calculator(measured_data[['Temp_max']],
                                       models[['complex']][['Tcan_max']])

      bias_mean_temp <- bias_calculator(measured_data[['Temp_mean']],
                                        models[['complex']][['Tcan_mean']])

      bias_min_temp_atm <- bias_calculator(meteo_data[['MinTemperature']],
                                         models[['complex']][['Tcan_min']])

      bias_max_temp_atm <- bias_calculator(meteo_data[['MaxTemperature']],
                                         models[['complex']][['Tcan_max']])

      bias_mean_temp_atm <- bias_calculator(meteo_data[['MeanTemperature']],
                                          models[['complex']][['Tcan_mean']])

      res <- data.frame(
        Temperature = c('Min', 'Max', 'Mean'),
        MAE_measured = c(MAE_min_temp, MAE_max_temp, MAE_mean_temp),
        Rsq_measured = c(r_squared_min_temp, r_squared_max_temp, r_squared_mean_temp),
        Bias_measured = c(bias_min_temp, bias_max_temp, bias_mean_temp),
        MAE_atm = c(MAE_min_temp_atm, MAE_max_temp_atm, MAE_mean_temp_atm),
        Rsq_atm = c(r_squared_min_temp_atm, r_squared_max_temp_atm, r_squared_mean_temp_atm),
        Bias_atm = c(bias_min_temp_atm, bias_max_temp_atm, bias_mean_temp_atm)
      )

      return(res)

    } else {
      # If there is no complex, nothing to do
      stop('The  Simple model generates no temperature data')
    }
  }
}

