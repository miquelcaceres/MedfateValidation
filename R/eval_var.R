
#' Statistics summary for a given variable
#'
#' @param x object of \code{\link{spwb}}
#'
#' @param var Character with the variable name
#'
#' @param measured_data Data frame with the "real" data
#'
#' @param trunc Numeric vector indicating the subset of data to be compared
#'
#' @export
eval_var <- function(x, var, measured_data, trunc = NULL) {

  thetaFC = medfate::soil.thetaFC(x$soilInput)
  SoilRes = x$Soil
  # if SWC
  if (var == 'SWC') {

    n = length(SoilRes[["W.1"]])
    # get the measured layers
    swc_vars <- c(
      'SWC',
      names(measured_data)[stringr::str_detect(names(measured_data), '^SWC_[0-9]$')]
    )

    SWC_MAE <- vapply(
      swc_vars,
      function(x) {
        index <- which(swc_vars == x)
        MAE_calculator(measured_data[[x]],
                       SoilRes[[paste0('W.', index)]] * thetaFC[index], relative=F)
      },
      numeric(1)
    )
    SWC_MAE_rel <- vapply(
      swc_vars,
      function(x) {
        index <- which(swc_vars == x)
        MAE_calculator(measured_data[[x]],
                       SoilRes[[paste0('W.', index)]] * thetaFC[index], relative=T)
      },
      numeric(1)
    )

    SWC_r_sq <- vapply(
      swc_vars,
      function(x) {
        index <- which(swc_vars == x)
        r_squared_calculator(measured_data[[x]],
                             SoilRes[[paste0('W.', index)]] * thetaFC[index])
      },
      numeric(1)
    )

    SWC_bias <- vapply(
      swc_vars,
      function(x) {
        index <- which(swc_vars == x)
        bias_calculator(measured_data[[x]],
                        SoilRes[[paste0('W.', index)]] * thetaFC[index], relative=F)
      },
      numeric(1)
    )

    SWC_bias_rel <- vapply(
      swc_vars,
      function(x) {
        index <- which(swc_vars == x)
        bias_calculator(measured_data[[x]],
                        SoilRes[[paste0('W.', index)]] * thetaFC[index], relative=T)
      },
      numeric(1)
    )
    # build the res object and return it!
    res <- data.frame(
      stringsAsFactors = FALSE,
      Layer = swc_vars,
      n = n,
      R_sq = SWC_r_sq,
      Bias = SWC_bias,
      Bias_rel = SWC_bias_rel,
      MAE = SWC_MAE,
      MAE_rel = SWC_MAE_rel
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
      MAE_simple = Eplanttot_MAE_simple,
      r_sq_simple = Eplanttot_r_sq_simple,
      bias_simple = Eplanttot_bias_simple,
      MAE_complex = Eplanttot_MAE_complex,
      r_sq_complex = Eplanttot_r_sq_complex,
      bias_complex = Eplanttot_bias_complex,
      MAE_both = Eplanttot_MAE_both,
      r_sq_both = Eplanttot_r_sq_both,
      bias_both = Eplanttot_bias_both
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

      MAE_min_temp <- MAE_calculator(measured_data[['Temp_min']][trunc],
                                     models[['complex']][['Tcan_min']][trunc])

      MAE_max_temp <- MAE_calculator(measured_data[['Temp_max']][trunc],
                                     models[['complex']][['Tcan_max']][trunc])

      MAE_mean_temp <- MAE_calculator(measured_data[['Temp_mean']][trunc],
                                      models[['complex']][['Tcan_mean']][trunc])

      MAE_min_temp_atm <- MAE_calculator(measured_data[['Temp_min']][trunc],
                                         meteo_data[['MinTemperature']][trunc])

      MAE_max_temp_atm <- MAE_calculator(measured_data[['Temp_max']][trunc],
                                         meteo_data[['MaxTemperature']][trunc])

      MAE_mean_temp_atm <- MAE_calculator(measured_data[['Temp_mean']][trunc],
                                          meteo_data[['MeanTemperature']][trunc])

      r_squared_min_temp <- r_squared_calculator(measured_data[['Temp_min']][trunc],
                                                 models[['complex']][['Tcan_min']][trunc])

      r_squared_max_temp <- r_squared_calculator(measured_data[['Temp_max']][trunc],
                                                 models[['complex']][['Tcan_max']][trunc])

      r_squared_mean_temp <- r_squared_calculator(measured_data[['Temp_mean']][trunc],
                                                  models[['complex']][['Tcan_mean']][trunc])

      r_squared_min_temp_atm <- r_squared_calculator(measured_data[['Temp_min']][trunc],
                                                     meteo_data[['MinTemperature']][trunc])

      r_squared_max_temp_atm <- r_squared_calculator(measured_data[['Temp_max']][trunc],
                                                     meteo_data[['MaxTemperature']][trunc])

      r_squared_mean_temp_atm <- r_squared_calculator(measured_data[['Temp_mean']][trunc],
                                                      meteo_data[['MeanTemperature']][trunc])

      bias_min_temp <- bias_calculator(measured_data[['Temp_min']][trunc],
                                       models[['complex']][['Tcan_min']][trunc])

      bias_max_temp <- bias_calculator(measured_data[['Temp_max']][trunc],
                                       models[['complex']][['Tcan_max']][trunc])

      bias_mean_temp <- bias_calculator(measured_data[['Temp_mean']][trunc],
                                        models[['complex']][['Tcan_mean']][trunc])

      bias_min_temp_atm <- bias_calculator(measured_data[['Temp_min']][trunc],
                                           meteo_data[['MinTemperature']][trunc])

      bias_max_temp_atm <- bias_calculator(measured_data[['Temp_max']][trunc],
                                           meteo_data[['MaxTemperature']][trunc])

      bias_mean_temp_atm <- bias_calculator(measured_data[['Temp_mean']][trunc],
                                            meteo_data[['MeanTemperature']][trunc])

      res <- data.frame(
        Temperature = c('Min', 'Max', 'Mean'),
        MAE_complex = c(MAE_min_temp, MAE_max_temp, MAE_mean_temp),
        Rsq_complex = c(r_squared_min_temp, r_squared_max_temp, r_squared_mean_temp),
        Bias_complex = c(bias_min_temp, bias_max_temp, bias_mean_temp),
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
