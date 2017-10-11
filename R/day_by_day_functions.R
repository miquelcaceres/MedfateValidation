#' Transpiration day by day
#'
#' Getting the transpiration calculated day by day using measured SWC values
#'
#' @param input swb.input object
#'
#' @param meteoData Data frame with the meteorological data
#'
#' @param soilData Data frame with the soil info data in unilayer format
#'
#' @param terrainData Data frame with the terrain info data
#'
#' @param measuredData Data frame with the measured data
#'
#' @export

transp_day_by_day <- function(input, meteoData, soilData,
                              terrainData, measuredData) {

  # control values
  n_coh <- nrow(input[['cohorts']])
  dates <- rownames(meteoData)

  # empty objects to store res
  e_coh <- array(dim = c(length(dates), n_coh),
                 dimnames = list(Dates = dates,
                                 Cohorts = paste0('E_', rownames(input[['cohorts']]))))

  # loop by dates

  for (i in 1:length(dates)) {

    # check if we have SWC measured
    if (is.na(measuredData[i, 'SWC'])) {
      # fill the res objects by cohort
      for (j in 1:n_coh) {
        e_coh[i,j] <- NA
      }
    } else {
      # copy of input to avoid modifications
      loop_input <- input

      # generate the soil object replacing W with the measured value
      soilData[['W']] <- measuredData[i, 'SWC']/soilData[['FC']]
      soil_object <- buildSoil(soilData)

      # swb.day
      temp_res <- medfate::swb.day(
        loop_input, soil_object, dates[[i]], meteoData[i, 'DOY'],
        meteoData[i, 'MinTemperature'], meteoData[i, 'MaxTemperature'],
        meteoData[i, 'MinRelativeHumidity'], meteoData[i, 'MaxRelativeHumidity'],
        meteoData[i, 'Radiation'], meteoData[i, 'WindSpeed'],
        terrainData[['latitude']], terrainData[['elevation']],
        terrainData[['slope']], terrainData[['aspect']],
        meteoData[i, 'Precipitation']
      )

      # fill the res objects by cohort
      for (j in 1:n_coh) {
        e_coh[i,j] <- temp_res[['EplantCoh']][j]
      }
    }

  }

  # build the final res
  res <- as.data.frame(e_coh)
  etot <- rowSums(res)
  res <- res %>%
    dplyr::mutate(Dates = dates, Eplanttot = etot) %>%
    dplyr::select(Dates, Eplanttot, dplyr::everything())

  # return the res
  return(res)
}
