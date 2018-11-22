#' Evaluates medfate for one site
#'
#' @param code String with the code of the site to be processed
#' @param transpMode Either 'simple', 'complex' or 'both'
#' @param plot A boolean flag indicating wether plots should be produced
#'
#' @return A list with evaluation results
#' @export
eval_site<-function(code, transpMode="simple",
                    control = medfate::defaultControl(),
                    plot = FALSE) {

  # library(dplyr)

  # raw data
  treeData <- load_treeData(code)
  shrubData <- load_shrubData(code)
  seedData <- load_seedData(code)
  customParams <- load_customParams(code)
  measuredData <- load_measuredData(code)
  meteoData <- load_meteoData(code)
  miscData <- load_miscData(code)
  soilData <- load_soilData(code)
  terrainData <- load_terrainData(code)
  remarks <- load_remarks(code)

  # model objects
  forest_object <- buildForest(treeData, shrubData, seedData, miscData)


  #Load parameters
  data("SpParamsMED", package="medfate", envir = environment())

  # modify
  sp_params <- SpParams_mod(SpParamsMED, customParams)


  # Warning for custom FC
  if (!all(is.na((soilData[['FC']])))) {
    warning('Custom FC values has been provided: ', soilData[['FC']][1], ' ...')
  }

  # dinamic fig size for soilwater
  swc_vars <- c(
    'SWC',
    names(measuredData)[stringr::str_detect(names(measuredData), '^SWC_[0-9]$')]
  )

  simple_input = NULL
  complex_input = NULL
  res_simple = NULL
  res_complex = NULL

  if(transpMode == "simple" || transpMode == "both") {
    # control object
    control$transpirationMode <- "Simple"

    # init soil object
    soil_object <- medfate::soil(soilData)

    # input object
    simple_input <- medfate::forest2spwbInput(forest_object, soil_object,
                                              sp_params, control)

    # input modifications (if any)
    simple_input <- inputMod(simple_input, customParams)

    # model run
    res_simple <- medfate::spwb(simple_input, soil_object, meteoData,elevation= terrainData$elevation)
    if(plot) {
      # config layout
      par(mfrow = c(3,2))
      # P/PET plot
      plot(res_simple, yearAxis = TRUE)
      # ET plot
      plot(res_simple, type = 'Evapotranspiration', yearAxis = TRUE)
      # SWC plot
      plot(res_simple, type = 'SoilTheta')
      # Stress plot
      plot(res_simple, type = 'PlantStress', yearAxis = TRUE)
      # Short wave radiation leaf
      plot(res_simple, type = 'PlantPhotosynthesisLeaf')
      # plant transpiration by leaf
      plot(res_simple, type = 'PlantTranspirationLeaf')
      # reset layout
      par(mfrow = c(1,1))
    }
  }

  if(transpMode=="complex" || transpMode == "both") {
    control$transpirationMode <- 'Complex'

    # input object
    # we also rebuild the soil_object to avoid using W data from the previous
    # runs
    # init soil object
    soil_object <- medfate::soil(soilData)

    complex_input <- medfate::forest2spwbInput(forest_object, soil_object,
                                               sp_params, control)

    # input modifications (if any)
    complex_input <- inputMod(complex_input, customParams)

    # model run
    res_complex <- medfate::spwb(complex_input, soil_object, meteoData,
                                 latitude = terrainData$latitude, elevation= terrainData$elevation,
                                 slope= terrainData$slope, aspect = terrainData$aspect)
    if(plot) {
      # config layout
      par(mfrow = c(3,2))
      # P/PET plot
      plot(res_complex, yearAxis = TRUE)
      # ET plot
      plot(res_complex, type = 'Evapotranspiration', yearAxis = TRUE)
      # SWC plot
      plot(res_complex, type = 'SoilTheta')
      # Stress plot
      plot(res_complex, type = 'PlantStress', yearAxis = TRUE)
      # Short wave radiation leaf
      plot(res_complex, type = 'PlantPhotosynthesisLeaf')
      # plant transpiration by leaf
      plot(res_complex, type = 'PlantTranspirationLeaf')
      # reset layout
      par(mfrow = c(1,1))
    }
  }


  e_meas <- measuredData %>%
    dplyr::summarize_all(dplyr::funs(all(is.na(.)))) %>%
    as.logical()

  e_measured_coh <- names(measuredData)[!e_meas]


  models_dfs <- saveRes(simple_res = res_simple, complex_res = res_complex,
                        measured_vars = e_measured_coh,
                        spParams = sp_params,
                        site_code = code, write=FALSE)
  SWC_stats <- statistics_summary('SWC', models_dfs, measuredData,
                                  soil_object)
  Eplanttot_stats <- statistics_summary('Eplanttot', models_dfs, measuredData,
                                        soil_object)


  Ecohorts_stats <- statistics_summary('E_by_Cohort', models_dfs, measuredData,
                                       soil_object)
  E_stats_df <- rbind(
    round(Ecohorts_stats[["Esp_MAE_simple"]], 5),
    round(Ecohorts_stats[["Esp_r_sq_simple"]], 5),
    round(Ecohorts_stats[["Esp_bias_simple"]], 5),
    round(Ecohorts_stats[["Esp_MAE_complex"]], 5),
    round(Ecohorts_stats[["Esp_r_sq_complex"]], 5),
    round(Ecohorts_stats[["Esp_bias_complex"]], 5),
    round(Ecohorts_stats[["Esp_MAE_both"]], 5),
    round(Ecohorts_stats[["Esp_r_sq_both"]], 5),
    round(Ecohorts_stats[["Esp_bias_both"]], 5)
  ) %>%
    cbind(
      c('MAE Simple vs. Measured',
        'rsq Simple vs. Measured',
        'Bias Simple vs. Measured',
        'MAE Complex vs. Measured',
        'rsq Complex vs. Measured',
        'Bias Complex vs. Measured',
        'MAE Simple vs. Complex',
        'rsq Simple vs. Complex',
        'Bias Simple vs. Complex'),
      .
    ) %>%
    as.data.frame()
  names(E_stats_df) <- c("Statistics", Ecohorts_stats[['Cohort_name']])

  E_stats_df$Etot = Eplanttot_stats

  if(plot) {
    print(plot_res_gg('SWC', models_dfs, soil_object, measuredData, transpMode))
    print(plot_res_gg('Eplanttot', models_dfs, soil_object, measuredData, transpMode))
    print(plot_res_gg('E_by_Cohort', models_dfs, soil_object, measuredData,transpMode))
  }

  eval = list(SWC = SWC_stats, E = E_stats_df)
  res = list(mode = transpMode,
             control = control,
             forest = forest_object,
             soil = soil_object,
             input_simple = simple_input,
             input_complex = complex_input,
             spwb_simple = res_simple,
             spwb_complex = res_complex,
             models_dfs = models_dfs,
             measured = measuredData,
             eval = eval)
  return(res)
}
