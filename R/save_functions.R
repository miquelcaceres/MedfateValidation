#' Save results
#'
#' Function to save model results in txt files, one for each model mode
#'
#' @family Save
#'
#' @param simple_res swb object with the simple model results
#'
#' @param complex_res swb object with the complex model results
#'
#' @param spParams data.frame containing the species parameters used in the model
#'
#' @param site_code character with the site/plot code name
#'
#' @param write boolean indicating if tables must be written in disk, default to
#'   TRUE
#'
#' @importFrom magrittr %>%
#'
#' @export

saveRes <- function(simple_res = NULL, complex_res = NULL, spParams = NULL,
                    site_code, write = TRUE) {

  # empty list to store the model data frame results for later use
  # (plots and statistics)
  models_dfs <- list(simple = NULL, complex = NULL)

  # simple model
  if (is.null(simple_res)) {
    print('Unable to save simple model results: No simple model performed')
  } else {

    # dates
    Dates <- rownames(simple_res[['DailyBalance']])

    # total plant transpiration
    Eplanttot <- simple_res[['DailyBalance']][['Eplanttot']]

    # sp's transpiration
    SP_transp <- as.data.frame(simple_res[['PlantTranspiration']])
    names(SP_transp) <- paste0('E_', names(SP_transp))

    # soilwater content
    soilWater <- simple_res[["SoilWaterBalance"]] %>%
      dplyr::select(dplyr::starts_with('W.'))

    # final data.frame to save
    simple_to_save <- cbind(
      Dates, Eplanttot, SP_transp, soilWater
    )

    # save
    file_name <- file.path('Output', packageVersion('medfate')[[1]],
                           site_code,
                           paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                  '_', site_code, '_',
                                  'simple_model_res.txt'))

    if (write) {
      write.table(simple_to_save, file_name, row.names = FALSE,
                  col.names = TRUE, fileEncoding = 'UTF-8')
    }

    models_dfs[['simple']] <- simple_to_save

    # indexes for later
    indexes <- simple_res[['cohorts']][['SP']]
  }

  # complex_model
  if (is.null(complex_res)) {
    print('Unable to save complex model results: No complex model performed')
  } else {

    # dates
    Dates <- rownames(complex_res[['DailyBalance']])

    # total plant transpiration
    Eplanttot <- complex_res[['DailyBalance']][['Eplanttot']]

    # sp's transpiration
    SP_transp <- as.data.frame(complex_res[['PlantTranspiration']])
    names(SP_transp) <- paste0('E_', names(SP_transp))

    # soilwater content
    soilWater <- complex_res[["SoilWaterBalance"]] %>%
      dplyr::select(dplyr::starts_with('W.'))

    # temperature
    Temperature <- complex_res[['Temperature']]

    # final data.frame to save
    complex_to_save <- cbind(
      Dates, Eplanttot, SP_transp, soilWater, Temperature
    )

    # save
    file_name <- file.path('Output', packageVersion('medfate')[[1]],
                           site_code,
                           paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                  '_', site_code, '_',
                                  'complex_model_res.txt'))

    if (write) {
      write.table(complex_to_save, file_name, row.names = FALSE,
                  col.names = TRUE, fileEncoding = 'UTF-8')
    }

    models_dfs[['complex']] <- complex_to_save

    # indexes for later
    indexes <- complex_res[['cohorts']][['SP']]
  }

  # spParams table
  params_file <- file.path('Output', packageVersion('medfate')[[1]],
                           site_code,
                           paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                  '_', site_code, '_',
                                  'SpParams.txt'))

  # filtering to obtain only the ones we want
  spParams <- spParams[spParams$SpIndex %in% indexes, ]

  if (write) {
    write.table(spParams, params_file, row.names = FALSE, col.names = TRUE,
                fileEncoding = 'UTF-8')
  }

  # returning the lis with the models dfs to later use
  return(models_dfs)
}
