#' Building model objects functions
#'
#' This functions create the objects needed for the model runs
#'
#' @family ModelObjects
#'
#' @param treeData tree data dataframe
#' @param shrubData shrub data dataframe
#' @param seedData seed bank data dataframe
#' @param miscData miscelaneous data dataframe
#'
#' @export

buildForest <- function(treeData = NULL, shrubData = NULL,
                        seedData = NULL, miscData = NULL) {

  # empty forest object
  empty_forest <- medfate::emptyforest()

  # adding tree data
  if (!is.null(treeData)) {
    empty_forest$treeData <- treeData
  }

  # adding shrub data
  if (!is.null(shrubData)) {
    empty_forest$shrubData <- shrubData
  }

  # adding seed data
  if (!is.null(seedData)) {
    empty_forest$seedBank <- seedData
  }

  # adding misc data
  if (!is.null(miscData)) {
    empty_forest$ID <- miscData$ID
    empty_forest$patchsize <- miscData$patchsize
    empty_forest$herbCover <- miscData$herbCover
    empty_forest$herbHeight <- miscData$herbHeight
  }

  # return the forest object
  return(empty_forest)
}

#' @describeIn buildForest
#'
#' @param soilData soil data dataframe
#'
#' @export

buildSoil <- function(soilData) {

  # obtain the number of soil layers
  layers <- nrow(soilData)

  # create a default soil list
  soil_list <- medfate::defaultSoilParams(n = layers)

  # subsitute default params with soilData params
  soil_list[['widths']] <- soilData[['widths']]
  soil_list[['clay']] <- soilData[['clay']]
  soil_list[['sand']] <- soilData[['sand']]
  soil_list[['macro']] <- soilData[['macro']]
  soil_list[['rfc']] <- soilData[['rfc']]
  soil_list[['Gsoil']] <- soilData[['Gsoil']][1]
  soil_list[['Ksoil']] <- soilData[['Ksoil']][1]

  # create the soil object
  soilObject <- medfate::soil(soil_list, soilData[['W']])

  # return the soil object
  return(soilObject)
}
