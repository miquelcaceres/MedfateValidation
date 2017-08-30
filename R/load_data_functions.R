#' Loading data functions
#'
#' This functions search and load the data for the corresponding site
#'
#' @family Load
#'
#' @param site Character indicating site code to load.
#'
#' @export

load_treeData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_treeData.txt'))
  # load data
  tree_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(tree_data)
}

#' @describeIn load_treeData
#'
#' @export

load_shrubData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_shrubData.txt'))
  # load data
  shrub_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(shrub_data)
}

#' @describeIn load_treeData
#'
#' @export

load_seedData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_seedData.txt'))
  # load data
  seed_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(seed_data)
}

#' @describeIn load_treeData
#'
#' @export

load_miscData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_miscData.txt'))
  # load data
  misc_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(misc_data)
}

#' @describeIn load_treeData
#'
#' @export

load_meteoData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_meteoData.txt'))
  # load data
  meteo_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(meteo_data)
}

#' @describeIn load_treeData
#'
#' @export

load_soilData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_soilData.txt'))
  # load data
  soil_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(soil_data)
}

#' @describeIn load_treeData
#'
#' @export

load_measuredData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_measuredData.txt'))
  # load data
  measured_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(measured_data)
}

#' @describeIn load_treeData
#'
#' @export

load_terrainData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_terrainData.txt'))
  # load data
  terrain_data <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(terrain_data)
}

#' @describeIn load_treeData
#'
#' @export

load_customParams <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_customParams.txt'))
  # load data
  custom_params <- read.table(location, header = TRUE, sep = '\t', dec = '.')
  # return data frame
  return(custom_params)
}
