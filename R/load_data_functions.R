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
  tree_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                          stringsAsFactors = FALSE)
  # return data frame
  return(tree_data)
}

#' @describeIn load_treeData
#'
#' @export

load_shrubData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_shrubData.txt'))

  # check if file exists, if not return NULL (in order to avoid errors in the
  # workflow)
  if (!file.exists(location)) {
    return(NULL)
  }

  # load data
  shrub_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                           stringsAsFactors = FALSE)
  # return data frame
  return(shrub_data)
}

#' @describeIn load_treeData
#'
#' @export

load_seedData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_seedData.txt'))

  # check if file exists, if not return NULL (in order to avoid errors in the
  # workflow)
  if (!file.exists(location)) {
    return(NULL)
  }

  # load data
  seed_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                          stringsAsFactors = FALSE)
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
  misc_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                          stringsAsFactors = FALSE)
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
  meteo_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                           stringsAsFactors = FALSE)
  # rownames
  rownames(meteo_data) <- meteo_data[['Date']]
  # get rid of Date column
  meteo_data$Date <- NULL
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
  soil_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                          stringsAsFactors = FALSE)
  # return data frame
  return(soil_data)
}

#' @describeIn load_treeData
#'
#' @export

load_soilDataUnilayer <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_soilDataUnilayer.txt'))
  # load data
  soil_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                          stringsAsFactors = FALSE)
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
  measured_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                              stringsAsFactors = FALSE)
  # return data frame
  return(measured_data)
}

#' @describeIn load_treeData
#'
#' @export

load_terrainData <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_terrainData.txt'))

  # check if file exists, if not return NULL (in order to avoid errors in the
  # workflow)
  if (!file.exists(location)) {
    return(NULL)
  }

  # load data
  terrain_data <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                             stringsAsFactors = FALSE)
  # return data frame
  return(terrain_data)
}

#' @describeIn load_treeData
#'
#' @export

load_customParams <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_customParams.txt'))

  # check if file exists, if not return NULL (in order to avoid errors in the
  # workflow)
  if (!file.exists(location)) {
    return(NULL)
  }

  # load data
  custom_params <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                              stringsAsFactors = FALSE)
  # return data frame
  return(custom_params)
}


#' @describeIn load_treeData
#'
#' @export

load_remarks <- function(site) {
  # file route
  location <- file.path('Sites_data', site, paste0(site, '_remarks.txt'))

  # check if file exists, if not return NULL (in order to avoid errors in the
  # workflow)
  if (!file.exists(location)) {
    return(NULL)
  }

  # load data
  remarks <- read.table(location, header = TRUE, sep = '\t', dec = '.',
                        stringsAsFactors = FALSE)
  # return data frame
  return(remarks)
}

#' Load all the RData's generated in the validation process
#'
#' This function will be used in the general report with all sites data
#'
#' @param type Character indicating which data to load ("Global", "Temperature"
#'   and "Definitive")
#'
#' @export

load_rdatas <- function(type = 'Global') {

  if (type == "Global") {
    # prepare the environments where we will store the RData's objects
    genv <- list(
      "210PSYL" = new.env(),
      "32PSYL" = new.env(),
      "302PPIN" = new.env(),
      "307CPPIN" = new.env(),
      "CANBALASC" = new.env(),
      "ESPALTARM" = new.env(),
      "ESPRIN" = new.env(),
      "FRAHES" = new.env(),
      "FRAPUE" = new.env(),
      "ISRYAT" = new.env(),
      "PRADES" = new.env(),
      "PVALLCEBRE" = new.env(),
      "QVALLCEBRE" = new.env()
    )

    # now time stamp, to get the diff and the newest RDatas for each site
    stamp <- Sys.time()

    # sites names
    sites <- names(genv)

    for (site in sites) {
      file_creation_info <- file.info(
        list.files(file.path('Output', packageVersion('medfate')[[1]], site),
                   pattern = 'RData', full.names = TRUE)
      )

      file_index <- which(as.numeric(stamp - file_creation_info$mtime) ==
                            as.numeric(min(stamp - file_creation_info$mtime)))

      file_name <- row.names(file_creation_info)[file_index]

      if (length(file_name) == 0) {
        next()
      } else {
        load(file_name, envir = genv[[site]])
      }
    }

    return(genv)
  }

  if (type == 'Temperature') {
    # prepare the environments where we will store the RData's objects
    genv <- list(
      "Plot_1" = new.env(),
      "Plot_11" = new.env(),
      "Plot_12" = new.env(),
      "Plot_13" = new.env(),
      "Plot_14" = new.env(),
      "Plot_18" = new.env(),
      "Plot_2" = new.env(),
      "Plot_24" = new.env(),
      "Plot_3" = new.env(),
      "Plot_36" = new.env(),
      "Plot_39" = new.env(),
      "Plot_40" = new.env(),
      "Plot_6" = new.env(),
      "Plot_7" = new.env(),
      "Plot_9" = new.env()
    )

    # now time stamp, to get the diff and the newest RDatas for each site
    stamp <- Sys.time()

    # sites names
    sites <- names(genv)

    for (site in sites) {
      file_creation_info <- file.info(
        list.files(file.path('Output', packageVersion('medfate')[[1]], site),
                   pattern = 'RData', full.names = TRUE)
      )

      file_index <- which(as.numeric(stamp - file_creation_info$mtime) ==
                            as.numeric(min(stamp - file_creation_info$mtime)))

      file_name <- row.names(file_creation_info)[file_index]

      if (length(file_name) == 0) {
        next()
      } else {
        load(file_name, envir = genv[[site]])
      }
    }

    return(genv)
  }

  if (type == 'Definitive') {
    # prepare the environments where we will store the RData's objects
    genv <- list(
      "CANBALASC" = new.env(),
      "ESPALTARM" = new.env(),
      "FRAPUE" = new.env(),
      "ISRYAT" = new.env(),
      "PRADES" = new.env(),
      "PVALLCEBRE" = new.env()
    )

    # now time stamp, to get the diff and the newest RDatas for each site
    stamp <- Sys.time()

    # sites names
    sites <- names(genv)

    for (site in sites) {
      file_creation_info <- file.info(
        list.files(file.path('Output', packageVersion('medfate')[[1]], site),
                   pattern = 'RData', full.names = TRUE)
      )

      file_index <- which(as.numeric(stamp - file_creation_info$mtime) ==
                            as.numeric(min(stamp - file_creation_info$mtime)))

      file_name <- row.names(file_creation_info)[file_index]

      if (length(file_name) == 0) {
        next()
      } else {
        load(file_name, envir = genv[[site]])
      }
    }

    return(genv)
  }

}
