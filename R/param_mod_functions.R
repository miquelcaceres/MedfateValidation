#' Modification of SpParams
#'
#' This function modifies the SpParams table with the values supplied in the
#' customParams.txt file
#'
#' @family CustomMods
#'
#' @param SpParams Data frame with the species parameters, usually obtained
#'   from the \code{\link{medfate}} package (\code{data('SpParamsMED')})
#'
#' @param customParams Data frame with the custom parameters info, usually
#'   the result of \code{\link{load_customParams}}
#'
#' @export

SpParams_mod <- function(SpParams, customParams) {

  # check if customParams exists, if not return SpParams without modification
  if (is.null(customParams)) {
    return(SpParams)
  }

  # get the names of the custom params and the SpParams
  custom <- names(customParams)
  sp_par <- names(SpParams)

  # iterate between the custom params exisiting in SpParams
  for (param in custom) {

    # check if the param exists in SpParams
    if (param %in% sp_par) {

      # iterate by species, in case same variable has different values by sp
      for (sp in customParams[['SpIndex']]) {

        # subsitute it!
        SpParams[which(SpParams[['SpIndex']] == sp), param] <- customParams[which(customParams[['SpIndex']] == sp), param]
      }
    }
  }

  # return the new SpParams
  return(SpParams)
}

#' Modification of swb input object
#'
#' This function looks for and modifies input parameters with the values
#' supplied in the customParams.txt file
#'
#' @family CustomMods
#'
#' @param swbInput An object of class \code{swbInput} as obtained from
#'   \code{\link{forest2swbInput}} or \code{\link{swbInput}} functions from
#'   \code{medfate} package
#'
#' @param customParams Data frame with the custom parameters info, usually
#'   the result of \code{\link{load_customParams}}
#'
#' @export

inputMod <- function(swbInput, customParams) {

  # check if customParams exists, if not return swbInput without modification
  if (is.null(customParams)) {
    return(swbInput)
  }

  # get the names of the custom params and the input tables
  custom <- names(customParams)
  above_par <- names(swbInput[['above']])
  # below_par <- names(swbInput$below)


  # iterate between the custom params
  for (param in custom) {
    # check if the param exists in above
    if (param %in% above_par) {
      # subsitute it! but by sp index, as the order is important
      for (sp in swbInput[['cohorts']][['SP']]) {
        sp_code <- rownames(swbInput[["cohorts"]][swbInput[["cohorts"]][['SP']] == sp, ])
        swbInput[["above"]][sp_code, param] <- customParams[customParams[['SpIndex']] == sp, param]
      }
    }

    # check if the param exists in below
    # if (param %in% below_par) {
    #   # subsitute it!
    #   swbInput$below[[param]] <- customParams[[param]]
    # }
  }

  # return the new input
  return(swbInput)
}
