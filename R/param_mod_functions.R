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

  # get the names of the custom params and the SpParams
  custom <- names(customParams)
  sp_par <- names(SpParams)

  # iterate between the custom params exisiting in SpParams
  for (param in custom) {
    # check if the param exists in SpParams
    if (param %in% sp_par) {
      # subsitute it!
      SpParams[[param]] <- customParams[[param]]
    }
  }

  # return the new SpParams
  return(SpParams)
}
