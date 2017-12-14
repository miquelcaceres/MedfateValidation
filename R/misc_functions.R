#' List the appropriate sites for a given validation
#'
#' This function list the sites that fit for a given validation (global,
#' transpiration and temperature)
#'
#' @param validation Character indicating for which validation sites are ready
#'
#' @param definitive Boolean indicating if only the definitive sites must be
#'   returned. Default to FALSE.
#'
#' @return A character vector with the site names for the given validation, except
#'   for "all", which returns a tibble with the site ID and the validation as
#'   columns
#'
#' @export

list_sites <- function(validation, definitive = FALSE) {

  # allowed validation values are "global", "transpiration", "temperature" and
  # "all" ("all" gives a summary table with all validation and sites)
  list.dirs('Sites_data/', FALSE, FALSE) %>%
    purrr::map(load_miscData) %>%
    dplyr::bind_rows() -> sites_misc_info

  if (validation == 'global') {
    sites_misc_info %>%
      dplyr::filter(Validation %in% c('global', 'global_transp')) -> temp_info

    if (definitive) {
      temp_info %>%
        dplyr::filter(Definitive == 'Yes') %>%
        dplyr::pull(ID) -> res
    } else {
      temp_info %>%
        dplyr::pull(ID) -> res
    }

    return(res)
  }

  if (validation == 'temperature') {
    sites_misc_info %>%
      dplyr::filter(Validation == 'temperature') -> temp_info

    if (definitive) {
      temp_info %>%
        dplyr::filter(Definitive == 'Yes') %>%
        dplyr::pull(ID) -> res
    } else {
      temp_info %>%
        dplyr::pull(ID) -> res
    }

    return(res)
  }

  if (validation == 'transpiration') {
    sites_misc_info %>%
      dplyr::filter(Validation == 'global_transp') -> temp_info

    if (definitive) {
      temp_info %>%
        dplyr::filter(Definitive == 'Yes') %>%
        dplyr::pull(ID) -> res
    } else {
      temp_info %>%
        dplyr::pull(ID) -> res
    }

    return(res)
  }

  if (validation == 'all') {
    sites_misc_info %>%
      dplyr::select(ID, Validation, Definitive) -> res
    return(res)
  }

}
