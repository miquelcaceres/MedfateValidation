#' List the appropriate sites for a given validation
#'
#' This function list the sites that fit for a given validation (global,
#' transpiration and temperature)
#'
#' @param validation Character indicating for which validation sites are ready
#'
#' @return A character vector with the site names for the given validation, except
#'   for "all", which returns a tibble with the site ID and the validation as
#'   columns
#'
#' @export

list_sites <- function(validation) {

  # allowed validation values are "global", "transpiration", "temperature" and
  # "all" ("all" gives a summary table with all validation and sites)
  list.dirs('Sites_data/', FALSE, FALSE) %>%
    purrr::map(load_miscData) %>%
    dplyr::bind_rows() -> sites_misc_info

  if (validation == 'global') {
    sites_misc_info %>%
      filter(Validation %in% c('global', 'global_transp')) %>%
      pull(ID) -> res
    return(res)
  }

  if (validation == 'temperature') {
    sites_misc_info %>%
      filter(Validation == 'temperature') %>%
      pull(ID) -> res
    return(res)
  }

  if (validation == 'transpiration') {
    sites_misc_info %>%
      filter(Validation == 'global_transp') %>%
      pull(ID) -> res
    return(res)
  }

  if (validation == 'all') {
    sites_misc_info %>%
      select(ID, Validation) -> res
    return(res)
  }

}
