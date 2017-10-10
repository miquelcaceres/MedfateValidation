#' Report generation function
#'
#' Function for generating automatically the report
#'
#' The parameters accepted by the rmarkdown template (\code{...} argument) are
#' the following:
#' \itemize{
#'   \item{wd: working directory (i.e. the route to the validation root folder)}
#'   \item{code: Site/Plot code for which the report must be generated}
#'   \item{transpMode: Transpiration mode, accepted values are \code{simple},
#'         \code{complex} and \code{both}}
#' }
#'
#' @param report which kind of report must be generated, "global" or
#'   "transpiration"
#' @param output_file file name for the output report
#' @param output_dir destination folder (if it does not exist, it will be
#'   created)
#' @param ... Rmarkdown parameters
#'
#' @export

report_render <- function(report = 'global',
                          output_file = NULL, output_dir = NULL, ...) {

  # check if directory to save the report exists and if not, create it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  if (report == 'global') {
    # render the template with the code indicated in the ... argument
    rmarkdown::render(input = system.file("Rmd_templates", "Global_report_template.Rmd", package = "MedfateValidation"),
                      output_format = c('html_document'),
                      output_file = output_file,
                      output_dir = output_dir,
                      runtime = 'auto',
                      clean = TRUE,
                      params = list(...),
                      run_pandoc = TRUE,
                      quiet = TRUE)
  }

  if (report == 'transpiration') {
    # render the template with the code indicated in the ... argument
    rmarkdown::render(input = system.file("Rmd_templates",
                                          "Transpiration_report_template.Rmd",
                                          package = "MedfateValidation"),
                      output_format = c('html_document'),
                      output_file = output_file,
                      output_dir = output_dir,
                      runtime = 'auto',
                      clean = TRUE,
                      params = list(...),
                      run_pandoc = TRUE,
                      quiet = TRUE)
  }

}

#' Main function to do the calibration/validation
#'
#' Validate a list of sites/plots
#'
#' @param sites A character vector with the sites codes
#'
#' @param wd Complete path to the validation directory
#'
#' @param transpMode Character string indicating the transpiration mode to use
#'
#' @export

global_process <- function(sites, wd, transpMode, SPParams = 'old') {

  for (code in sites) {
    report_name <- file.path('Output', packageVersion('medfate')[[1]],
                             code,
                             paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                    '_', code, '_',
                                    'global_report.html'))

    report_folder <- file.path('Output', packageVersion('medfate')[[1]],
                               code)

    report_render('global', report_name, report_folder, wd = wd, code = code,
                  transpMode = transpMode, SPParams = SPParams)
  }
}

#' Main function to do the transpiration process
#'
#' Validate a list of sites/plots
#'
#' @param sites A character vector with the sites codes
#'
#' @param wd Complete path to the validation directory
#'
#' @param transpMode Character string indicating the transpiration mode to use
#'
#' @export

transpiration_process <- function(sites, wd, transpMode, SPParams = 'old') {

  for (code in sites) {
    report_name <- file.path('Output', packageVersion('medfate')[[1]],
                             code,
                             paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                    '_', code, '_',
                                    'transp_report.html'))

    report_folder <- file.path('Output', packageVersion('medfate')[[1]],
                               code)

    report_render('transpiration', report_name, report_folder, wd = wd,
                  code = code, transpMode = transpMode, SPParams = SPParams)
  }
}
