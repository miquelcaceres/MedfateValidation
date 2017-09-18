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
#' @param output_file file name for the output report
#' @param output_dir destination folder (if it does not exist, it will be
#'   created)
#' @param ... Rmarkdown parameters
#'
#' @export

report_render <- function(output_file = NULL, output_dir = NULL, ...) {

  # check if directory to save the report exists and if not, create it
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # render the template with the code indicated in the ... argument
  rmarkdown::render(input = system.file("Rmd_templates", "Report_template.Rmd", package = "MedfateValidation"),
                    output_format = c('html_document'),
                    output_file = output_file,
                    output_dir = output_dir,
                    runtime = 'auto',
                    clean = TRUE,
                    params = list(...),
                    run_pandoc = TRUE,
                    quiet = TRUE)
}

