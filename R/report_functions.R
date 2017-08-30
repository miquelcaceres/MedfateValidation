#' Report generation function
#'
#' Function for generating automaitcally the report
#'
#' @param output_file file name for the output report
#' @param output_dir destination folder
#' @param ... Rmarkdown parameters
#'
#' @export

report_render <- function(output_file = NULL, output_dir = NULL, ...) {

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
