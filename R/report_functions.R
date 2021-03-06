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
#'   \item{control: List with control parameters}
#' }
#'
#' @param report which kind of report must be generated, "global" or
#'   "transpiration"
#' @param output_file file name for the output report
#' @param output_dir destination folder (if it does not exist, it will be
#'   created)
#' @param ... Rmarkdown parameters
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

  if (report == 'kmax') {
    # render the template with the code indicated in the ... argument
    rmarkdown::render(input = system.file("Rmd_templates", "Global_report_template_kmax_factor.Rmd",
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

  if (report == 'temperature') {
    # render the template with the code indicated in the ... argument
    rmarkdown::render(input = system.file("Rmd_templates",
                                          "Temperature_report_template.Rmd",
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
#' @param control List with control parameters
#'
#' @export
report_eval <- function(sites, wd, transpMode,
                        control = medfate::defaultControl()) {

  for (code in sites) {
    report_name <- file.path('Output', packageVersion('medfate')[[1]],
                             code,
                             paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                    '_', code, '_',
                                    'global_report.html'))

    report_folder <- file.path('Output', packageVersion('medfate')[[1]],
                               code)

    report_render('global', report_name, report_folder,
                  wd = wd, code = code, transpMode = transpMode, control = control)
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
#' @param SPParams Character indicating which SpParams table to use. "old" for
#'   the SpParamsMED table from medfate and "new" for the newParams table of
#'   MedfateValidation
#'
#' @param tapering Logical indicating if use the taper factor (TRUE) or not
#'   (FALSE)
#'
#' @export

global_kmax_factor_process <- function(sites, wd, transpMode, SPParams = 'old',
                                       tapering = TRUE) {

  for (code in sites) {
    report_name <- file.path('Output', packageVersion('medfate')[[1]],
                             code,
                             paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                    '_', code, '_',
                                    'global_kmax_factor_report.html'))

    report_folder <- file.path('Output', packageVersion('medfate')[[1]],
                               code)

    report_render('kmax', report_name, report_folder, wd = wd, code = code,
                  transpMode = transpMode, SPParams = SPParams, tapering = tapering)
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

    subcode <- strsplit(code, '/', fixed = TRUE)[[1]][2]

    report_name <- file.path('Output', packageVersion('medfate')[[1]],
                             code,
                             paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                    '_', subcode, '_',
                                    'transp_report.html'))

    report_folder <- file.path('Output', packageVersion('medfate')[[1]],
                               code)

    report_render('transpiration', report_name, report_folder, wd = wd,
                  code = code, transpMode = transpMode, SPParams = SPParams)
  }
}

#' Main function to do the Temperature
#'
#' Validate a list of sites/plots
#'
#' @param sites A character vector with the sites codes
#'
#' @param wd Complete path to the validation directory
#'
#' @param transpMode Character string indicating the transpiration mode to use
#'
#' @param SPParams Character indicating which SpParams table to use. "old" for
#'   the SpParamsMED table from medfate and "new" for the newParams table of
#'   MedfateValidation
#'
#' @param tapering Logical indicating if use the taper factor (TRUE) or not
#'   (FALSE)
#'
#' @export

temperature_process <- function(sites, wd, transpMode = 'complex',
                                SPParams = 'new', tapering = TRUE) {

  for (code in sites) {
    report_name <- file.path('Output', packageVersion('medfate')[[1]],
                             code,
                             paste0(format(Sys.time(), "%Y%m%d_%H%M"),
                                    '_', code, '_',
                                    'temperature_report.html'))

    report_folder <- file.path('Output', packageVersion('medfate')[[1]],
                               code)

    report_render('temperature', report_name, report_folder, wd = wd, code = code,
                  transpMode = transpMode, SPParams = SPParams,
                  tapering = tapering)
  }
}

#' Shiny widget for the Status Rmd
#'
#' This function generates the shiny widget to inspect sites individually
#'
#' @param data List with the environments where the report objects are located,
#'   generated with \code{\link{load_rdatas}}.
#'
#' @param wd character indicating the working directory
#'
#' @export

report_widget <- function(data, wd) {

  shinyApp(
    options = list(height = 1000),

    ui = fluidPage(

      br(),

      # inputs
      fluidRow(
        column(
          4,
          selectInput('dataset', 'Select the site:', names(data))
        ),
        column(
          8,
          htmlOutput('link')
        )
      ),

      tags$br(),

      #outputs
      fluidRow(

        column(
          6,
          p('SWC plot'),
          br(),
          plotOutput('swc_plot', width = '100%')
        ),

        column(
          6,
          p('Total transpiration plot'),
          br(),
          plotOutput('etot_plot', width = '100%')
        )
      )
    ),

    server = function(input, output, session) {

      output$link <- renderUI({

        stamp <- Sys.time()

        file_creation_info <- file.info(
          list.files(file.path(wd, 'Output', packageVersion('medfate')[[1]], input$dataset),
                     pattern = 'html', full.names = TRUE)
        )

        file_index <- which(as.numeric(stamp - file_creation_info$mtime) ==
                              as.numeric(min(stamp - file_creation_info$mtime)))

        file_name <- row.names(file_creation_info)[file_index]

        file.copy(file_name, 'www/report.html', overwrite = TRUE)

        tags$a(paste0('Link to the detailed report of ', input$dataset, ' site'),
               href = 'http://10.1.2.24:8787/file_show?path=%2Fsrv%2Fshiny-server%2Fmedfatevalidation_report%2Fwww%2Freport.html', target = '_blank')
      })

      output$swc_plot <- renderPlot({
        plot_res_gg(
          'SWC', data[[input$dataset]][['models_dfs']],
          data[[input$dataset]][['soil_object']],
          data[[input$dataset]][['measuredData']],
          data[[input$dataset]][['params']][['transpMode']]
        )
      }, height = function() {
        400 + (200*nrow(data[[input$dataset]][['SWC_stats']]))
      })

      output$etot_plot <- renderPlot({
        plot_res_gg(
          'Eplanttot', data[[input$dataset]][['models_dfs']],
          data[[input$dataset]][['soil_object']],
          data[[input$dataset]][['measuredData']],
          data[[input$dataset]][['params']][['transpMode']]
        )
      }, height = 600)

    }
  )
}
