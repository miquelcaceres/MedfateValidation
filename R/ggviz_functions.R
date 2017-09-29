#' @describeIn plot_swc_simple
#'
#' @export

plot_swc_simple_gg <- function(models, soil, measured_data) {

  # data
  dates <- models[['simple']][['Dates']]
  SWC_vals_simple <- models[['simple']][['W.1']] * soil$Theta_FC[[1]]
  SWC_vals_meas <- measured_data[['SWC']]
  y_limits <- c(
    min(min(SWC_vals_simple, na.rm = TRUE),
        min(SWC_vals_meas, na.rm = TRUE)) - (min(min(SWC_vals_simple, na.rm = TRUE),
                                                 min(SWC_vals_meas, na.rm = TRUE)))*0.05,
    max(max(SWC_vals_simple, na.rm = TRUE),
        max(SWC_vals_meas, na.rm = TRUE)) + (max(max(SWC_vals_simple, na.rm = TRUE),
                                                 max(SWC_vals_meas, na.rm = TRUE)))*0.05
  )

  plot_data <- data.frame(
    Date = as.Date(dates),
    Simple = SWC_vals_simple,
    Measured  = SWC_vals_meas,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Simple, y = Measured)) +
    theme_minimal()


  swc_plot <- plot_data %>%
    tidyr::gather(key = Model, value = SWC, -Date) %>%
    ggplot(aes(x = Date, y = SWC, colour = Model)) +
    scale_color_manual(values = c('red', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(SWC_vals_meas))) {

    return(list(swc = swc_plot, cor = cor_plot))

  } else {

    # if there is measured values, populate the plots
    swc_plot <- swc_plot +
      geom_line(alpha = 0.4, size = 1)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4)

    return(list(swc = swc_plot, cor = cor_plot))
  }
}

#' @describeIn plot_swc_complex
#'
#' @export

plot_swc_complex_gg <- function(models, soil, measured_data) {

  # data
  dates <- models[['complex']][['Dates']]
  SWC_vals_complex <- models[['complex']][['W.1']] * soil$Theta_FC[[1]]
  SWC_vals_meas <- measured_data[['SWC']]
  y_limits <- c(
    min(min(SWC_vals_complex, na.rm = TRUE),
        min(SWC_vals_meas, na.rm = TRUE)) - (min(min(SWC_vals_complex, na.rm = TRUE),
                                                 min(SWC_vals_meas, na.rm = TRUE)))*0.05,
    max(max(SWC_vals_complex, na.rm = TRUE),
        max(SWC_vals_meas, na.rm = TRUE)) + (max(max(SWC_vals_complex, na.rm = TRUE),
                                                 max(SWC_vals_meas, na.rm = TRUE)))*0.05
  )

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = SWC_vals_complex,
    Measured  = SWC_vals_meas,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Measured)) +
    theme_minimal()


  swc_plot <- plot_data %>%
    tidyr::gather(key = Model, value = SWC, -Date) %>%
    ggplot(aes(x = Date, y = SWC, colour = Model)) +
    scale_color_manual(values = c('green', 'red')) +
    scale_x_date(date_breaks = '4 months') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(SWC_vals_meas))) {

    return(list(swc = swc_plot, cor = cor_plot))

  } else {

    # if there is measured values, populate the plots
    swc_plot <- swc_plot +
      geom_line(alpha = 0.4, size = 1)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4)

    return(list(swc = swc_plot, cor = cor_plot))
  }
}

#' @describeIn plot_swc_both
#'
#' @export

plot_swc_both_gg <- function(models, soil, measured_data) {

  # data
  dates <- models[['simple']][['Dates']]
  SWC_vals_complex <- models[['complex']][['W.1']] * soil$Theta_FC[[1]]
  SWC_vals_simple <- models[['simple']][['W.1']] * soil$Theta_FC[[1]]
  y_limits <- c(
    min(min(SWC_vals_complex, na.rm = TRUE),
        min(SWC_vals_simple, na.rm = TRUE)) - (min(min(SWC_vals_complex, na.rm = TRUE),
                                                   min(SWC_vals_simple, na.rm = TRUE)))*0.05,
    max(max(SWC_vals_complex, na.rm = TRUE),
        max(SWC_vals_simple, na.rm = TRUE)) + (max(max(SWC_vals_complex, na.rm = TRUE),
                                                   max(SWC_vals_simple, na.rm = TRUE)))*0.05
  )

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = SWC_vals_complex,
    Simple  = SWC_vals_simple,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Simple)) +
    theme_minimal()


  swc_plot <- plot_data %>%
    tidyr::gather(key = Model, value = SWC, -Date) %>%
    ggplot(aes(x = Date, y = SWC, colour = Model)) +
    scale_color_manual(values = c('green', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # if there is measured values, populate the plots
  swc_plot <- swc_plot +
    geom_line(alpha = 0.4, size = 1)

  cor_plot <- cor_plot +
    geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
    geom_point(shape = 20, alpha = 0.8, colour = 'black') +
    stat_smooth(method = 'lm', colour = 'black',
                size = 1, se = FALSE, alpha = 0.4)

  return(list(swc = swc_plot, cor = cor_plot))
}

#' @describeIn plot_eplanttot_simple
#'
#' @export

plot_eplanttot_simple_gg <- function(models, measured_data) {

  # data
  dates <- models[['simple']][['Dates']]
  Etot_vals_simple <- models[['simple']][['Eplanttot']]
  Etot_vals_meas <- measured_data[['Eplanttot']]
  y_limits <- c(
    min(min(Etot_vals_simple, na.rm = TRUE),
        min(Etot_vals_meas, na.rm = TRUE)) - (min(min(Etot_vals_simple, na.rm = TRUE),
                                                 min(Etot_vals_meas, na.rm = TRUE)))*0.05,
    max(max(Etot_vals_simple, na.rm = TRUE),
        max(Etot_vals_meas, na.rm = TRUE)) + (max(max(Etot_vals_simple, na.rm = TRUE),
                                                 max(Etot_vals_meas, na.rm = TRUE)))*0.05
  )

  plot_data <- data.frame(
    Date = as.Date(dates),
    Simple = Etot_vals_simple,
    Measured  = Etot_vals_meas,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Simple, y = Measured)) +
    theme_minimal()


  etot_plot <- plot_data %>%
    tidyr::gather(key = Model, value = Etot, -Date) %>%
    ggplot(aes(x = Date, y = Etot, colour = Model)) +
    scale_color_manual(values = c('red', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(Etot_vals_meas))) {

    return(list(etot = etot_plot, cor = cor_plot))

  } else {

    # if there is measured values, populate the plots
    etot_plot <- etot_plot +
      geom_line(alpha = 0.4, size = 1)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4)

    return(list(etot = etot_plot, cor = cor_plot))
  }
}

#' @describeIn plot_eplanttot_complex
#'
#' @export

plot_eplanttot_complex_gg <- function(models, measured_data) {

  # data
  dates <- models[['complex']][['Dates']]
  Etot_vals_complex <- models[['complex']][['Eplanttot']]
  Etot_vals_meas <- measured_data[['Eplanttot']]
  y_limits <- c(
    min(min(Etot_vals_complex, na.rm = TRUE),
        min(Etot_vals_meas, na.rm = TRUE)) - (min(min(Etot_vals_complex, na.rm = TRUE),
                                                  min(Etot_vals_meas, na.rm = TRUE)))*0.05,
    max(max(Etot_vals_complex, na.rm = TRUE),
        max(Etot_vals_meas, na.rm = TRUE)) + (max(max(Etot_vals_complex, na.rm = TRUE),
                                                  max(Etot_vals_meas, na.rm = TRUE)))*0.05
  )

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = Etot_vals_complex,
    Measured  = Etot_vals_meas,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Measured)) +
    theme_minimal()


  etot_plot <- plot_data %>%
    tidyr::gather(key = Model, value = Etot, -Date) %>%
    ggplot(aes(x = Date, y = Etot, colour = Model)) +
    scale_color_manual(values = c('green', 'red')) +
    scale_x_date(date_breaks = '4 months') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(Etot_vals_meas))) {

    return(list(etot = etot_plot, cor = cor_plot))

  } else {

    # if there is measured values, populate the plots
    etot_plot <- etot_plot +
      geom_line(alpha = 0.4, size = 1)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4)

    return(list(etot = etot_plot, cor = cor_plot))
  }
}

#' @describeIn plot_eplanttot_both
#'
#' @export

plot_eplanttot_both_gg <- function(models, measured_data) {

  # data
  dates <- models[['simple']][['Dates']]
  Eplanttot_vals_complex <- models[['complex']][['Eplanttot']]
  Eplanttot_vals_simple <- models[['simple']][['Eplanttot']]
  y_limits <- c(
    min(min(Eplanttot_vals_complex, na.rm = TRUE),
        min(Eplanttot_vals_simple, na.rm = TRUE)) - (min(min(Eplanttot_vals_complex, na.rm = TRUE),
                                                   min(Eplanttot_vals_simple, na.rm = TRUE)))*0.05,
    max(max(Eplanttot_vals_complex, na.rm = TRUE),
        max(Eplanttot_vals_simple, na.rm = TRUE)) + (max(max(Eplanttot_vals_complex, na.rm = TRUE),
                                                   max(Eplanttot_vals_simple, na.rm = TRUE)))*0.05
  )

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = Eplanttot_vals_complex,
    Simple  = Eplanttot_vals_simple,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Simple)) +
    theme_minimal()


  etot_plot <- plot_data %>%
    tidyr::gather(key = Model, value = Etot, -Date) %>%
    ggplot(aes(x = Date, y = SWC, colour = Model)) +
    scale_color_manual(values = c('green', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # if there is measured values, populate the plots
  etot_plot <- etot_plot +
    geom_line(alpha = 0.4, size = 1)

  cor_plot <- cor_plot +
    geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
    geom_point(shape = 20, alpha = 0.8, colour = 'black') +
    stat_smooth(method = 'lm', colour = 'black',
                size = 1, se = FALSE, alpha = 0.4)

  return(list(etot = etot_plot, cor = cor_plot))
}

#' @describeIn plot_cohort_simple
#'
#' @export

plot_cohort_simple_gg <- function(models, measured_data) {

  coh_names <- as.character(
    na.omit(stringr::str_extract(names(models[['simple']]), '^E_.+'))
  )
  dates <- models[['simple']][['Dates']]

  list_ecoh_plots <- vector("list", length(coh_names))
  list_cor_plots <- vector("list", length(coh_names))

  n <- 1
  # loop by cohort
  for (cohort in coh_names) {
    E_vals_simple <- models[['simple']][[cohort]]
    E_vals_meas <- measured_data[[cohort]]

    plot_data <- data.frame(
      Date = as.Date(dates),
      Simple = E_vals_simple,
      Measured  = E_vals_meas,
      stringsAsFactors = FALSE
    )

    # empty plots
    cor_plot <- ggplot(data = plot_data,
                       aes(x = Simple, y = Measured)) +
      theme_minimal()


    ecoh_plot <- plot_data %>%
      tidyr::gather(key = Model, value = Ecoh, -Date) %>%
      ggplot(aes(x = Date, y = Ecoh, colour = Model)) +
      scale_color_manual(values = c('red', 'blue')) +
      scale_x_date(date_breaks = '4 months') +
      theme_minimal() +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(angle = 25)
      )

    # check if there are measured vals, if not, return an empty plot
    if (all(is.na(E_vals_meas))) {

      list_ecoh_plots[n] <- ecoh_plot
      list_cor_plots[n] <- cor_plot

    } else {
      # if there is measured values, populate the plots
      ecoh_plot <- ecoh_plot +
        geom_line(alpha = 0.4, size = 1)

      cor_plot <- cor_plot +
        geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
        geom_point(shape = 20, alpha = 0.8, colour = 'black') +
        stat_smooth(method = 'lm', colour = 'black',
                    size = 1, se = FALSE, alpha = 0.4)

      list_ecoh_plots[n] <- ecoh_plot
      list_cor_plots[n] <- cor_plot
    }

    n <- n+1
  }

  names(list_ecoh_plots) <- coh_names
  names(list_cor_plots) <- coh_names

  return(list(ecoh = list_ecoh_plots, cor = list_cor_plots))
}

#' @describeIn plot_cohort_complex
#'
#' @export

plot_cohort_complex_gg <- function(models, measured_data) {

  coh_names <- as.character(
    na.omit(stringr::str_extract(names(models[['complex']]), '^E_.+'))
  )
  dates <- models[['complex']][['Dates']]

  list_ecoh_plots <- vector("list", length(coh_names))
  list_cor_plots <- vector("list", length(coh_names))

  n <- 1
  # loop by cohort
  for (cohort in coh_names) {
    E_vals_complex <- models[['complex']][[cohort]]
    E_vals_meas <- measured_data[[cohort]]

    plot_data <- data.frame(
      Date = as.Date(dates),
      Complex = E_vals_complex,
      Measured  = E_vals_meas,
      stringsAsFactors = FALSE
    )

    # empty plots
    cor_plot <- ggplot(data = plot_data,
                       aes(x = Complex, y = Measured)) +
      theme_minimal()


    ecoh_plot <- plot_data %>%
      tidyr::gather(key = Model, value = Ecoh, -Date) %>%
      ggplot(aes(x = Date, y = Ecoh, colour = Model)) +
      scale_color_manual(values = c('green', 'red')) +
      scale_x_date(date_breaks = '4 months') +
      theme_minimal() +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(angle = 25)
      )

    # check if there are measured vals, if not, return an empty plot
    if (all(is.na(E_vals_meas))) {

      list_ecoh_plots[n] <- ecoh_plot
      list_cor_plots[n] <- cor_plot

    } else {
      # if there is measured values, populate the plots
      ecoh_plot <- ecoh_plot +
        geom_line(alpha = 0.4, size = 1)

      cor_plot <- cor_plot +
        geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
        geom_point(shape = 20, alpha = 0.8, colour = 'black') +
        stat_smooth(method = 'lm', colour = 'black',
                    size = 1, se = FALSE, alpha = 0.4)

      list_ecoh_plots[n] <- ecoh_plot
      list_cor_plots[n] <- cor_plot
    }

    n <- n+1
  }

  names(list_ecoh_plots) <- coh_names
  names(list_cor_plots) <- coh_names

  return(list(ecoh = list_ecoh_plots, cor = list_cor_plots))
}

#' @describeIn plot_cohort_both
#'
#' @export

plot_cohort_both_gg <- function(models, measured_data) {

  coh_names <- as.character(
    na.omit(stringr::str_extract(names(models[['simple']]), '^E_.+'))
  )
  dates <- models[['simple']][['Dates']]

  list_ecoh_plots <- vector("list", length(coh_names))
  list_cor_plots <- vector("list", length(coh_names))

  n <- 1
  # loop by cohort
  for (cohort in coh_names) {
    E_vals_simple <- models[['simple']][[cohort]]
    E_vals_complex <- models[['cohort']][[cohort]]

    plot_data <- data.frame(
      Date = as.Date(dates),
      Simple = E_vals_simple,
      Complex  = E_vals_complex,
      stringsAsFactors = FALSE
    )

    # empty plots
    cor_plot <- ggplot(data = plot_data,
                       aes(x = Complex, y = Simple)) +
      theme_minimal()


    ecoh_plot <- plot_data %>%
      tidyr::gather(key = Model, value = Ecoh, -Date) %>%
      ggplot(aes(x = Date, y = Ecoh, colour = Model)) +
      scale_color_manual(values = c('green', 'blue')) +
      scale_x_date(date_breaks = '4 months') +
      theme_minimal() +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(angle = 25)
      )

    # check if there are measured vals, if not, return an empty plot
    if (all(is.na(E_vals_meas))) {

      list_ecoh_plots[n] <- ecoh_plot
      list_cor_plots[n] <- cor_plot

    } else {
      # if there is measured values, populate the plots
      ecoh_plot <- ecoh_plot +
        geom_line(alpha = 0.4, size = 1)

      cor_plot <- cor_plot +
        geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
        geom_point(shape = 20, alpha = 0.8, colour = 'black') +
        stat_smooth(method = 'lm', colour = 'black',
                    size = 1, se = FALSE, alpha = 0.4)

      list_ecoh_plots[n] <- ecoh_plot
      list_cor_plots[n] <- cor_plot
    }

    n <- n+1
  }

  names(list_ecoh_plots) <- coh_names
  names(list_cor_plots) <- coh_names

  return(list(ecoh = list_ecoh_plots, cor = list_cor_plots))
}

#' @describeIn plot_res
#'
#' @export

plot_res_gg <- function(variable, models, soil, measured_data, mode) {

  # SWC
  if (variable == 'SWC') {

    if (mode == 'both') {
      # get the plots
      simple <- plot_swc_simple_gg(models, soil, measured_data)
      complex <- plot_swc_complex_gg(models, soil, measured_data)
      both <- plot_swc_both_gg(models, soil, measured_data)

      # build the cowplot
      plot_grid(
        simple[['swc']], complex[['swc']], both[['swc']],
        simple[['cor']], complex[['cor']], both[['cor']],
        ncol = 3
      )
    }

    if (mode == 'simple') {
      # get the plots
      simple <- plot_swc_simple_gg(models, soil, measured_data)

      # build the cowplot
      plot_grid(
        plotlist = simple, ncol = 1
      )
    }

    if (mode == 'complex') {
      # get the plots
      complex <- plot_swc_complex_gg(models, soil, measured_data)

      # build the cowplot
      plot_grid(
        plotlist = complex, ncol = 1
      )
    }
  }

  if (variable == 'Eplanttot') {

    if (mode == 'both') {
      # get the plots
      simple <- plot_eplanttot_simple_gg(models, measured_data)
      complex <- plot_eplanttot_complex_gg(models, measured_data)
      both <- plot_eplanttot_both_gg(models, measured_data)

      # build the cowplot
      plot_grid(
        simple[['etot']], complex[['etot']], both[['etot']],
        simple[['cor']], complex[['cor']], both[['cor']],
        ncol = 3
      )
    }

    if (mode == 'simple') {
      # get the plots
      simple <- plot_eplanttot_simple_gg(models, measured_data)

      # build the cowplot
      plot_grid(
        plotlist = simple, ncol = 1
      )
    }

    if (mode == 'complex') {
      # get the plots
      complex <- plot_eplanttot_complex_gg(models, measured_data)

      # build the cowplot
      plot_grid(
        plotlist = complex, ncol = 1
      )
    }
  }

  if (variable == 'E_by_Cohort') {

    if (mode == 'both') {
      # get the plot lists
      simple <- plot_cohort_simple_gg(models, measured_data)
      complex <- plot_cohort_complex_gg(models, measured_data)
      both <- plot_cohort_both_gg(models, measured_data)

      # build the cowplot
      n_coh <- length(simple[['ecoh']])
      plot_grid(
        plotlist = simple[['ecoh']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
      plot_grid(
        plotlist = simple[['cor']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
      plot_grid(
        plotlist = complex[['ecoh']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
      plot_grid(
        plotlist = complex[['cor']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
      plot_grid(
        plotlist = both[['ecoh']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
      plot_grid(
        plotlist = both[['cor']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
    }

    if (mode == 'simple') {
      # get the plot lists
      simple <- plot_cohort_simple_gg(models, measured_data)

      # build the cowplot
      n_coh <- length(simple[['ecoh']])
      plot_grid(
        plotlist = simple[['ecoh']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
      plot_grid(
        plotlist = simple[['cor']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
    }

    if (mode == 'complex') {
      # get the plot lists
      complex <- plot_cohort_complex_gg(models, measured_data)

      # build the cowplot
      n_coh <- length(complex[['ecoh']])
      plot_grid(
        plotlist = complex[['ecoh']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
      plot_grid(
        plotlist = complex[['cor']],
        ncol = if (n_coh < 4) {n_coh} else {3},
        nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)}
      )
    }
  }
}
