
plot_swc_simple_gg <- function(models, soil, measured_data) {

  theta_FC = medfate::soil.thetaFC(soil)

  # get the measured layers
  swc_vars <- c(
    'SWC',
    names(measured_data)[stringr::str_detect(names(measured_data), '^SWC_[0-9]$')]
  )

  dates <- character(0)
  SWC_vals_simple <- numeric(0)
  SWC_vals_meas <- numeric(0)
  Layer <- character(0)

  # data
  for (name in swc_vars) {
    index <- which(swc_vars == name)
    dates <- c(dates, as.character(models[['simple']][['Dates']]))
    SWC_vals_simple <- c(SWC_vals_simple,
                         models[['simple']][[paste0('W.', index)]] * theta_FC[index])
    SWC_vals_meas <- c(SWC_vals_meas, measured_data[[name]])
    Layer <- c(Layer, rep(name, length(measured_data[[name]])))
  }

  plot_data <- data.frame(
    Date = as.Date(dates),
    Simple = SWC_vals_simple,
    Measured  = SWC_vals_meas,
    Layer = Layer,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Simple, y = Measured)) +
    facet_grid(Layer ~ .) +
    theme_medfate()


  swc_plot <- plot_data %>%
    tidyr::gather(key = Model, value = SWC, -Date, -Layer) %>%
    ggplot(aes(x = Date, y = SWC, colour = Model, linetype = Layer)) +
    facet_grid(Layer ~ .) +
    scale_color_manual(values = c('red', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(SWC_vals_meas))) {

    return(list(swc = swc_plot, cor = cor_plot))

  } else {

    # build the limits
    y_limits <- c(
      min(min(SWC_vals_simple, na.rm = TRUE),
          min(SWC_vals_meas, na.rm = TRUE)) - (min(min(SWC_vals_simple, na.rm = TRUE),
                                                   min(SWC_vals_meas, na.rm = TRUE)))*0.05,
      max(max(SWC_vals_simple, na.rm = TRUE),
          max(SWC_vals_meas, na.rm = TRUE)) + (max(max(SWC_vals_simple, na.rm = TRUE),
                                                   max(SWC_vals_meas, na.rm = TRUE)))*0.05
    )

    # if there is measured values, populate the plots
    swc_plot <- swc_plot +
      geom_line(alpha = 0.4, size = 0.7) +
      geom_point(shape = 20, alpha = 0.3, size = 2) +
      scale_y_continuous(limits = y_limits)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4) +
      scale_y_continuous(limits = y_limits) +
      scale_x_continuous(limits = y_limits)

    return(list(swc = swc_plot, cor = cor_plot))
  }
}

plot_swc_complex_gg <- function(models, soil, measured_data) {

  theta_FC = medfate::soil.thetaFC(soil)

  # get the measured layers
  swc_vars <- c(
    'SWC',
    names(measured_data)[stringr::str_detect(names(measured_data), '^SWC_[0-9]$')]
  )

  dates <- character(0)
  SWC_vals_complex <- numeric(0)
  SWC_vals_meas <- numeric(0)
  Layer <- character(0)

  # data
  for (name in swc_vars) {
    index <- which(swc_vars == name)
    dates <- c(dates, as.character(models[['complex']][['Dates']]))
    SWC_vals_complex <- c(SWC_vals_complex,
                         models[['complex']][[paste0('W.', index)]] * theta_FC[index])
    SWC_vals_meas <- c(SWC_vals_meas, measured_data[[name]])
    Layer <- c(Layer, rep(name, length(measured_data[[name]])))
  }

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = SWC_vals_complex,
    Measured  = SWC_vals_meas,
    Layer = Layer,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Measured)) +
    facet_grid(Layer ~ .) +
    theme_medfate()


  swc_plot <- plot_data %>%
    tidyr::gather(key = Model, value = SWC, -Date, -Layer) %>%
    ggplot(aes(x = Date, y = SWC, colour = Model, linetype = Layer)) +
    facet_grid(Layer ~ .) +
    scale_color_manual(values = c('green', 'red')) +
    scale_x_date(date_breaks = '4 months') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(SWC_vals_meas))) {

    return(list(swc = swc_plot, cor = cor_plot))

  } else {

    # build the limits
    y_limits <- c(
      min(min(SWC_vals_complex, na.rm = TRUE),
          min(SWC_vals_meas, na.rm = TRUE)) - (min(min(SWC_vals_complex, na.rm = TRUE),
                                                   min(SWC_vals_meas, na.rm = TRUE)))*0.05,
      max(max(SWC_vals_complex, na.rm = TRUE),
          max(SWC_vals_meas, na.rm = TRUE)) + (max(max(SWC_vals_complex, na.rm = TRUE),
                                                   max(SWC_vals_meas, na.rm = TRUE)))*0.05
    )

    # if there is measured values, populate the plots
    swc_plot <- swc_plot +
      geom_line(alpha = 0.4, size = 0.7) +
      geom_point(shape = 20, alpha = 0.3, size = 2) +
      scale_y_continuous(limits = y_limits)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4) +
      scale_y_continuous(limits = y_limits) +
      scale_x_continuous(limits = y_limits)

    return(list(swc = swc_plot, cor = cor_plot))
  }
}

plot_swc_both_gg <- function(models, soil, measured_data) {

  theta_FC = medfate::soil.thetaFC(soil)

    # get the measured layers
  swc_vars <- c(
    'SWC',
    names(measured_data)[stringr::str_detect(names(measured_data), '^SWC_[0-9]$')]
  )

  dates <- character(0)
  SWC_vals_complex <- numeric(0)
  SWC_vals_simple <- numeric(0)
  Layer <- character(0)

  # data
  for (name in swc_vars) {
    index <- which(swc_vars == name)
    dates <- c(dates, as.character(models[['complex']][['Dates']]))
    SWC_vals_complex <- c(SWC_vals_complex,
                          models[['complex']][[paste0('W.', index)]] * theta_FC[index])
    SWC_vals_simple <- c(SWC_vals_simple,
                       models[['simple']][[paste0('W.', index)]] * theta_FC[index])
    Layer <- c(Layer, rep(name, length(measured_data[[name]])))
  }

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = SWC_vals_complex,
    Simple  = SWC_vals_simple,
    Layer = Layer,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Simple)) +
    facet_grid(Layer ~ .) +
    theme_medfate()


  swc_plot <- plot_data %>%
    tidyr::gather(key = Model, value = SWC, -Date, -Layer) %>%
    ggplot(aes(x = Date, y = SWC, colour = Model, linetype = Layer)) +
    facet_grid(Layer ~ .) +
    scale_color_manual(values = c('green', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # if there is measured values, populate the plots

  y_limits <- c(
    min(min(SWC_vals_complex, na.rm = TRUE),
        min(SWC_vals_simple, na.rm = TRUE)) - (min(min(SWC_vals_complex, na.rm = TRUE),
                                                   min(SWC_vals_simple, na.rm = TRUE)))*0.05,
    max(max(SWC_vals_complex, na.rm = TRUE),
        max(SWC_vals_simple, na.rm = TRUE)) + (max(max(SWC_vals_complex, na.rm = TRUE),
                                                   max(SWC_vals_simple, na.rm = TRUE)))*0.05
  )

  swc_plot <- swc_plot +
    geom_line(alpha = 0.4, size = 0.7) +
    geom_point(shape = 20, alpha = 0.3, size = 2) +
    scale_y_continuous(limits = y_limits)

  cor_plot <- cor_plot +
    geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
    geom_point(shape = 20, alpha = 0.8, colour = 'black') +
    stat_smooth(method = 'lm', colour = 'black',
                size = 1, se = FALSE, alpha = 0.4) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(limits = y_limits)

  return(list(swc = swc_plot, cor = cor_plot))
}

#' Title
#'
#' @param models
#'
#' @return
#' @export
#'
#' @examples
plot_swc_layers_gg <- function(models) {

  # simple
  if (!is.null(models[['simple']])) {
    simple_data <- models[['simple']] %>%
      dplyr::mutate(Dates = as.Date(Dates)) %>%
      dplyr::select(Dates, dplyr::starts_with('W.')) %>%
      tidyr::gather(Layer, SWC, -Dates) %>%
      dplyr::mutate(Model = 'Simple')
  } else {
    simple_data <- data.frame(
      Dates = NA, Layer = NA, SWC  = NA, Model = 'Simple',
      stringsAsFactors = FALSE
    )
  }

  # complex
  if (!is.null(models[['complex']])) {
    complex_data <- models[['complex']] %>%
      dplyr::mutate(Dates = as.Date(Dates)) %>%
      dplyr::select(Dates, dplyr::starts_with('W.')) %>%
      tidyr::gather(Layer, SWC, -Dates) %>%
      dplyr::mutate(Model = 'Complex')
  } else {
    complex_data <- data.frame(
      Dates = NA, Layer = NA, SWC  = NA, Model = NA,
      stringsAsFactors = FALSE
    )
  }

  # both united
  plot_data <- dplyr::bind_rows(simple_data, complex_data) %>%
    dplyr::filter(!is.na(Model))

  # plot
  swc_layers_plot <- ggplot(plot_data,
                            aes(x = Dates, y = SWC,
                                linetype = Layer, color = Model)) +
    geom_line() +
    scale_color_manual(values = c('green', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    facet_grid(. ~ Model) +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # return the plot
  return(swc_layers_plot)

}

plot_eplanttot_simple_gg <- function(models, measured_data, leaf_norm) {

  # data
  dates <- models[['simple']][['Dates']]

  # normalization if needed
  if (leaf_norm) {
    Etot_vals_simple <- models[['simple']][['Eplanttot']] / models[['simple']][['LAI_tot']]
    Etot_vals_meas <- measured_data[['Eplanttot']] / models[['simple']][['LAI_tot']]
  } else {
    Etot_vals_simple <- models[['simple']][['Eplanttot']]
    Etot_vals_meas <- measured_data[['Eplanttot']]
  }

  plot_data <- data.frame(
    Date = as.Date(dates),
    Simple = Etot_vals_simple,
    Measured  = Etot_vals_meas,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Simple, y = Measured)) +
    theme_medfate()


  etot_plot <- plot_data %>%
    tidyr::gather(key = Model, value = Etot, -Date) %>%
    ggplot(aes(x = Date, y = Etot, colour = Model)) +
    scale_color_manual(values = c('red', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(Etot_vals_meas))) {

    return(list(etot = etot_plot, cor = cor_plot))

  } else {

    # build the limits
    y_limits <- c(
      min(min(Etot_vals_simple, na.rm = TRUE),
          min(Etot_vals_meas, na.rm = TRUE)) - (min(min(Etot_vals_simple, na.rm = TRUE),
                                                    min(Etot_vals_meas, na.rm = TRUE)))*0.05,
      max(max(Etot_vals_simple, na.rm = TRUE),
          max(Etot_vals_meas, na.rm = TRUE)) + (max(max(Etot_vals_simple, na.rm = TRUE),
                                                    max(Etot_vals_meas, na.rm = TRUE)))*0.05
    )

    # if there is measured values, populate the plots
    etot_plot <- etot_plot +
      geom_line(alpha = 0.4, size = 0.7) +
      scale_y_continuous(limits = y_limits)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4) +
      scale_y_continuous(limits = y_limits) +
      scale_x_continuous(limits = y_limits)

    return(list(etot = etot_plot, cor = cor_plot))
  }
}

plot_eplanttot_complex_gg <- function(models, measured_data, leaf_norm) {

  # data
  dates <- models[['complex']][['Dates']]

  # normalization if needed
  if (leaf_norm) {
    Etot_vals_complex <- models[['complex']][['Eplanttot']] / models[['complex']][['LAI_tot']]
    Etot_vals_meas <- measured_data[['Eplanttot']] / models[['complex']][['LAI_tot']]
  } else {
    Etot_vals_complex <- models[['complex']][['Eplanttot']]
    Etot_vals_meas <- measured_data[['Eplanttot']]
  }

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = Etot_vals_complex,
    Measured  = Etot_vals_meas,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Measured)) +
    theme_medfate()


  etot_plot <- plot_data %>%
    tidyr::gather(key = Model, value = Etot, -Date) %>%
    ggplot(aes(x = Date, y = Etot, colour = Model)) +
    scale_color_manual(values = c('green', 'red')) +
    scale_x_date(date_breaks = '4 months') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # check if there are measured vals, if not, return an empty plot
  if (all(is.na(Etot_vals_meas))) {

    return(list(etot = etot_plot, cor = cor_plot))

  } else {

    # build the limits
    y_limits <- c(
      min(min(Etot_vals_complex, na.rm = TRUE),
          min(Etot_vals_meas, na.rm = TRUE)) - (min(min(Etot_vals_complex, na.rm = TRUE),
                                                    min(Etot_vals_meas, na.rm = TRUE)))*0.05,
      max(max(Etot_vals_complex, na.rm = TRUE),
          max(Etot_vals_meas, na.rm = TRUE)) + (max(max(Etot_vals_complex, na.rm = TRUE),
                                                    max(Etot_vals_meas, na.rm = TRUE)))*0.05
    )

    # if there is measured values, populate the plots
    etot_plot <- etot_plot +
      geom_line(alpha = 0.4, size = 0.7) +
      scale_y_continuous(limits = y_limits)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4) +
      scale_y_continuous(limits = y_limits) +
      scale_x_continuous(limits = y_limits)

    return(list(etot = etot_plot, cor = cor_plot))
  }
}

plot_eplanttot_both_gg <- function(models, measured_data, leaf_norm) {

  # data
  dates <- models[['simple']][['Dates']]

  # normalization if needed
  if (leaf_norm) {
    Eplanttot_vals_complex <- models[['complex']][['Eplanttot']] / models[['complex']][['LAI_tot']]
    Eplanttot_vals_simple <- models[['simple']][['Eplanttot']] / models[['simple']][['LAI_tot']]
  } else {
    Eplanttot_vals_complex <- models[['complex']][['Eplanttot']]
    Eplanttot_vals_simple <- models[['simple']][['Eplanttot']]
  }

  plot_data <- data.frame(
    Date = as.Date(dates),
    Complex = Eplanttot_vals_complex,
    Simple  = Eplanttot_vals_simple,
    stringsAsFactors = FALSE
  )

  # empty plots
  cor_plot <- ggplot(data = plot_data,
                     aes(x = Complex, y = Simple)) +
    theme_medfate()


  etot_plot <- plot_data %>%
    tidyr::gather(key = Model, value = Etot, -Date) %>%
    ggplot(aes(x = Date, y = Etot, colour = Model)) +
    scale_color_manual(values = c('green', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    )

  # if there is measured values, populate the plots
  y_limits <- c(
    min(min(Eplanttot_vals_complex, na.rm = TRUE),
        min(Eplanttot_vals_simple, na.rm = TRUE)) - (min(min(Eplanttot_vals_complex, na.rm = TRUE),
                                                         min(Eplanttot_vals_simple, na.rm = TRUE)))*0.05,
    max(max(Eplanttot_vals_complex, na.rm = TRUE),
        max(Eplanttot_vals_simple, na.rm = TRUE)) + (max(max(Eplanttot_vals_complex, na.rm = TRUE),
                                                         max(Eplanttot_vals_simple, na.rm = TRUE)))*0.05
  )
  etot_plot <- etot_plot +
    geom_line(alpha = 0.4, size = 0.7) +
    scale_y_continuous(limits = y_limits)

  cor_plot <- cor_plot +
    geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
    geom_point(shape = 20, alpha = 0.8, colour = 'black') +
    stat_smooth(method = 'lm', colour = 'black',
                size = 1, se = FALSE, alpha = 0.4) +
    scale_y_continuous(limits = y_limits)

  return(list(etot = etot_plot, cor = cor_plot))
}

plot_cohort_simple_gg <- function(models, measured_data, leaf_norm) {

  coh_names <- as.character(
    na.omit(stringr::str_extract(names(models[['simple']]), '^E_.+'))
  )

  # lai names
  if (leaf_norm) {
    lai_names <- as.character(
      na.omit(stringr::str_extract(names(models[['simple']]), '^LAI_.+'))
    )
  }


  dates <- models[['simple']][['Dates']]

  list_ecoh_plots <- vector("list", length(coh_names))
  names(list_ecoh_plots) <- coh_names
  list_cor_plots <- vector("list", length(coh_names))
  names(list_cor_plots) <- coh_names

  # loop by cohort
  for (i in 1:length(coh_names)) {

    # normalize E value if needed
    if (leaf_norm) {
      E_vals_simple <- models[['simple']][[coh_names[i]]] / models[['simple']][[lai_names[i]]]
      E_vals_meas <- measured_data[[coh_names[i]]] / models[['simple']][[lai_names[i]]]
    } else {
      E_vals_simple <- models[['simple']][[coh_names[i]]]
      E_vals_meas <- measured_data[[coh_names[i]]]
    }

    plot_data <- data.frame(
      Date = as.Date(dates),
      Simple = E_vals_simple,
      Measured  = E_vals_meas,
      stringsAsFactors = FALSE
    )

    # empty plots
    cor_plot <- ggplot(data = plot_data,
                       aes(x = Simple, y = Measured)) +
      labs(title = coh_names[i]) +
      theme_medfate()


    ecoh_plot <- plot_data %>%
      tidyr::gather(key = Model, value = Ecoh, -Date) %>%
      ggplot(aes(x = Date, y = Ecoh, colour = Model)) +
      scale_color_manual(values = c('red', 'blue')) +
      scale_x_date(date_breaks = '4 months') +
      labs(title = coh_names[i]) +
      theme_medfate() +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(angle = 25)
      )

    # check if there are measured vals, if not, return an empty plot
    if (all(is.na(E_vals_meas))) {

      list_ecoh_plots[[coh_names[i]]] <- ecoh_plot
      list_cor_plots[[coh_names[i]]] <- cor_plot

    } else {
      # build the limits
      y_limits <- c(
        min(min(E_vals_simple, na.rm = TRUE),
            min(E_vals_meas, na.rm = TRUE)) - (min(min(E_vals_simple, na.rm = TRUE),
                                                   min(E_vals_meas, na.rm = TRUE)))*0.05,
        max(max(E_vals_simple, na.rm = TRUE),
            max(E_vals_meas, na.rm = TRUE)) + (max(max(E_vals_simple, na.rm = TRUE),
                                                   max(E_vals_meas, na.rm = TRUE)))*0.05
      )

      # if there is measured values, populate the plots
      ecoh_plot <- ecoh_plot +
        geom_line(alpha = 0.4, size = 0.7) +
        scale_y_continuous(limits = y_limits)

      cor_plot <- cor_plot +
        geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
        geom_point(shape = 20, alpha = 0.8, colour = 'black') +
        stat_smooth(method = 'lm', colour = 'black',
                    size = 1, se = FALSE, alpha = 0.4) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = y_limits)

      list_ecoh_plots[[coh_names[i]]] <- ecoh_plot
      list_cor_plots[[coh_names[i]]] <- cor_plot
    }
  }

  names(list_ecoh_plots) <- coh_names
  names(list_cor_plots) <- coh_names

  return(list(ecoh = list_ecoh_plots, cor = list_cor_plots))
}

plot_cohort_complex_gg <- function(models, measured_data, leaf_norm) {

  coh_names <- as.character(
    na.omit(stringr::str_extract(names(models[['complex']]), '^E_.+'))
  )

  # lai names
  if (leaf_norm) {
    lai_names <- as.character(
      na.omit(stringr::str_extract(names(models[['complex']]), '^LAI_.+'))
    )
  }

  dates <- models[['complex']][['Dates']]

  list_ecoh_plots <- vector("list", length(coh_names))
  names(list_ecoh_plots) <- coh_names
  list_cor_plots <- vector("list", length(coh_names))
  names(list_cor_plots) <- coh_names

  # loop by cohort
  for (i in 1:length(coh_names)) {

    # normalize E values if needed
    if (leaf_norm) {
      E_vals_complex <- models[['complex']][[coh_names[i]]] / models[['complex']][[lai_names[i]]]
      E_vals_meas <- measured_data[[coh_names[i]]] / models[['complex']][[lai_names[i]]]
    } else {
      E_vals_complex <- models[['complex']][[coh_names[i]]]
      E_vals_meas <- measured_data[[coh_names[i]]]
    }

    plot_data <- data.frame(
      Date = as.Date(dates),
      Complex = E_vals_complex,
      Measured  = E_vals_meas,
      stringsAsFactors = FALSE
    )

    # empty plots
    cor_plot <- ggplot(data = plot_data,
                       aes(x = Complex, y = Measured)) +
      labs(title = coh_names[i]) +
      theme_medfate()


    ecoh_plot <- plot_data %>%
      tidyr::gather(key = Model, value = Ecoh, -Date) %>%
      ggplot(aes(x = Date, y = Ecoh, colour = Model)) +
      scale_color_manual(values = c('green', 'red')) +
      scale_x_date(date_breaks = '4 months') +
      labs(title = coh_names[i]) +
      theme_medfate() +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(angle = 25)
      )

    # check if there are measured vals, if not, return an empty plot
    if (all(is.na(E_vals_meas))) {

      list_ecoh_plots[[coh_names[i]]] <- ecoh_plot
      list_cor_plots[[coh_names[i]]] <- cor_plot

    } else {

      # build the limits
      y_limits <- c(
        min(min(E_vals_complex, na.rm = TRUE),
            min(E_vals_meas, na.rm = TRUE)) - (min(min(E_vals_complex, na.rm = TRUE),
                                                   min(E_vals_meas, na.rm = TRUE)))*0.05,
        max(max(E_vals_complex, na.rm = TRUE),
            max(E_vals_meas, na.rm = TRUE)) + (max(max(E_vals_complex, na.rm = TRUE),
                                                   max(E_vals_meas, na.rm = TRUE)))*0.05
      )

      # if there is measured values, populate the plots
      ecoh_plot <- ecoh_plot +
        geom_line(alpha = 0.4, size = 0.7) +
        scale_y_continuous(limits = y_limits)

      cor_plot <- cor_plot +
        geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
        geom_point(shape = 20, alpha = 0.8, colour = 'black') +
        stat_smooth(method = 'lm', colour = 'black',
                    size = 1, se = FALSE, alpha = 0.4) +
        scale_y_continuous(limits = y_limits) +
        scale_x_continuous(limits = y_limits)

      list_ecoh_plots[[coh_names[i]]] <- ecoh_plot
      list_cor_plots[[coh_names[i]]] <- cor_plot
    }
  }

  names(list_ecoh_plots) <- coh_names
  names(list_cor_plots) <- coh_names

  return(list(ecoh = list_ecoh_plots, cor = list_cor_plots))
}

plot_cohort_both_gg <- function(models, measured_data, leaf_norm) {

  coh_names <- as.character(
    na.omit(stringr::str_extract(names(models[['simple']]), '^E_.+'))
  )

  # lai names
  if (leaf_norm) {
    lai_names <- as.character(
      na.omit(stringr::str_extract(names(models[['complex']]), '^LAI_.+'))
    )
  }

  dates <- models[['simple']][['Dates']]

  list_ecoh_plots <- vector("list", length(coh_names))
  names(list_ecoh_plots) <- coh_names
  list_cor_plots <- vector("list", length(coh_names))
  names(list_cor_plots) <- coh_names

  # loop by cohort
  for (i in 1:length(coh_names)) {

    # Normalize E if needed
    if (leaf_norm) {
      E_vals_simple <- models[['simple']][[coh_names[i]]] / models[['simple']][[lai_names[i]]]
      E_vals_complex <- models[['complex']][[coh_names[i]]] / models[['simple']][[lai_names[i]]]
    } else {
      E_vals_simple <- models[['simple']][[coh_names[i]]]
      E_vals_complex <- models[['complex']][[coh_names[i]]]
    }

    plot_data <- data.frame(
      Date = as.Date(dates),
      Simple = E_vals_simple,
      Complex  = E_vals_complex,
      stringsAsFactors = FALSE
    )

    # empty plots
    cor_plot <- ggplot(data = plot_data,
                       aes(x = Complex, y = Simple)) +
      labs(title = coh_names[i]) +
      theme_medfate()


    ecoh_plot <- plot_data %>%
      tidyr::gather(key = Model, value = Ecoh, -Date) %>%
      ggplot(aes(x = Date, y = Ecoh, colour = Model)) +
      scale_color_manual(values = c('green', 'blue')) +
      scale_x_date(date_breaks = '4 months') +
      labs(title = coh_names[i]) +
      theme_medfate() +
      theme(
        legend.position = 'top',
        axis.text.x = element_text(angle = 25)
      )

    y_limits <- c(
      min(min(E_vals_complex, na.rm = TRUE),
          min(E_vals_simple, na.rm = TRUE)) - (min(min(E_vals_complex, na.rm = TRUE),
                                                   min(E_vals_simple, na.rm = TRUE)))*0.05,
      max(max(E_vals_complex, na.rm = TRUE),
          max(E_vals_simple, na.rm = TRUE)) + (max(max(E_vals_complex, na.rm = TRUE),
                                                   max(E_vals_simple, na.rm = TRUE)))*0.05
    )

    # check if there are measured vals, if not, return an empty plot
    ecoh_plot <- ecoh_plot +
      geom_line(alpha = 0.4, size = 0.7) +
      scale_y_continuous(limits = y_limits)

    cor_plot <- cor_plot +
      geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
      geom_point(shape = 20, alpha = 0.8, colour = 'black') +
      stat_smooth(method = 'lm', colour = 'black',
                  size = 1, se = FALSE, alpha = 0.4) +
      scale_y_continuous(limits = y_limits) +
      scale_x_continuous(limits = y_limits)

    list_ecoh_plots[[coh_names[i]]] <- ecoh_plot
    list_cor_plots[[coh_names[i]]] <- cor_plot
  }

  names(list_ecoh_plots) <- coh_names
  names(list_cor_plots) <- coh_names

  return(list(ecoh = list_ecoh_plots, cor = list_cor_plots))
}

plot_temp_complex_gg <- function(models, measured_data, meteo_data) {

  max_data <- data.frame(
    stringsAsFactors = FALSE,
    Date = as.Date(models[['complex']][['Dates']]),
    Measured = measured_data[['Temp_max']],
    Complex = models[['complex']][['Tcan_max']],
    Meteo = meteo_data[['MaxTemperature']]
  )

  y_limits <- c(
    min(min(max_data['Complex'], na.rm = TRUE),
        min(max_data[['Measured']], na.rm = TRUE),
        min(max_data[['Meteo']])) - (min(min(max_data['Complex'], na.rm = TRUE),
                                         min(max_data[['Measured']], na.rm = TRUE),
                                         min(max_data[['Meteo']])))*0.05,
    max(max(max_data['Complex'], na.rm = TRUE),
        max(max_data[['Measured']], na.rm = TRUE),
        max(max_data[['Meteo']])) + (max(max(max_data['Complex'], na.rm = TRUE),
                                         max(max_data[['Measured']], na.rm = TRUE),
                                         max(max_data[['Meteo']])))*0.05
  )

  # plots
  max_cor_plot <- ggplot(data = max_data,
                     aes(x = Complex, y = Measured)) +
    labs(title = 'Max Temp.', x = 'Complex (Black) / Atm (blue)') +
    geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
    geom_point(shape = 20, alpha = 0.8, colour = 'black') +
    stat_smooth(method = 'lm', colour = 'black',
                size = 1, se = FALSE, alpha = 0.4) +
    geom_point(aes(x = Meteo), shape = 15, colour = 'blue', alpha = 0.8) +
    stat_smooth(aes(x = Meteo), method = 'lm', colour = 'blue', size = 1,
                se = FALSE, alpha = 0.4) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(limits = y_limits) +
    theme_medfate()


  max_plot <- max_data %>%
    tidyr::gather(key = Model, value = Temperature, -Date) %>%
    ggplot(aes(x = Date, y = Temperature, colour = Model)) +
    scale_color_manual(values = c('green', 'red', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    labs(title = 'Max Temp.') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    ) +
    geom_line(alpha = 0.4, size = 0.7) +
    scale_y_continuous(limits = y_limits)

  min_data <- data.frame(
    stringsAsFactors = FALSE,
    Date = as.Date(models[['complex']][['Dates']]),
    Measured = measured_data[['Temp_min']],
    Complex = models[['complex']][['Tcan_min']],
    Meteo = meteo_data[['MinTemperature']]
  )

  y_limits <- c(
    min(min(min_data['Complex'], na.rm = TRUE),
        min(min_data[['Measured']], na.rm = TRUE),
        min(min_data[['Meteo']])) - (min(min(min_data['Complex'], na.rm = TRUE),
                                         min(min_data[['Measured']], na.rm = TRUE),
                                         min(min_data[['Meteo']])))*0.05,
    max(max(min_data['Complex'], na.rm = TRUE),
        max(min_data[['Measured']], na.rm = TRUE),
        max(min_data[['Meteo']])) + (max(max(min_data['Complex'], na.rm = TRUE),
                                         max(min_data[['Measured']], na.rm = TRUE),
                                         max(min_data[['Meteo']])))*0.05
  )

  # empty plots
  min_cor_plot <- ggplot(data = min_data,
                         aes(x = Complex, y = Measured)) +
    labs(title = 'Min Temp.', x = 'Complex (Black) / Atm (blue)') +
    geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
    geom_point(shape = 20, alpha = 0.8, colour = 'black') +
    stat_smooth(method = 'lm', colour = 'black',
                size = 1, se = FALSE, alpha = 0.4) +
    geom_point(aes(y = Meteo), shape = 15, colour = 'blue', alpha = 0.8) +
    stat_smooth(aes(y = Meteo), method = 'lm', colour = 'blue', size = 1,
                se = FALSE, alpha = 0.4) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(limits = y_limits) +
    theme_medfate()


  min_plot <- min_data %>%
    tidyr::gather(key = Model, value = Temperature, -Date) %>%
    ggplot(aes(x = Date, y = Temperature, colour = Model)) +
    scale_color_manual(values = c('green', 'red', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    labs(title = 'Min Temp.') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    ) +
    geom_line(alpha = 0.4, size = 0.7) +
    scale_y_continuous(limits = y_limits)

  mean_data <- data.frame(
    stringsAsFactors = FALSE,
    Date = as.Date(models[['complex']][['Dates']]),
    Measured = measured_data[['Temp_mean']],
    Complex = models[['complex']][['Tcan_mean']],
    Meteo  = meteo_data[['MeanTemperature']]
  )

  y_limits <- c(
    min(min(mean_data['Complex'], na.rm = TRUE),
        min(mean_data[['Measured']], na.rm = TRUE),
        min(mean_data[['Meteo']])) - (min(min(mean_data['Complex'], na.rm = TRUE),
                                         min(mean_data[['Measured']], na.rm = TRUE),
                                         min(mean_data[['Meteo']])))*0.05,
    max(max(mean_data['Complex'], na.rm = TRUE),
        max(mean_data[['Measured']], na.rm = TRUE),
        max(mean_data[['Meteo']])) + (max(max(mean_data['Complex'], na.rm = TRUE),
                                         max(mean_data[['Measured']], na.rm = TRUE),
                                         max(mean_data[['Meteo']])))*0.05
  )

  # empty plots
  mean_cor_plot <- ggplot(data = mean_data,
                         aes(x = Complex, y = Measured)) +
    labs(title = 'Mean Temp.', x = 'Complex (Black) / Atm (blue)') +
    geom_abline(slope = 1, intercept = 0, colour = 'lightgreen', size = 0.8) +
    geom_point(shape = 20, alpha = 0.8, colour = 'black') +
    stat_smooth(method = 'lm', colour = 'black',
                size = 1, se = FALSE, alpha = 0.4) +
    geom_point(aes(y = Meteo), shape = 15, colour = 'blue', alpha = 0.8) +
    stat_smooth(aes(y = Meteo), method = 'lm', colour = 'blue', size = 1,
                se = FALSE, alpha = 0.4) +
    scale_y_continuous(limits = y_limits) +
    scale_x_continuous(limits = y_limits) +
    theme_medfate()


  mean_plot <- mean_data %>%
    tidyr::gather(key = Model, value = Temperature, -Date) %>%
    ggplot(aes(x = Date, y = Temperature, colour = Model)) +
    scale_color_manual(values = c('green', 'red', 'blue')) +
    scale_x_date(date_breaks = '4 months') +
    labs(title = 'Mean Temp.') +
    theme_medfate() +
    theme(
      legend.position = 'top',
      axis.text.x = element_text(angle = 25)
    ) +
    geom_line(alpha = 0.4, size = 0.7) +
    scale_y_continuous(limits = y_limits)

  list_temp_plots <- list(Min = min_plot, Max = max_plot, Mean = mean_plot)
  list_cor_plots <- list(Min = min_cor_plot, Max = max_cor_plot, Mean = mean_cor_plot)


  return(list(temp = list_temp_plots, cor = list_cor_plots))
}


#' Plotting the model evaluation results
#'
#' Functions to generate model evaluation plots in order to inspect the results in a
#' visual way
#'
#' @param variable A string indicating the variable to visualize:
#'   \itemize{
#'     \item{\code{SWC} : Soil water content.}
#'     \item{\code{Eplanttot} : Overall plant transpiration.}
#'     \item{\code{E_by_Cohort} : Plant transpiration by cohort.}
#'   }
#'
#' @param models List with the data frames with the results of the models as
#'   generated by \code{\link{saveRes}}
#'
#' @param soil Soil object used in the model
#'
#' @param measured_data Measured data dataframe
#'
#' @param mode Transpiration mode indicated in the models ('simple', 'complex' or 'both')
#'
#' @param leaf_norm Boolean indicating if transpiration must be normalized by
#'   leaf area previously to plot it. (Only necessary when plotting Transpiration)
#'
#' @export
plot_res_gg <- function(variable, models, soil, measured_data, mode, leaf_norm = FALSE) {

  # SWC
  if (variable == 'SWC') {

    if (mode == 'both') {
      # get the plots
      simple <- plot_swc_simple_gg(models, soil, measured_data)
      complex <- plot_swc_complex_gg(models, soil, measured_data)
      both <- plot_swc_both_gg(models, soil, measured_data)

      # build the cowplot
      return(cowplot::plot_grid(
        simple[['swc']], simple[['cor']],
        complex[['swc']], complex[['cor']],
        both[['swc']], both[['cor']],
        ncol = 2, align = 'h', axis = "tblr"
      ))
    }

    if (mode == 'simple') {
      # get the plots
      simple <- plot_swc_simple_gg(models, soil, measured_data)

      # build the cowplot
      return(cowplot::plot_grid(
        plotlist = simple, ncol = 2, align = 'h', axis = "tblr"
      ))
    }

    if (mode == 'complex') {
      # get the plots
      complex <- plot_swc_complex_gg(models, soil, measured_data)

      # build the cowplot
      return(cowplot::plot_grid(
        plotlist = complex, ncol = 2, align = 'h', axis = "tblr"
      ))
    }
  }

  if (variable == 'Eplanttot') {

    if (mode == 'both') {
      # get the plots
      simple <- plot_eplanttot_simple_gg(models, measured_data, leaf_norm)
      complex <- plot_eplanttot_complex_gg(models, measured_data, leaf_norm)
      both <- plot_eplanttot_both_gg(models, measured_data, leaf_norm)

      # build the cowplot
      return(cowplot::plot_grid(
        simple[['etot']], simple[['cor']],
        complex[['etot']], complex[['cor']],
        both[['etot']], both[['cor']],
        ncol = 2, align = 'h', axis = "tblr"
      ))
    }

    if (mode == 'simple') {
      # get the plots
      simple <- plot_eplanttot_simple_gg(models, measured_data, leaf_norm)

      # build the cowplot
      return(cowplot::plot_grid(
        plotlist = simple, ncol = 2, align = 'h', axis = "tblr"
      ))
    }

    if (mode == 'complex') {
      # get the plots
      complex <- plot_eplanttot_complex_gg(models, measured_data, leaf_norm)

      # build the cowplot
      return(cowplot::plot_grid(
        plotlist = complex, ncol = 2, align = 'h', axis = "tblr"
      ))
    }
  }

  if (variable == 'E_by_Cohort') {

    if (mode == 'both') {
      # get the plot lists
      simple <- plot_cohort_simple_gg(models, measured_data, leaf_norm)
      complex <- plot_cohort_complex_gg(models, measured_data, leaf_norm)
      both <- plot_cohort_both_gg(models, measured_data, leaf_norm)

      # build the cowplot
      n_coh <- length(simple[['ecoh']])

      res <- list(
        simple_ecoh = cowplot::plot_grid(
          plotlist = simple[['ecoh']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        ),
        simple_cor = cowplot::plot_grid(
          plotlist = simple[['cor']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        ),
        complex_ecoh = cowplot::plot_grid(
          plotlist = complex[['ecoh']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        ),
        complex_cor = cowplot::plot_grid(
          plotlist = complex[['cor']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        ),
        both_ecoh = cowplot::plot_grid(
          plotlist = both[['ecoh']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        ),
        both_cor = cowplot::plot_grid(
          plotlist = both[['cor']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        )
      )

      return(res)
    }

    if (mode == 'simple') {
      # get the plot lists
      simple <- plot_cohort_simple_gg(models, measured_data, leaf_norm)

      # build the cowplot
      n_coh <- length(simple[['ecoh']])

      res <- list(
        simple_ecoh = cowplot::plot_grid(
          plotlist = simple[['ecoh']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        ),
        simple_cor = cowplot::plot_grid(
          plotlist = simple[['cor']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        )
      )

      return(res)
    }

    if (mode == 'complex') {
      # get the plot lists
      complex <- plot_cohort_complex_gg(models, measured_data, leaf_norm)

      # build the cowplot
      n_coh <- length(complex[['ecoh']])

      res <- list(
        complex_ecoh = cowplot::plot_grid(
          plotlist = complex[['ecoh']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        ),
        complex_cor = cowplot::plot_grid(
          plotlist = complex[['cor']],
          ncol = if (n_coh < 4) {n_coh} else {3},
          nrow = if (n_coh < 4) {1} else {ceiling(n_coh/3)},
          align = 'h', axis = "tblr"
        )
      )

      return(res)
    }
  }
}


#' @param eval Evaluation object returned by function \code{\link{eval_site}}
#'
#' @name plot_res_gg
#'
#' @export
plot_eval_gg<-function(variable, eval, leaf_norm = FALSE) {
  plot_res_gg(variable, eval$models_dfs, eval$soil, eval$measured, eval$mode, leaf_norm=leaf_norm)
}



#' ggplot2 theme for the reports plots
#'
#' @export

theme_medfate <- function(base_size = 11, base_family = "")
{
  half_line <- base_size/2
  theme(line = element_line(colour = "black", size = 0.5, linetype = 1,
                            lineend = "butt"),
        rect = element_rect(fill = NA,
                            colour = "black", size = 0.5, linetype = 1),
        text = element_text(family = base_family,
                            face = "plain", colour = "black", size = base_size,
                            lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                            angle = 0, margin = margin(), debug = FALSE),
        title = element_text(),
        axis.line = element_line(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line),
                                   vjust = 1),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line),
                                   hjust = 1),
        axis.ticks = element_line(colour = "black"),
        axis.ticks.length = unit(half_line, "pt"),
        axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
                                                    b = 0.8 * half_line),
                                    hjust = 0.85),
        axis.title.y = element_text(angle = 90,
                                    margin = margin(r = 1.1 * half_line, l = 0.8 * half_line),
                                    hjust = 0.85),
        legend.background = element_rect(colour = NA),
        legend.margin = margin(t = half_line/4, r = half_line/4,
                               b = 0, l = half_line/4, unit = "pt"),
        legend.key = element_rect(colour = NA),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.8)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0),
        legend.title.align = NULL,
        legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = c(0.9,1),
        legend.box = NULL,
        # legend.spacing = unit(0.2, 'mm'),
        # legend.spacing.x = unit(0.2, 'mm'),
        # legend.spacing.y = NULL,
        panel.background = element_rect(color = NA),
        panel.border = element_rect(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(half_line, "pt"),
        panel.ontop = FALSE,
        strip.background = element_rect(colour = NA),
        strip.placement = 'inside',
        strip.text = element_text(colour = "black", size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line, b = half_line)),
        strip.text.y = element_text(angle = -90,
                                    margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_rect(colour = NA),
        plot.title = element_text(size = rel(1.2),
                                  margin = margin(b = half_line/2),
                                  hjust = 0.1),
        plot.subtitle = element_text(size = rel(0.8), hjust = 0.1,
                                     margin = margin(b = half_line/2)),
        plot.margin = margin(half_line, half_line, half_line, half_line),
        complete = TRUE)
}
