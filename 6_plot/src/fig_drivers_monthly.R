fig_drivers_monthly <- function(fig_ind, transparent, drivers_file, fig_cfg_yml, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  drivers <- readRDS(drivers_file) %>%
    group_by(month, var, period) %>%
    summarise(mean = mean(var_value),
              min = min(var_value),
              max = max(var_value)) %>%
    ungroup() %>%
    mutate(month = as.Date(paste('2001-',month,'-01', sep =''), format = '%Y-%m-%d'))

  vars = c('Temp', 'Precip', 'Evap', 'Runoff')

  month_temp = ggplot(dplyr::filter(drivers, var == 'Temp'), aes(x = month, y = mean, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(drivers, period != 'Retro', var == 'Temp'),
                aes(x = month, y = mean, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line() +
    theme_classic() +
    ylab(bquote(Air~Temperature~(C))) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.position = c(.15,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 10)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_precip = ggplot(dplyr::filter(drivers, var == 'Precip'), aes(x = month, y = mean, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(drivers, period != 'Retro', var == 'Precip'),
                aes(x = month, y = mean, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(Precipitation~(mm))) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.position = c(.15,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 10)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_evap = ggplot(dplyr::filter(drivers, var == 'Evap'), aes(x = month, y = mean, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(drivers, period != 'Retro', var == 'Evap'),
                aes(x = month, y = mean, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(Evapotranspiration~(mm))) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.position = c(.15,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 10)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_runoff = ggplot(dplyr::filter(drivers, var == 'Runoff'), aes(x = month, y = mean, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(drivers, period != 'Retro', var == 'Runoff'),
                aes(x = month, y = mean, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(Runoff~(mm))) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.position = c(.15,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 10)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))



  g = ggdraw() +
    draw_plot(month_temp, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(month_precip, x= .5, y= .5, width = .5, height = .5) +
    draw_plot(month_evap, x = 0, y = 0, width = .5, height = .5) +
    draw_plot(month_runoff, x= .5, y= 0, width = .5, height = .5)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 10, height = 9)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
