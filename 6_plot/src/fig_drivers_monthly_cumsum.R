fig_drivers_monthly_cumsum <- function(fig_ind, transparent, drivers_file, fig_cfg_yml, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  drivers <- readRDS(drivers_file) %>%
    group_by(month, var, period) %>%
    summarise(mean = mean(var_value),
              min = min(var_value),
              max = max(var_value)) %>%
    ungroup() %>%
    mutate(month = as.Date(paste('2001-',month,'-01', sep =''), format = '%Y-%m-%d'))

  drivers_sum <- readRDS(drivers_file) %>%
    group_by(var, period, gcm) %>%
    arrange(month) %>%
    mutate(cumsum = cumsum(var_value)) %>%
    ungroup() %>%
    mutate(month = as.Date(paste('2001-',month,'-01', sep =''), format = '%Y-%m-%d'))

  drivers_sum_retro = dplyr::filter(drivers_sum, period =='Retro')

  drivers_sum_future = dplyr::filter(drivers_sum, period != 'Retro') %>%
    left_join(drivers_sum_retro, by = c('month'='month','var'='var'), suffix = c('_future','_retro'))


  vars = c('Temp', 'Precip', 'Evap', 'Runoff', 'Baseflow', 'Runoff_and_baseflow')

  drivers_retro <- readRDS(drivers_file) %>%
    spread(key = var, value = var_value) %>%
    dplyr::filter(period == 'Retro') %>%
    summarise(Temp = mean(Temp),
              Precip = sum(Precip),
              Evap = sum(Evap),
              Runoff_and_baseflow = sum(Runoff_and_baseflow)) %>%
    mutate(tmp = 'a')

  drivers_future <- readRDS(drivers_file) %>%
    spread(key = var, value = var_value) %>%
    dplyr::filter(period != 'Retro') %>%
    group_by(period, gcm) %>%
    summarise(Temp = mean(Temp),
              Precip = sum(Precip),
              Evap = sum(Evap),
              Runoff_and_baseflow = sum(Runoff_and_baseflow)) %>%
    mutate(tmp = 'a') %>%
    left_join(drivers_retro, by = 'tmp', suffix = c('_future', '_retro')) %>%
    select(-tmp)


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

  month_precip = ggplot(dplyr::filter(drivers_sum, var == 'Precip'),
                        aes(x = month, y = cumsum, color = period, size = period, linetype = period, group = interaction(period, gcm))) +
    geom_line(show.legend = F, alpha = .6) +
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

  month_evap = ggplot(dplyr::filter(drivers_sum, var == 'Evap'),
                      aes(x = month, y = cumsum, color = period, size = period, group= interaction(period,gcm),linetype = period)) +
    geom_line(show.legend = F,alpha = .5) +
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


  month_r_b = ggplot(dplyr::filter(drivers_sum, var == 'Runoff_and_baseflow'),
                     aes(x = month, y = cumsum, color = period, size = period, linetype = period, group = interaction(period, gcm))) +
    geom_line(show.legend = F, alpha = transparent) +
    theme_classic() +
    ylab(bquote(Baseflow~+~Runoff~(mm))) +
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

  temp_diff = ggplot(drivers_future, aes(x = period, y = Temp_future - Temp_retro, color = period, fill = period,
                                  size =period, shape = period)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Temp~(degree~C))) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
                                 '2080s' = t_col(fig_config$period$`2080s`, transparent)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 5,
                                 '2080s' = 5),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  temp_diff

  precip_diff = ggplot(drivers_future, aes(x = period, y = Precip_future - Precip_retro, color = period, fill = period,
                                         size =period, shape = period)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Precip~(mm)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
                                 '2080s' = t_col(fig_config$period$`2080s`, transparent)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 5,
                                 '2080s' = 5),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  precip_diff

  evap_diff = ggplot(drivers_future, aes(x = period, y = Evap_future - Evap_retro, color = period, fill = period,
                                           size =period, shape = period)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Evap~(mm)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
                                 '2080s' = t_col(fig_config$period$`2080s`, transparent)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 5,
                                 '2080s' = 5),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  evap_diff

  r_b_diff = ggplot(drivers_future, aes(x = period, y = Runoff_and_baseflow_future - Runoff_and_baseflow_retro, color = period, fill = period,
                                           size =period, shape = period)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Runoff+Baseflow~(mm)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, transparent),
                                 '2080s' = t_col(fig_config$period$`2080s`, transparent)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 5,
                                 '2080s' = 5),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  r_b_diff

  g = ggdraw() +
    draw_plot(month_temp, x = 0, y = .5, width = .3, height = .3) +
    draw_plot(month_precip, x= .5, y= .5, width = .3, height = .3) +
    draw_plot(month_evap, x = 0, y = .2, width = .3, height = .3) +
    draw_plot(month_r_b, x= .5, y= .2, width = .3, height = .3)+
    draw_plot(temp_diff, x = .3, y = .55, width = .15, height = .2) +
    draw_plot(precip_diff, x = .8 , y = .55, width = .15, height = .2) +
    draw_plot(evap_diff, x = .3, y = .25, width = .15, height = .2) +
    draw_plot(r_b_diff, x = .8, y = .25, width = .15, height = .2)

  g

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 16, height = 12)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
