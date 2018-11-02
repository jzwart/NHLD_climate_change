fig_drivers_ice <- function(fig_ind, transparent, ice_dur_ind_file, remake_file, drivers_file, snow_file, fig_cfg_yml, basemap, gd_config){

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

  snow <- readRDS(snow_file)

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
    scale_x_date(labels = scales::date_format('%b')) +
    labs(tag = 'A')

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
    scale_x_date(labels = scales::date_format('%b'))+
    labs(tag = 'C')

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
    scale_x_date(labels = scales::date_format('%b'))+
    labs(tag = 'E')

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

  month_r_b = ggplot(dplyr::filter(drivers, var == 'Runoff_and_baseflow'), aes(x = month, y = mean, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(drivers, period != 'Retro', var == 'Runoff_and_baseflow'),
                aes(x = month, y = mean, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(Streamflow~(mm))) +
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
    scale_x_date(labels = scales::date_format('%b'))+
    labs(tag = 'G')

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
                       labels = c('2050\'s', '2080\'s'))+
    labs(tag = 'B')

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
                       labels = c('2050\'s', '2080\'s'))+
    labs(tag = 'D')

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
                       labels = c('2050\'s', '2080\'s'))+
    labs(tag = 'F')

  evap_diff

  r_b_diff = ggplot(drivers_future, aes(x = period, y = Runoff_and_baseflow_future - Runoff_and_baseflow_retro, color = period, fill = period,
                                           size =period, shape = period)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Streamflow~(mm)))+
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
                       labels = c('2050\'s', '2080\'s'))+
    labs(tag = 'H')

  r_b_diff

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  ice_data <- readRDS(sc_retrieve(ice_dur_ind_file, remake_file = remake_file)) %>% as_tibble() %>%
    mutate(day = lubridate::day(datetime),
           month = lubridate::month(datetime),
           month_day = strftime(datetime, format = '%b %d'),
           water_year = if_else(month == 9 & day == 30, 1, 0)) %>%
    group_by(gcm, period) %>%
    mutate(water_year = cumsum(c(F, diff(water_year) < 0 ))) %>%
    ungroup() %>%
    group_by(gcm, period, water_year) %>%
    mutate(water_day = 1:n()) %>%
    ungroup() %>%
    dplyr::filter(water_year >0, water_year<25)

  retro <- ice_data %>% dplyr::filter(period == 'Retro')
  scenarios <- ice_data %>% dplyr::filter(period != 'Retro')

  #couldn't figure out dplyr way so did for loops
  ice_on_off <- data.frame()
  for(gcm in unique(scenarios$gcm)){
    for(period in unique(scenarios$period)){
      for(wy in unique(scenarios$water_year)){
        cur = scenarios[scenarios$gcm == gcm & scenarios$period == period & scenarios$water_year == wy, ]
        out = data.frame(gcm = gcm, period = period, wy = wy, year = lubridate::year(cur$datetime[1]),
                         ice_on = cur$datetime[min(cur$water_day[cur$ice_on==1])],
                         ice_off = cur$datetime[max(cur$water_day[cur$ice_on == 1])],
                         ice_on_water_day = cur$water_day[min(cur$water_day[cur$ice_on==1])],
                         ice_off_water_day = cur$water_day[max(cur$water_day[cur$ice_on==1])])
        ice_on_off = rbind(ice_on_off, out)
      }
    }
  }
  for(wy in unique(retro$water_year)){
    cur = retro[retro$water_year == wy, ]
    out = data.frame(gcm = 'Retro', period = 'Retro', wy = wy, year = lubridate::year(cur$datetime[1]),
                     ice_on = cur$datetime[min(cur$water_day[cur$ice_on==1])],
                     ice_off = cur$datetime[max(cur$water_day[cur$ice_on == 1])],
                     ice_on_water_day = cur$water_day[min(cur$water_day[cur$ice_on==1])],
                     ice_off_water_day = cur$water_day[max(cur$water_day[cur$ice_on==1])])
    ice_on_off = rbind(ice_on_off, out)
  }
  ice <- ice_on_off %>% as_tibble() %>%
    mutate(ice_on_doy = lubridate::yday(ice_on),
           ice_off_doy = lubridate::yday(ice_off),
           ice_on_month_day = strftime(ice_on, format = '%b %d'),
           ice_off_month_day = strftime(ice_off, format = '%b %d'),
           ice_dur_days = difftime(ice_off, ice_on),
           mid_plot = case_when(period == 'Retro' ~ 0,
                                period == '2050s' ~ 2,
                                period == '2080s' ~ 4)) %>%
    group_by(period) %>%
    arrange(ice_on_water_day) %>%
    ungroup() #%>%
  # tidyr::gather(key = 'event', value = 'water_day', contains('water_day')) %>%
  # tidyr::gather(key = 'y_placement', value = 'y', contains('plot'))

  water_day_to_date = data.frame(water_day = ice_data$water_day[1:365], date = strftime(ice_data$datetime[1:365], format = '%b %d'))

  ice_summaries <- ice %>%
    group_by(period) %>%
    mutate(shortest_ice_on = ice_on_water_day[which(min(ice_dur_days,na.rm=T) == ice_dur_days)],
           shortest_ice_off = ice_off_water_day[which(min(ice_dur_days,na.rm=T) == ice_dur_days)],
           longest_ice_on = ice_on_water_day[which(max(ice_dur_days,na.rm=T) == ice_dur_days)],
           longest_ice_off = ice_off_water_day[which(max(ice_dur_days,na.rm=T) == ice_dur_days)],
           mean_ice_on = round(mean(ice_on_water_day, na.rm=T)),
           mean_ice_off = round(mean(ice_off_water_day, na.rm = T)),
           no_ice_years = sum(is.na(ice_on_water_day))) %>%
    summarise(shortest_ice_on = median(shortest_ice_on),
              shortest_ice_off = median(shortest_ice_off),
              longest_ice_on = median(longest_ice_on),
              longest_ice_off = median(longest_ice_off),
              mean_ice_on = mean(ice_on_water_day, na.rm=T),
              mean_ice_off = mean(ice_off_water_day, na.rm = T),
              no_ice_years = median(no_ice_years)) %>%
    ungroup() %>%
    arrange(factor(period, levels = c('Retro', '2050s', '2080s')))


  ice_days = ice_data %>%
    dplyr::filter(ice_on == 1)

  min_max_ice = ice %>% group_by(period) %>%
    summarise(min_ice_on = min(ice_on_water_day, na.rm = T),
              max_ice_on = max(ice_on_water_day, na.rm = T),
              min_ice_off = min(ice_off_water_day, na.rm = T),
              max_ice_off = max(ice_off_water_day, na.rm = T)) %>%
    ungroup()

  ice_ends <- min_max_ice %>%
    group_by(period) %>%
    gather(key = 'ice_on', value = 'loc_on', contains('_on')) %>%
    gather(key = 'ice_off', value = 'loc_off', contains('_off')) %>%
    ungroup() %>%
    mutate(dens_on = 0,
           dens_off = 0) %>%
    select(period, loc_on, dens_on, loc_off, dens_off)

  # calculating density of ice on /off days
  dens_ice_on <- ice %>%
    dplyr::filter(!is.na(ice_on_water_day), !is.na(ice_off_water_day)) %>% # getting rid of years without ice
    group_by(period) %>%
    do(data.frame(loc_on = density(.$ice_on_water_day)$x,
                  dens_on = density(.$ice_on_water_day)$y)) %>%
    bind_rows(select(ice_ends, period, loc_on, dens_on)) %>%
    mutate(dens_on = (dens_on - min(dens_on)) / (max(dens_on)-min(dens_on)), # normalizing to make all same width
           loc_on = round(loc_on)) %>%
    ungroup()%>%
    group_by(period, loc_on) %>%
    summarise(dens_on = mean(dens_on)) %>%
    ungroup() %>%
    mutate(min_ice_on = case_when(period == 'Retro' ~ min_max_ice$min_ice_on[min_max_ice$period=='Retro'],
                                  period == '2050s' ~ min_max_ice$min_ice_on[min_max_ice$period=='2050s'],
                                  period == '2080s' ~ min_max_ice$min_ice_on[min_max_ice$period=='2080s']),
           max_ice_on = case_when(period == 'Retro' ~ min_max_ice$max_ice_on[min_max_ice$period=='Retro'],
                                  period == '2050s' ~ min_max_ice$max_ice_on[min_max_ice$period=='2050s'],
                                  period == '2080s' ~ min_max_ice$max_ice_on[min_max_ice$period=='2080s'])) %>%
    group_by(period) %>%
    dplyr::filter(loc_on >= min_ice_on,
                  loc_on <= max_ice_on) %>%
    mutate(dens_on = case_when(loc_on <= min_ice_on ~ 0,
                               loc_on >= max_ice_on ~ 0,
                               TRUE ~ dens_on)) %>%
    ungroup() %>%
    mutate(dens_on = case_when(period == '2050s' ~ dens_on + 2,
                               period == '2080s' ~ dens_on + 4,
                               period == 'Retro' ~ dens_on + 0))

  dens_ice_off <- ice %>%
    dplyr::filter(!is.na(ice_on_water_day), !is.na(ice_off_water_day)) %>% # getting rid of years without ice
    group_by(period) %>%
    do(data.frame(loc_off = density(.$ice_off_water_day)$x,
                  dens_off = density(.$ice_off_water_day)$y)) %>%
    bind_rows(select(ice_ends, period, loc_off, dens_off)) %>%
    mutate(dens_off = (dens_off - min(dens_off)) / (max(dens_off)-min(dens_off)), # normalizing to make all same width
           loc_off = round(loc_off)) %>%
    ungroup()%>%
    group_by(period, loc_off) %>%
    summarise(dens_off = mean(dens_off)) %>%
    ungroup() %>%
    mutate(min_ice_off = case_when(period == 'Retro' ~ min_max_ice$min_ice_off[min_max_ice$period=='Retro'],
                                   period == '2050s' ~ min_max_ice$min_ice_off[min_max_ice$period=='2050s'],
                                   period == '2080s' ~ min_max_ice$min_ice_off[min_max_ice$period=='2080s']),
           max_ice_off = case_when(period == 'Retro' ~ min_max_ice$max_ice_off[min_max_ice$period=='Retro'],
                                   period == '2050s' ~ min_max_ice$max_ice_off[min_max_ice$period=='2050s'],
                                   period == '2080s' ~ min_max_ice$max_ice_off[min_max_ice$period=='2080s'])) %>%
    group_by(period) %>%
    dplyr::filter(loc_off >= min_ice_off,
                  loc_off <= max_ice_off) %>%
    mutate(dens_off = case_when(loc_off <= min_ice_off ~ 0,
                                loc_off >= max_ice_off ~ 0,
                                TRUE ~ dens_off)) %>%
    ungroup() %>%
    mutate(dens_off = case_when(period == '2050s' ~ dens_off + 2,
                                period == '2080s' ~ dens_off + 4,
                                period == 'Retro' ~ dens_off + 0))

  date_breaks = c(32, 93, 152, 213, 274, 360)
  date_breaks = c(32, 93, 152, 213)
  shortest_loc = c(-.6, 1.4, 3.4)
  longest_loc = c(-.2, 1.8, 3.8)
  ave_loc = c(-.4, 1.6, 3.6)
  # transparent = 60 # transparency for fill

  water_day = ice_data %>%
    dplyr::filter(year == 2001, gcm == 'CESM1_CAM5', period =='2050s') %>%
    select(datetime, month_day, water_day) %>%
    mutate(month = as.Date(datetime))

  snow_plot <- snow %>%
    mutate(datetime = as.Date(strptime(paste('2001-', doy, sep=''), format = '%Y-%j', tz = 'GMT'))) %>%
    left_join(water_day, by = 'datetime') %>%
    group_by(period, water_day) %>%
    summarise(mean = mean(snow_depth),
              max = max(snow_depth),
              min = min(snow_depth)) %>%
    ungroup() %>%
    mutate(mean = case_when(period == '2050s' ~ mean + 2,
                            period == '2080s' ~ mean + 4,
                            TRUE ~ mean),
           max = case_when(period == '2050s' ~ max + 2,
                           period == '2080s' ~ max + 4,
                           TRUE ~ max),
           min = case_when(period == '2050s' ~ min + 2,
                           period == '2080s' ~ min + 4,
                           TRUE ~ min)) %>%
    arrange(water_day)

  max_snow = snow_plot %>%
    group_by(period) %>%
    summarise(max_snow = max(max),
              max_min_snow = max(min)) %>%
    mutate(max_snow_date = snow_plot$water_day[snow_plot$max == max_snow],
           max_min_snow_date = snow_plot$water_day[snow_plot$min == max_min_snow]) %>%
    ungroup() %>%
    mutate(max_label = case_when(period == '2050s' ~ max_snow - 2,
                                  period == '2080s' ~ max_snow - 4,
                                  TRUE ~ max_snow),
           max_min_label = case_when(period == '2050s' ~ max_min_snow - 2,
                                 period == '2080s' ~ max_min_snow - 4,
                                 TRUE ~ max_min_snow))

  snow_plot = snow_plot %>%
    mutate(temp_max = case_when(period == '2050s' ~ max - 2,
                                 period == '2080s' ~ max - 4,
                                 TRUE ~ max)) %>%
    dplyr::filter(temp_max > 0) %>%
    select(-temp_max)

  ice_panel = ggplot(dens_ice_on, aes(dens_on, loc_on, group = period)) +
    geom_polygon(data = snow_plot, aes(x = mean, y = water_day, group = period), fill = 'grey', alpha = .3)+
    geom_polygon(data = snow_plot, aes(x = max, y = water_day, group = period), fill = 'grey', alpha = .3)+
    geom_polygon(data = snow_plot, aes(x = min, y = water_day, group = period), fill = 'grey', alpha = .3)+
    geom_polygon(aes(color = period, fill = period), show.legend = F) +
    geom_polygon(data = dens_ice_off, aes(dens_off, loc_off, group = period, color=period, fill =period), show.legend = F) +
    geom_segment(data = ice_summaries, aes(y = shortest_ice_on, yend = shortest_ice_off,
                                           x = shortest_loc, xend = shortest_loc),
                 size = 1.2,
                 arrow = arrow(90, ends = 'both', length = unit(.1, units = 'in'))) +
    geom_segment(data = ice_summaries, aes(y = longest_ice_on, yend = longest_ice_off,
                                           x = longest_loc, xend = longest_loc),
                 size = 1.2,
                 arrow = arrow(90, ends = 'both', length = unit(.1, units = 'in'))) +
    geom_segment(data = ice_summaries, aes(y = mean_ice_on, yend = mean_ice_off,
                                           x = ave_loc, xend = ave_loc),
                 size = 1.2,
                 arrow = arrow(90, ends = 'both', length = unit(.1, units = 'in'))) +
    annotate(geom = 'text',
             x = c(-.2,-.4,-.6), y = c(242,220,200), hjust = 0,
             label = c('Longest Ice Duration', 'Average Ice Duration', 'Shortest Ice Duration')) +
    annotate(geom = 'text',
             size = 5,
             x = c(2.5, 4.5), y = c(220,220), hjust = 0,
             label = c(paste(ice_summaries$no_ice_years[ice_summaries$period=='2050s'],'Ice-Free Year'),
                       paste(ice_summaries$no_ice_years[ice_summaries$period=='2080s'],'Ice-Free Years'))) +
    annotate(geom = 'text',
             x = c(0, 2, 4), y = c(20,20,20), hjust = .5, angle = 90, size = 6,
             label = c('Historic','2050s','2080s')) +
    geom_point(data = ice, aes(x = mid_plot, y = ice_on_water_day, group = period, shape = 'ice_on'),size = 2) +
    geom_point(data = ice, aes(x = mid_plot, y = ice_off_water_day, group = period, shape = 'ice_off'), size = 3) +
    scale_shape_manual(breaks = c('ice_on', 'ice_off'),
                       values = c(4, 16),
                       labels = c('Ice On' ,'Ice Off')) +
    expand_limits(y = 280) +
    coord_flip() +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 16),
          axis.line.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.9,.3),
          legend.text = element_text(size = 16),
          plot.tag = element_text(size = 16),
          plot.tag.position = c(.12,.93)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro)) +
    scale_y_continuous(breaks = date_breaks,
                       labels = strftime(as.Date(water_day_to_date$date[water_day_to_date$water_day %in% date_breaks], format = '%b %d'), format = '%b'))+
    labs(tag = 'I') +
    ggrepel::geom_text_repel(data = max_snow,
                             aes(x = max_snow, y = max_snow_date, label = paste('Max Snow:',round(max_label, digits = 2), 'm')),
                             segment.size  = 1,
                             nudge_y = c(20,-20,-20),
                             nudge_x = c(.2,.3,.2),
                             segment.color = "grey50",
                             direction     = "both") +
    ggrepel::geom_text_repel(data = dplyr::filter(max_snow, period !='Retro'),
                             aes(x = max_min_snow, y = max_min_snow_date, label = paste('Min Snow:',round(max_min_label, digits = 2), 'm')),
                             segment.size  = 1,
                             nudge_y = c(-20,-30),
                             nudge_x = c(.25,.4),
                             segment.color = "grey50",
                             direction     = "both")

  # ice_panel

  g = ggdraw() +
    draw_plot(month_temp, x = 0, y = .75, width = .3, height = .25) +
    draw_plot(month_precip, x= .5, y= .75, width = .3, height = .25) +
    draw_plot(month_evap, x = 0, y = .45, width = .3, height = .25) +
    draw_plot(month_r_b, x= .5, y= .45, width = .3, height = .25)+
    draw_plot(temp_diff, x = .3, y = .8, width = .15, height = .15) +
    draw_plot(precip_diff, x = .8 , y = .8, width = .15, height = .15) +
    draw_plot(evap_diff, x = .3, y = .5, width = .15, height = .15) +
    draw_plot(r_b_diff, x = .8, y = .5, width = .15, height = .15) +
    draw_plot(ice_panel, x = 0, y = 0, width = .6, height = .45) +
    draw_plot(basemap, x = .6, y = 0, width = .4, height = .45)

  g

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 16, height = 18)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
