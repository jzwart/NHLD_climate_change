fig_ice_dur <- function(fig_ind, fig_cfg_yml, ice_dur_ind_file, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  ice_data <- readRDS(sc_retrieve(ice_dur_ind_file, remake_file = remake_file)) %>% as_tibble() %>%
    mutate(day = lubridate::day(datetime),
           month = lubridate::month(datetime),
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
           top_plot = case_when(period == 'Retro' ~ 1,
                           period == '2050s' ~ 2,
                           period == '2080s' ~ 3),
           bot_plot = case_when(period == 'Retro' ~ 0,
                           period == '2050s' ~ 1,
                           period == '2080s' ~ 2)) %>%
    group_by(period) %>%
    arrange(ice_on_water_day) %>%
    ungroup() %>%
    tidyr::gather(key = 'event', value = 'water_day', contains('water_day')) %>%
    tidyr::gather(key = 'y_placement', value = 'y', contains('plot'))

  water_day_to_date = data.frame(water_day = ice_data$water_day[1:365], date = strftime(ice_data$datetime[1:365], format = '%b %d'))

  years = unique(paste(ice$gcm,ice$period,ice$wy))
  alpha_base = 0.1
  alpha_border = 0.2
  jitter = 0.1
  jitter_future = jitter / 6
  ymax = jitter * sum(grepl('Retro', years)) * 2 + jitter_future * (length(years) - sum(grepl('Retro', years))) * 2

  fig_file = as_data_file(fig_ind)
  png(fig_file, width = 10, height = 6, units = 'in', res = 300)

  plot(ice_data$water_day, seq(0,ymax,length.out = nrow(ice_data)), cex = 0, xlim = c(40,260), xlab = '', xaxt = 'n', yaxt= 'n', ylab = '', bty = 'n')
  axis(1, at = c(1, 62, 124, 183, 244, 300, 360), labels = water_day_to_date$date[water_day_to_date$water_day %in% c(1, 62, 124, 183, 244, 300, 360)])
  count_retro = 0
  mid_retro = sum(grepl('Retro', years)) * jitter
  count_2050s = 0
  mid_2050s = sum(grepl('2050s', years)) * jitter_future + sum(grepl('Retro', years)) * 2 * jitter
  count_2080s = 0
  mid_2080s = sum(grepl('2080s', years)) * jitter_future + sum(grepl('Retro', years)) * 2 * jitter + sum(grepl('2050s', years)) * 2 * jitter_future
  for(year in years){
    cur = ice[paste(ice$gcm,ice$period,ice$wy) == year,]
    if(is.na(cur$water_day[1])){
      next
    }
    x = sort(cur$water_day)
    if(cur$period[1] == 'Retro'){
      count_retro = count_retro + 1
      ymin = mid_retro - count_retro * jitter
      ymax = mid_retro + count_retro * jitter
      y = c(ymin, ymax, ymax, ymin)
      alpha = alpha_base
      color = grDevices::adjustcolor(fig_config$period$Retro, alpha.f = alpha)
      b_color = grDevices::adjustcolor(fig_config$period$Retro, alpha.f = alpha_border)
    }else if(cur$period[1] == '2050s'){
      count_2050s = count_2050s + 1
      ymin = mid_2050s - count_2050s * jitter_future
      ymax = mid_2050s + count_2050s * jitter_future
      y = c(ymin, ymax, ymax, ymin)
      alpha = alpha_base / 6
      color = grDevices::adjustcolor(fig_config$period$`2050s`, alpha.f = alpha)
      b_color = grDevices::adjustcolor(fig_config$period$`2050s`, alpha.f = alpha_border)
    }else if(cur$period[1] == '2080s'){
      count_2080s = count_2080s + 1
      ymin = mid_2080s - count_2080s * jitter_future
      ymax = mid_2080s + count_2080s * jitter_future
      y = c(ymin, ymax, ymax, ymin)
      alpha = alpha_base / 6
      color = grDevices::adjustcolor(fig_config$period$`2080s`, alpha.f = alpha)
      b_color = grDevices::adjustcolor(fig_config$period$`2080s`, alpha.f = alpha_border)
    }

    polygon(x = x, y = y, col = color, border = b_color)
  }
  text(x = c(240,240,240), y = c(mid_retro, mid_2050s, mid_2080s), labels = c('Historic', '2050s', '2080s'), cex = 1.5, pos = 4)

  dev.off()

  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
