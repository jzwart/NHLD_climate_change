fig_ice_dur <- function(fig_ind, fig_cfg_yml, ice_dur_ind_file, remake_file, gd_config){

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

  # years = unique(paste(ice$gcm,ice$period,ice$wy))
  # alpha_base = 0.1
  # alpha_border = 0.2
  # jitter = 0.1
  # jitter_future = jitter / 6
  # ymax = jitter * sum(grepl('Retro', years)) * 2 + jitter_future * (length(years) - sum(grepl('Retro', years))) * 2
  #
  # ice_on = ice_data %>% dplyr::filter(ice_on == 1)
  # ggplot(ice_data, aes(x = water_day, y= ice_on, group = period)) +
  #   geom_violin()

  # fig_file = as_data_file(fig_ind)
  # png(fig_file, width = 10, height = 6, units = 'in', res = 300)

  # plot(ice_data$water_day, seq(0,ymax,length.out = nrow(ice_data)), cex = 0, xlim = c(40,260), xlab = '', xaxt = 'n', yaxt= 'n', ylab = '', bty = 'n')
  # axis(1, at = c(1, 62, 124, 183, 244, 300, 360), labels = water_day_to_date$date[water_day_to_date$water_day %in% c(1, 62, 124, 183, 244, 300, 360)])
  # count_retro = 0
  # mid_retro = sum(grepl('Retro', years)) * jitter
  # count_2050s = 0
  # mid_2050s = sum(grepl('2050s', years)) * jitter_future + sum(grepl('Retro', years)) * 2 * jitter
  # count_2080s = 0
  # mid_2080s = sum(grepl('2080s', years)) * jitter_future + sum(grepl('Retro', years)) * 2 * jitter + sum(grepl('2050s', years)) * 2 * jitter_future
  # for(year in years){
  #   cur = ice[paste(ice$gcm,ice$period,ice$wy) == year,]
  #   if(is.na(cur$water_day[1])){
  #     next
  #   }
  #   x = sort(cur$water_day)
  #   if(cur$period[1] == 'Retro'){
  #     count_retro = count_retro + 1
  #     ymin = mid_retro - count_retro * jitter
  #     ymax = mid_retro + count_retro * jitter
  #     y = c(ymin, ymax, ymax, ymin)
  #     alpha = alpha_base
  #     color = grDevices::adjustcolor(fig_config$period$Retro, alpha.f = alpha)
  #     b_color = grDevices::adjustcolor(fig_config$period$Retro, alpha.f = alpha_border)
  #   }else if(cur$period[1] == '2050s'){
  #     count_2050s = count_2050s + 1
  #     ymin = mid_2050s - count_2050s * jitter_future
  #     ymax = mid_2050s + count_2050s * jitter_future
  #     y = c(ymin, ymax, ymax, ymin)
  #     alpha = alpha_base / 6
  #     color = grDevices::adjustcolor(fig_config$period$`2050s`, alpha.f = alpha)
  #     b_color = grDevices::adjustcolor(fig_config$period$`2050s`, alpha.f = alpha_border)
  #   }else if(cur$period[1] == '2080s'){
  #     count_2080s = count_2080s + 1
  #     ymin = mid_2080s - count_2080s * jitter_future
  #     ymax = mid_2080s + count_2080s * jitter_future
  #     y = c(ymin, ymax, ymax, ymin)
  #     alpha = alpha_base / 6
  #     color = grDevices::adjustcolor(fig_config$period$`2080s`, alpha.f = alpha)
  #     b_color = grDevices::adjustcolor(fig_config$period$`2080s`, alpha.f = alpha_border)
  #   }
  #
  #   polygon(x = x, y = y, col = color, border = b_color)
  # }
  # text(x = c(240,240,240), y = c(mid_retro, mid_2050s, mid_2080s), labels = c('Historic', '2050s', '2080s'), cex = 1.5, pos = 4)


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
              max_ice_off = max(ice_off_water_day, na.rm = T)) %>% ungroup()

  # calculating density of ice on days
  dens <- ice_days %>%
    group_by(period) %>%
    do(data.frame(loc = density(.$water_day)$x,
                  dens = density(.$water_day)$y)) %>%
    mutate(dens = (dens - min(dens)) / (max(dens)-min(dens)), # normalizing to make all same width
           loc = round(loc)) %>%
    dplyr::filter(!duplicated(loc)) %>%
    ungroup() %>%
    mutate(min_ice_on = case_when(period == 'Retro' ~ min_max_ice$min_ice_on[min_max_ice$period=='Retro'],
                                  period == '2050s' ~ min_max_ice$min_ice_on[min_max_ice$period=='2050s'],
                                  period == '2080s' ~ min_max_ice$min_ice_on[min_max_ice$period=='2080s']),
           max_ice_off = case_when(period == 'Retro' ~ min_max_ice$max_ice_off[min_max_ice$period=='Retro'],
                                  period == '2050s' ~ min_max_ice$max_ice_off[min_max_ice$period=='2050s'],
                                  period == '2080s' ~ min_max_ice$max_ice_off[min_max_ice$period=='2080s'])) %>%
    group_by(period) %>%
    dplyr::filter(loc >= min_ice_on, loc <= max_ice_off) %>%
    mutate(dens = case_when(loc == min_ice_on ~ 0,
                            loc == max_ice_off ~ 0,
                            TRUE ~ dens)) %>%
    ungroup() %>%
    mutate(dens = case_when(period == '2050s' ~ dens + 2,
                            period == '2080s' ~ dens + 4,
                            period == 'Retro' ~ dens + 0))


  date_breaks = c(32, 93, 152, 213, 274, 360)
  shortest_loc = c(-.6, 1.4, 3.4)
  longest_loc = c(-.2, 1.8, 3.8)
  ave_loc = c(-.4, 1.6, 3.6)

  g = ggplot(dens, aes(dens, loc, group = period)) +
    geom_polygon(color = 'grey', show.legend = F, fill = 'grey') +
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
             x = c(2.5, 4.5), y = c(220,220), hjust = 0,
             label = c(paste(ice_summaries$no_ice_years[ice_summaries$period=='2050s'],'Ice-Free Year'),
                       paste(ice_summaries$no_ice_years[ice_summaries$period=='2080s'],'Ice-Free Years'))) +
    annotate(geom = 'text',
             x = c(0, 2, 4), y = c(20,20,20), hjust = .5, angle = 90, size = 8,
             label = c('Historic','2050s','2080s')) +
    geom_point(data = ice, aes(x = mid_plot, y = ice_on_water_day, group = period, shape = 'ice_on')) +
    geom_point(data = ice, aes(x = mid_plot, y = ice_off_water_day, group = period, shape = 'ice_off'), size = 2) +
    scale_shape_manual(breaks = c('ice_on', 'ice_off'),
                       values = c(4, 16),
                       labels = c('Ice On' ,'Ice Off')) +
    expand_limits(y = 300) +
    coord_flip() +
    theme_classic() +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 16),
          axis.line.y = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.9,.3)) +
    # scale_fill_manual(breaks=c('2080s','2050s','Retro'),
    #                   values=c('2080s' = fig_config$period$`2080s`,
    #                            '2050s' = fig_config$period$`2050s`,
    #                            'Retro' = fig_config$period$Retro)) +
    scale_y_continuous(breaks = date_breaks,
                       labels = water_day_to_date$date[water_day_to_date$water_day %in% date_breaks])
  g

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 9, height = 6)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
