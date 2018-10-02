fig_total_c_flux <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  drivers <- readRDS(drivers_file) # meteo drivers for period / gcm

  all <- readRDS(scenarios) %>%
    dplyr::filter(season == 'all') %>%
    mutate(Emit = Emit * 12 * 365, # emissions in g C / year
           Bury = Burial_total * 12 * 365, # bury in g C / year
           Emit_areal = Emit / Area, # emissions in g C/ m2/ year
           Bury_areal = Bury / Area, #  bury in g C / m2 / year
           DOC_load = DOC_Load / Area,
           # R_B = (SWin + Baseflow) / Area_m2,
           Precip_lake = DirectP / Area) %>%
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Area, DOC_load, HRT, Stage, Vol,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp)

  total <- all %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
              Bury = sum(Bury),
              Area = sum(Area),
              Emit_areal = mean(Emit_areal), # mean for all lakes
              Bury_areal = mean(Bury_areal),
              DOC_load = mean(DOC_load),
              Precip_lake = mean(Precip_lake),
              Water_in = sum(waterIn),
              # R_B = median(R_B, na.rm = T),
              ndays_ice = mean(ndays_ice),
              epiTemp = median(epiTemp),
              HRT = median(HRT),
              Vol = sum(Vol)) %>%
    ungroup()

  ave_drivers <- drivers %>%
    group_by(period, gcm, var) %>%
    dplyr::summarise(var_value = sum(var_value)) %>%
    ungroup() %>%
    spread(key = 'var', value = 'var_value')

  c_and_drivers <- left_join(total, ave_drivers, by = c('period' = 'period', 'gcm' = 'gcm')) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

  r_b_emit = ggplot(c_and_drivers, aes(x = Runoff_and_baseflow, y = Emit / 10^9, color = period, size =period, shape = period)) +  # converting to gigagrams
    geom_point() +
    theme_classic() +
    ylab(expression(Total~Emissions~(Gg~C~year^-1))) +
    xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                      values = c('2050s' = 16,
                                 '2080s' = 16,
                                 'Retro' = 16),
                      labels = c('Historic','2050\'s', '2080\'s'))

  r_b_bury = ggplot(c_and_drivers, aes(x = Runoff_and_baseflow, y = Bury / 10^9, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~C~Burial~(Gg~C~year^-1))) +
    xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  r_b_emit_to_bury = ggplot(c_and_drivers, aes(x = Runoff_and_baseflow, y = Emit / Bury, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Emissions~to~Burial~(C:C))) +
    xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  r_b_emit_minus_bury = ggplot(c_and_drivers, aes(x = Runoff_and_baseflow, y = (Emit - Bury)/10^9, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Emissions~minus~Burial~(Gg~C~year^-1))) +
    xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('Historic','2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 8,
                                 '2080s' = 8,
                                 'Retro' = 8),
                      labels = c('Historic','2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period',
                       values = c('2050s' = 16,
                                  '2080s' = 16,
                                  'Retro' = 16),
                       labels = c('Historic','2050\'s', '2080\'s'))

  g = ggdraw() +
    draw_plot(r_b_emit, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(r_b_bury, x= .5, y= .5, width = .5, height = .5) +
    draw_plot(r_b_emit_to_bury, x = 0, y = 0, width = .5, height = .5) +
    draw_plot(r_b_emit_minus_bury, x= .5, y= 0, width = .5, height = .5)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 7, height = 6)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
