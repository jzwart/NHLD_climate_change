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
              R_B = median(R_B, na.rm = T),
              ndays_ice = mean(ndays_ice),
              epiTemp = median(epiTemp),
              HRT = median(HRT),
              Vol = sum(Vol)) %>%
    ungroup()

  ave_drivers <- drivers %>%
    group_by(period, gcm, var) %>%
    dplyr::summarise(var_value = mean(var_value)) %>%
    ungroup() %>%
    spread(key = 'var', value = 'var_value')

  c_and_drivers <- left_join(total, ave_drivers, by = c('period' = 'period', 'gcm' = 'gcm')) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

  r_b_emit = ggplot(c_and_drivers, aes(x = Runoff_and_baseflow, y = Emit / 10^9, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~Emissions)) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro))

  precip_emit = ggplot(c_and_drivers, aes(x = Precip, y = Emit / 10^9, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~Emissions)) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro))

  r_b_bury = ggplot(c_and_drivers, aes(x = Runoff_and_baseflow, y = Bury / 10^9, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~Emissions)) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro))

  precip_bury = ggplot(c_and_drivers, aes(x = Precip, y = Bury / 10^9, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~Emissions)) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro))


  r_b_emit_to_bury = ggplot(c_and_drivers, aes(x = Runoff_and_baseflow, y = Emit/Bury, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~Emissions)) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro))



  ggplot(c_and_drivers, aes(x = Temp, y = Emit / 10^9, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro))

  ggplot(c_and_drivers, aes(x = R_B, y = Emit, fill = period, color = period)) +  # converting to gigagrams
    geom_point(size = 8, shape = 16, show.legend = F) +
    theme_classic() +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16)) +
    scale_fill_manual(name = 'period',
                      breaks=c('2080s','2050s','Retro'),
                      values=c('2080s' = t_col(fig_config$period$`2080s`, transparent),
                               '2050s' = t_col(fig_config$period$`2050s`, transparent),
                               'Retro' = t_col(fig_config$period$Retro, transparent))) +
    scale_color_manual(name = 'period',
                       breaks=c('2080s','2050s','Retro'),
                       values=c('2080s' = fig_config$period$`2080s`,
                                '2050s' = fig_config$period$`2050s`,
                                'Retro' = fig_config$period$Retro))



  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 7, height = 6)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
