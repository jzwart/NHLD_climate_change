fig_total_c_flux <- function(fig_ind, transparent, scenarios, fig_cfg_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  drivers <- readRDS(drivers_file) # meteo drivers for period / gcm

  all <- readRDS(scenarios) %>%
    dplyr::filter(season == 'all') %>%
    mutate(Emit = Emit * 12 * 365, # emissions in g C / year
           Bury = Burial_total * 12 * 365, # bury in g C / year
           Emit_areal = Emit / Area, # emissions in g C/ m2/ year
           Bury_areal = Bury / Area, #  bury in g C / m2 / year
           precip = DirectP / Area, # precip in m
           runoff = (SWin) / Area) %>%
    select(Permanent_, Emit, Bury, Emit_areal, Bury_areal, Area, precip, runoff, period, gcm) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

  total <- all %>%
    group_by(period, gcm) %>%
    summarise(Emit = sum(Emit),
              Bury = sum(Bury),
              Area = sum(Area),
              Emit_areal = mean(Emit_areal), # mean for all lakes
              Bury_areal = mean(Bury_areal),
              precip = mean(precip),
              runoff = mean(runoff)) %>% # mean for all laes
    # mutate(Emit_areal = Emit / Area, # weighted by lake area
    #        Bury_areal = Bury / Area) %>% # weighted by lake area
    ungroup()

  runoff = ggplot(total, aes(x = runoff, y = Emit / 10^9, fill = period, color = period)) +  # converting to gigagrams
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
  runoff

  precip = ggplot(total, aes(x = precip, y = Emit / 10^9, fill = period, color = period)) +  # converting to gigagrams
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
  precip

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 7, height = 6)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
