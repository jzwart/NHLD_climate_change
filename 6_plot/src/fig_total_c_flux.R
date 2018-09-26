fig_total_c_flux <- function(fig_ind, transparent, scenarios, fig_cfg_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  all <- readRDS(scenarios) %>%
    dplyr::filter(season == 'all') %>%
    mutate(Emit = Emit * 12 * 365, # emissions in g C / year
           Bury = Burial_total * 12 * 365, # bury in g C / year
           Emit_areal = Emit / Area, # emissions in g C/ m2/ year
           Bury_areal = Bury / Area) %>% # bury in g C / m2 / year
    select(Permanent_, Emit, Bury, Emit_areal, Bury_areal, Area, period, gcm) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

  total <- all %>%
    group_by(period, gcm) %>%
    summarise(Emit = sum(Emit),
              Bury = sum(Bury),
              Area = sum(Area),
              Emit_areal = mean(Emit_areal), # mean for all lakes
              Bury_areal = mean(Bury_areal)) %>% # mean for all laes
    # mutate(Emit_areal = Emit / Area, # weighted by lake area
    #        Bury_areal = Bury / Area) %>% # weighted by lake area
    ungroup()

  ave = total %>%
    group_by(period) %>%
    summarise(Emit = mean(Emit),
              Bury = mean(Bury),
              Area = mean(Area),
              Emit_areal = mean(Emit_areal),
              Bury_areal = mean(Bury_areal)) %>%
    ungroup()

  emit = ggplot(total, aes(x = period, y = Emit / 10^9, group = period, fill = period, color = period)) +  # converting to gigagrams
    geom_violin(trim = F, show.legend = F)+
    geom_point(size = 4, shape = 16, show.legend = F) +
    geom_point(data = ave, aes(x = period, y = Emit / 10^9, group = period, color = period) , size = 7, shape = 21, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~Emissions)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
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

  bury = ggplot(total, aes(x = period, y = Bury / 10^9, group = period, fill = period, color = period)) +  # converting to gigagrams
    geom_violin(trim = F, show.legend = F)+
    geom_point(size = 4, shape = 16, show.legend = F) +
    geom_point(data = ave, aes(x = period, y = Bury / 10^9, group = period, color = period) , size = 7, shape = 21, show.legend = F) +
    theme_classic() +
    ylab(expression(Total~Burial)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
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

  emit_areal = ggplot(total, aes(x = period, y = Emit_areal, group = period, fill = period, color = period)) +  # converting to gigagrams
    geom_violin(trim = F, show.legend = F)+
    geom_point(size = 4, shape = 16, show.legend = F) +
    geom_point(data = ave, aes(x = period, y = Emit_areal, group = period, color = period) , size = 7, shape = 21, show.legend = F) +
    theme_classic() +
    ylab(expression(Areal~Emissions)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          # axis.text.x = element_blank(),
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

  bury_areal = ggplot(total, aes(x = period, y = Bury_areal, group = period, fill = period, color = period)) +  # converting to gigagrams
    geom_violin(trim = F, show.legend = F)+
    geom_point(size = 4, shape = 16, show.legend = F) +
    geom_point(data = ave, aes(x = period, y = Bury_areal, group = period, color = period) , size = 7, shape = 21, show.legend = F) +
    theme_classic() +
    ylab(expression(Areal~Burial)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          # axis.text.x = element_blank(),
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

  area = ggplot(total, aes(x = period, y = Area, group = period, fill = period, color = period)) +  # converting to gigagrams
    geom_violin(trim = F, show.legend = F)+
    geom_point(size = 4, shape = 16, show.legend = F) +
    geom_point(data = ave, aes(x = period, y = Area, group = period, color = period) , size = 7, shape = 21, show.legend = F) +
    theme_classic() +
    ylab(expression(Lake~Area)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
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

  g = ggdraw() +
    draw_plot(emit, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(bury, x= .5, y= .5, width = .5, height = .5) +
    draw_plot(emit_areal, x = 0, y = 0, width = .5, height = .5) +
    draw_plot(bury_areal, x= .5, y= 0, width = .5, height = .5)
  g

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 7, height = 6)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
