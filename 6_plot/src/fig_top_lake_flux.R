fig_top_lake_flux <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){
 # figure for the change in emissions / burial of top x% of emitting lakes

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

  top_frac <- .05 # looking for group of lakes that account for 90% of regional emissions

  top_emit <- all %>%
    group_by(period, gcm) %>%
    arrange(Emit, .by_group = T) %>%
    mutate(cumsum_emit = cumsum(Emit) / sum(Emit)) %>%
    arrange(desc(Emit), .by_group = T) %>%
    mutate(top_emit = case_when(cumsum_emit >= top_frac ~ 'top',
                                TRUE ~ 'bottom'),
           top_emit = factor(top_emit, levels = c('top','bottom'))) %>%
    ungroup()

  top_emit_retro <- top_emit %>%
    dplyr::filter(gcm == 'Retro')

  top_emit_future <- top_emit %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(top_emit_retro, by = 'Permanent_', suffix = c('_future','_retro'))

  top_bury <- all %>%
    group_by(period, gcm) %>%
    arrange(Bury, .by_group = T) %>%
    mutate(cumsum_bury = cumsum(Bury) / sum(Bury)) %>%
    arrange(desc(Bury), .by_group = T) %>%
    mutate(top_bury = case_when(cumsum_bury >= top_frac ~ 'top',
                                TRUE ~ 'bottom'),
           top_bury = factor(top_bury, levels = c('top','bottom'))) %>%
    ungroup()

  top_bury_retro <- top_bury %>%
    dplyr::filter(gcm == 'Retro')

  top_bury_future <- top_bury %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(top_bury_retro, by = 'Permanent_', suffix = c('_future','_retro'))


  ggplot(top_emit_future, aes(x = cumsum_emit_retro, y = Emit_future/Emit_retro,
                              color = top_emit_retro, size =top_emit_retro)) +
    geom_point() +
    theme_classic() +
    ylab(expression(Emissions~Ratio)) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'top_emit_retro',
                       values = c('top' = t_col('red',80),
                                  'bottom' = t_col('blue',80)),
                       labels = c('top','bottom')) +
    scale_fill_manual(name = 'top_emit_retro',
                      values = c('top' = t_col('red',80),
                                 'bottom' = t_col('blue',80)),
                      labels = c('top','bottom')) +
    scale_size_manual(name = 'top_emit_retro',
                      values = c('top' = 1,
                                 'bottom' = 1),
                      labels = c('top','bottom')) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_smooth(method = 'lm', se = F, color = 'black') # make individual lines for gcm's and color based on runoff

  ggplot(dplyr::filter(top_emit_future, period_future =='2080s'), aes(x = cumsum_emit_retro, y = Emit_future/Emit_retro,
                                                                      color = waterIn_future/waterIn_retro)) +
    geom_point() +
    theme_classic() +
    ylab(expression(Emissions~Ratio)) +
    xlab(expression(Cumulative~Fraction))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.95,.8),
          legend.text = element_text(size = 12)) +
    scale_color_continuous(low = 'lightblue',high = 'blue') +
    scale_x_log10() +
    # scale_color_manual(name = 'top_emit_retro',
    #                    values = c('top' = t_col('red',80),
    #                               'bottom' = t_col('blue',80)),
    #                    labels = c('top','bottom')) +
    # scale_fill_manual(name = 'top_emit_retro',
    #                   values = c('top' = t_col('red',80),
    #                              'bottom' = t_col('blue',80)),
    #                   labels = c('top','bottom')) +
    # scale_size_manual(name = 'top_emit_retro',
    #                   values = c('top' = 1,
    #                              'bottom' = 1),
    #                   labels = c('top','bottom')) +
    geom_hline(yintercept = 1, linetype = 'dashed')# +
    # geom_smooth(method = 'loess', se = F) # make individual lines for gcm's and color based on runoff

  ggplot(top_emit_future,
         aes(x = waterIn_future/waterIn_retro, y = Emit_future/Emit_retro, color = period_future,
             shape = period_future, fill = period_future, size = cumsum_emit_retro)) +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Emissions~Ratio)) +
    xlab(expression(Water~Load~Ratio))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.95,.8),
          legend.text = element_text(size = 12)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_vline(xintercept = 1, linetype = 'dashed') +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 90),
                                  '2080s' = t_col(fig_config$period$`2080s`, 90)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 90),
                                  '2080s' = t_col(fig_config$period$`2080s`, 90)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 21,
                                  '2080s' = 21),
                       labels = c('2050\'s', '2080\'s')) +
    geom_smooth(method = 'lm')

  ggplot(top_emit_future,
         aes(x = waterIn_future/waterIn_retro, y = Emit_future/Emit_retro, color = period_future,
             shape = period_future, fill = period_future, size = 1/HRT_retro)) +
    geom_point(show.legend = F) +
    theme_classic() +
    ylab(expression(Emissions~Ratio)) +
    xlab(expression(Water~Load~Ratio))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_blank(),
          legend.position = c(.95,.8),
          legend.text = element_text(size = 12)) +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_vline(xintercept = 1, linetype = 'dashed') +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 90),
                                  '2080s' = t_col(fig_config$period$`2080s`, 90)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, 90),
                                 '2080s' = t_col(fig_config$period$`2080s`, 90)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 21,
                                  '2080s' = 21),
                       labels = c('2050\'s', '2080\'s')) +
    geom_smooth(method = 'lm')



  ggplot(top_emit_future, aes(x = top_emit_retro, y = Emit_future/Emit_retro,
                              color = top_emit_retro, size =top_emit_retro, fill = top_emit_retro)) +  # converting to gigagrams
    geom_violin() +
    theme_classic() +
    ylab(expression(Emissions~Ratio)) +
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'top_emit_retro',
                       values = c('top' = t_col('red',0),
                                  'bottom' = t_col('blue',0)),
                       labels = c('top','bottom')) +
    scale_fill_manual(name = 'top_emit_retro',
                       values = c('top' = t_col('red',80),
                                  'bottom' = t_col('blue',80)),
                       labels = c('top','bottom')) +
    scale_size_manual(name = 'top_emit_retro',
                      values = c('top' = 1,
                                 'bottom' = 1),
                      labels = c('top','bottom')) +
    geom_hline(yintercept = 1, linetype = 'dashed')

  ggplot(top_bury_future, aes(x = top_bury_retro, y = Bury_future/Bury_retro,
                              color = top_bury_retro, size =top_bury_retro, fill = top_bury_retro)) +  # converting to gigagrams
    geom_violin() +
    theme_classic() +
    ylab(expression(Emissions~Ratio)) +
    lims(y = c(0,2.5))+
    # xlab(expression(Runoff+Baseflow~(mm~year^-1)))+
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.8),
          legend.text = element_text(size = 12)) +
    scale_color_manual(name = 'top_bury_retro',
                       values = c('top' = t_col('red',0),
                                  'bottom' = t_col('blue',0)),
                       labels = c('top','bottom')) +
    scale_fill_manual(name = 'top_bury_retro',
                      values = c('top' = t_col('red',80),
                                 'bottom' = t_col('blue',80)),
                      labels = c('top','bottom')) +
    scale_size_manual(name = 'top_bury_retro',
                      values = c('top' = 1,
                                 'bottom' = 1),
                      labels = c('top','bottom')) +
    geom_hline(yintercept = 1, linetype = 'dashed')

  g = ggdraw() +
    draw_plot(r_b_emit, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(r_b_bury, x= .5, y= .5, width = .5, height = .5) +
    draw_plot(r_b_emit_to_bury, x = 0, y = 0, width = .5, height = .5) +
    draw_plot(r_b_emit_minus_bury, x= .5, y= 0, width = .5, height = .5)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 11, height = 10)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
