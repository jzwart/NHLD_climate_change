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
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins)

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
  bins <- seq(0,1,length.out = 6) # breaks for cumulative fraction for emissions / burial

  top_emit <- all %>%
    group_by(period, gcm) %>%
    arrange(Emit, .by_group = T) %>%
    mutate(cumsum_emit = cumsum(Emit),
           cumsum_frac_emit = cumsum_emit / sum(Emit)) %>%
    arrange(desc(Emit), .by_group = T) %>%
    mutate(top_emit = case_when(cumsum_frac_emit >= top_frac ~ 'top',
                                TRUE ~ 'bottom'),
           top_emit = factor(top_emit, levels = c('top','bottom')),
           cumsum_frac_emit_bins = cut(cumsum_frac_emit, breaks = bins)) %>%
    ungroup() %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  top_emit_retro <- top_emit %>%
    dplyr::filter(gcm == 'Retro')

  top_emit_future <- top_emit %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(top_emit_retro, by = 'Permanent_', suffix = c('_future','_retro')) %>%
    mutate(emit_diff = Emit_future - Emit_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(Emit_retro, .by_group = T) %>%
    mutate(cumsum_emit_diff = cumsum(emit_diff)) %>%
    ungroup()

  top_bury <- all %>%
    group_by(period, gcm) %>%
    arrange(Bury, .by_group = T) %>%
    mutate(cumsum_bury = cumsum(Bury) / sum(Bury)) %>%
    arrange(desc(Bury), .by_group = T) %>%
    mutate(top_bury = case_when(cumsum_bury >= top_frac ~ 'top',
                                TRUE ~ 'bottom'),
           top_bury = factor(top_bury, levels = c('top','bottom')),
           bins = cut(cumsum_bury, breaks = bins)) %>%
    ungroup()

  top_bury_retro <- top_bury %>%
    dplyr::filter(gcm == 'Retro')

  top_bury_future <- top_bury %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(top_bury_retro, by = 'Permanent_', suffix = c('_future','_retro'))


  # cumulative fraction plotted against total emissions diff; make this colored by runoff + baseflow
  emit = ggplot(top_emit_future, aes(x = cumsum_frac_emit_retro, y = cumsum_emit_diff/10^9, linetype = period_future,
              group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 2) +
    theme_classic() +
    ylab(expression(Cumulative~Emissions~Difference~(Gg~C~year^-1))) +
    xlab(expression(Fraction~of~Total~Emissions))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_continuous(guide = guide_colorbar(title = expression(Runoff+Baseflow~(mm~yr^-1))),
      low = 'lightblue',high = 'darkblue') #+
    # scale_linetype_manual(name = 'period_future',
    #                       values = c('2050s' = 'dotted',
    #                                  '2080s' = 'solid'),
    #                       labels = c('2050\'s', '2080\'s'))

  emit

  g = ggdraw() +
    draw_plot(r_b_emit, x = 0, y = .5, width = .5, height = .5) +
    draw_plot(r_b_bury, x= .5, y= .5, width = .5, height = .5) +
    draw_plot(r_b_emit_to_bury, x = 0, y = 0, width = .5, height = .5) +
    draw_plot(r_b_emit_minus_bury, x= .5, y= 0, width = .5, height = .5)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 11, height = 10)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
