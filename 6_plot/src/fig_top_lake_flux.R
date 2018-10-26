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
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Area, DOC_load, HRT, Stage, Vol, doc_conc,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins, percentEvap)

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
              Vol = sum(Vol),
              DOC = median(doc_conc)) %>%
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
           cumsum_frac_emit = cumsum_emit / sum(Emit),
           frac_lakes = seq(1:length(cumsum_frac_emit))/length(cumsum_frac_emit)) %>%
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

  doc <- all %>%
    group_by(period, gcm) %>%
    arrange(percentEvap, .by_group = T) %>%
    ungroup() %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  doc_retro <- doc %>%
    dplyr::filter(gcm == 'Retro')

  doc_future <- doc %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(doc_retro, by = 'Permanent_', suffix = c('_future','_retro')) %>%
    mutate(doc_ratio = doc_conc_future / doc_conc_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(percentEvap_retro, .by_group = T) %>%
    ungroup()

  top_bury <- all %>%
    group_by(period, gcm) %>%
    arrange(Bury, .by_group = T) %>%
    mutate(cumsum_bury = cumsum(Bury),
           cumsum_frac_bury = cumsum_bury / sum(Bury)) %>%
    arrange(desc(Bury), .by_group = T) %>%
    mutate(top_bury = case_when(cumsum_frac_bury >= top_frac ~ 'top',
                                TRUE ~ 'bottom'),
           top_bury = factor(top_bury, levels = c('top','bottom')),
           cumsum_frac_bury_bins = cut(cumsum_frac_bury, breaks = bins)) %>%
    ungroup() %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  top_bury_retro <- top_bury %>%
    dplyr::filter(gcm == 'Retro')

  top_bury_future <- top_bury %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(top_bury_retro, by = 'Permanent_', suffix = c('_future','_retro')) %>%
    mutate(bury_diff = Bury_future - Bury_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(Bury_retro, .by_group = T) %>%
    mutate(cumsum_bury_diff = cumsum(bury_diff)) %>%
    ungroup()

  emit_minus_bury <- all %>%
    mutate(emit_minus_bury = Emit - Bury) %>%
    group_by(period, gcm) %>%
    arrange(emit_minus_bury, .by_group = T) %>%
    mutate(cumsum_emit_minus_bury = cumsum(emit_minus_bury),
           cumsum_frac_emit_minus_bury = cumsum_emit_minus_bury / sum(emit_minus_bury)) %>%
    arrange(desc(emit_minus_bury), .by_group = T) %>%
    mutate(top_emit_minus_bury = case_when(cumsum_frac_emit_minus_bury >= top_frac ~ 'top',
                                TRUE ~ 'bottom'),
           top_emit_minus_bury = factor(top_emit_minus_bury, levels = c('top','bottom')),
           cumsum_frac_emit_minus_bury_bins = cut(cumsum_frac_emit_minus_bury, breaks = bins)) %>%
    ungroup() %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  emit_minus_bury_retro <- emit_minus_bury %>%
    dplyr::filter(gcm == 'Retro')

  emit_minus_bury_future <- emit_minus_bury %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(emit_minus_bury_retro, by = 'Permanent_', suffix = c('_future','_retro')) %>%
    mutate(emit_minus_bury_diff = emit_minus_bury_future - emit_minus_bury_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(emit_minus_bury_retro, .by_group = T) %>%
    mutate(cumsum_emit_minus_bury_diff = cumsum(emit_minus_bury_diff)) %>%
    ungroup()

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
      low = 'lightblue',high = 'darkblue') +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))
#
#   ggplot(top_emit_future, aes(x = frac_lakes_retro, y = cumsum_emit_diff/10^9, linetype = period_future,
#                               group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
#     geom_line(size = 2) +
#     theme_classic() +
#     ylab(expression(Cumulative~Emissions~Difference~(Gg~C~year^-1))) +
#     xlab(expression(Cumulative~Fraction~of~Lakes))+
#     theme(axis.text = element_text(size=16),
#           axis.title = element_text(size = 16),
#           legend.title = element_text(size =14),
#           legend.position = c(.25,.8),
#           legend.background = element_blank(),
#           legend.text = element_text(size = 14))+
#     geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
#     scale_color_continuous(guide = guide_colorbar(title = expression(Runoff+Baseflow~(mm~yr^-1))),
#                            low = 'lightblue',high = 'darkblue') +
#     scale_linetype_manual(name = 'period_future',
#                           values = c('2050s' = 'twodash',
#                                      '2080s' = 'solid'),
#                           labels = c('2050\'s', '2080\'s'),
#                           guide = guide_legend(title = expression(Period)))

    ggplot(top_emit_future, aes(x = cumsum_frac_emit_retro, y = cumsum_emit_diff/10^9, linetype = period_future,
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
                             low = 'lightblue',high = 'darkblue') +
      scale_linetype_manual(name = 'period_future',
                            values = c('2050s' = 'twodash',
                                       '2080s' = 'solid'),
                            labels = c('2050\'s', '2080\'s'),
                            guide = guide_legend(title = expression(Period))) +
      geom_vline(xintercept = top_emit_future$cumsum_frac_emit_retro[which(round(top_emit_future$frac_lakes_retro,digits = 4) %in% c(.9))],
                 linetype = 'dashed')


  bury = ggplot(top_bury_future, aes(x = cumsum_frac_bury_retro, y = cumsum_bury_diff/10^9, linetype = period_future,
                                     group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 2) +
    theme_classic() +
    ylab(expression(Cumulative~Burial~Difference~(Gg~C~year^-1))) +
    xlab(expression(Fraction~of~Total~Burial))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_continuous(guide = guide_colorbar(title = expression(Runoff+Baseflow~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))

  doc_fhee = ggplot(doc_future, aes(x = percentEvap_retro, y = doc_ratio, color = Runoff_and_baseflow_future)) +
    geom_point(size = 2, pch=16, alpha = .2) +
    theme_classic() +
    ylab(expression(DOC~Ratio~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Runoff+Baseflow~(mm~yr^-1))),
                           low = 'orange',high = 'blue') +
    geom_smooth(aes(x = percentEvap_retro, y = doc_ratio, color = Runoff_and_baseflow_future, group = Runoff_and_baseflow_future),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid') +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1)

  doc_fhee

  emit_minus_bury = ggplot(emit_minus_bury_future, aes(x = cumsum_frac_emit_minus_bury_retro,
                                                       y = cumsum_emit_minus_bury_diff/10^9, linetype = period_future,
                                     group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 2) +
    theme_classic() +
    ylab(expression(Cumulative~Emit-Burial~Difference~(Gg~C~year^-1))) +
    xlab(expression(Fraction~of~Total~Emit-Burial))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_continuous(guide = guide_colorbar(title = expression(Runoff+Baseflow~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))

  g = plot_grid(emit, bury, labels = c('A', 'B'), align = 'h')


  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 14, height = 7)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
