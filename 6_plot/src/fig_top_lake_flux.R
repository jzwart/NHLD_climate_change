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
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins, percentEvap, GPP, Vepi) %>%
    mutate(GPP = GPP * Vepi) # converting from volumetric to mol C


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
    mutate(cumsum_emit_diff = abs(cumsum(emit_diff))/ max(cumsum_emit_retro) *100*ifelse(cumsum(emit_diff)>0,1,-1)) %>%
    ungroup()

  top_bury <- all %>%
    group_by(period, gcm) %>%
    arrange(Bury, .by_group = T) %>%
    mutate(cumsum_bury = cumsum(Bury),
           cumsum_frac_bury = cumsum_bury / sum(Bury),
           frac_lakes = seq(1:length(cumsum_frac_bury))/length(cumsum_frac_bury)) %>%
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
    mutate(cumsum_bury_diff = abs(cumsum(bury_diff))/ max(cumsum_bury_retro) *100*ifelse(cumsum(bury_diff)>0,1,-1)) %>%
    ungroup()

  top_gpp <- all %>%
    group_by(period, gcm) %>%
    arrange(GPP, .by_group = T) %>%
    mutate(cumsum_gpp = cumsum(GPP),
           cumsum_frac_gpp = cumsum_gpp / sum(GPP),
           frac_lakes = seq(1:length(cumsum_frac_gpp))/length(cumsum_frac_gpp)) %>%
    arrange(desc(GPP), .by_group = T) %>%
    mutate(top_gpp = case_when(cumsum_frac_gpp >= top_frac ~ 'top',
                                TRUE ~ 'bottom'),
           top_gpp = factor(top_gpp, levels = c('top','bottom')),
           cumsum_frac_gpp_bins = cut(cumsum_frac_gpp, breaks = bins)) %>%
    ungroup() %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  top_gpp_retro <- top_gpp %>%
    dplyr::filter(gcm == 'Retro')

  top_gpp_future <- top_gpp %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(top_gpp_retro, by = 'Permanent_', suffix = c('_future','_retro')) %>%
    mutate(gpp_diff = GPP_future - GPP_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(GPP_retro, .by_group = T) %>%
    mutate(cumsum_gpp_diff = abs(cumsum(gpp_diff))/ max(cumsum_gpp_retro) *100*ifelse(cumsum(gpp_diff)>0,1,-1)) %>%
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
  # emit = ggplot(top_emit_future, aes(x = cumsum_frac_emit_retro, y = cumsum_emit_diff/10^9, linetype = period_future,
  #             group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
  #   geom_line(size = 2) +
  #   theme_classic() +
  #   ylab(expression(Cumulative~Emissions~Difference~(Gg~C~year^-1))) +
  #   xlab(expression(Fraction~of~Total~Emissions))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_text(size =14),
  #         legend.position = c(.25,.8),
  #         legend.background = element_blank(),
  #         legend.text = element_text(size = 14))+
  #   geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
  #   scale_color_continuous(guide = guide_colorbar(title = expression(Runoff+Baseflow~(mm~yr^-1))),
  #     low = 'lightblue',high = 'darkblue') +
  #   scale_linetype_manual(name = 'period_future',
  #                         values = c('2050s' = 'twodash',
  #                                    '2080s' = 'solid'),
  #                         labels = c('2050\'s', '2080\'s'),
  #                         guide = guide_legend(title = expression(Period)))


  inset_lim_e = .1 # where 90% of emissions occur
  inset_xmin_e = mean(top_emit_future$frac_lakes_retro[which(round(top_emit_future$cumsum_frac_emit_retro,digits = 3) == inset_lim_e)])
  inset_xmax_e = 1.0
  inset_ymin_e = min(top_emit_future$cumsum_emit_diff)
  inset_ymax_e = max(top_emit_future$cumsum_emit_diff)

  emit = ggplot(top_emit_future, aes(x = frac_lakes_retro, y = cumsum_emit_diff, linetype = period_future,
                              group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 2) +
    geom_rect(mapping = aes(xmin = inset_xmin_e, xmax = inset_xmax_e, ymin = inset_ymin_e, ymax= inset_ymax_e), fill = NA,
              color = 'black' , linetype = 'dotted', show.legend = F, size = 1) +
    theme_classic() +
    ylab(expression(Cumulative~Delta~Emissions~('%'))) +
    xlab(expression(Cumulative~Fraction~of~Lakes))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))

  emit_inset = ggplot(top_emit_future, aes(x = frac_lakes_retro, y = cumsum_emit_diff, linetype = period_future,
                                           group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 1.5, show.legend = F) +
    xlim(c(inset_xmin_e, inset_xmax_e)) +
    theme_classic() +
    ylab(expression(Cumulative~Delta~Emissions~('%'))) +
    xlab(expression(Cumulative~Fraction~of~Lakes))+
    theme(axis.text = element_text(size=12),
          axis.title = element_blank(),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))


#
#   breaks = seq(0,1,length.out = 11)
#   area_breaks = c(0,1e3,1e4,1e5,1e6,1e7,1e8,1e9,1e10)
#
#   lake_summary <- top_emit_future %>%
#     mutate(frac_bins = cut(frac_lakes_retro, breaks = breaks),
#            area_bins =cut(Area_future, breaks = area_breaks)) %>%
#     group_by(frac_bins) %>%
#     summarise(area = min(Area_retro),
#               hrt = mean(HRT_retro),
#               emit_areal = max(Emit_areal_retro),
#               doc = mean(doc_conc_retro),
#               frac_evap = mean(percentEvap_retro)) %>%
#     ungroup()
#
#   lake_summary
#
#   ggplot(lake_summary, aes(x = frac_lakes_retro, y = Area_retro/ 10000)) +
#     geom_point() +
#     scale_y_log10() +
#     geom_point(aes(x=frac_lakes_retro, y= Emit_areal_retro), color= 'blue') +
#     geom_smooth(aes(x=frac_lakes_retro, y= Emit_areal_retro), method = 'lm')
#
#   ggplot(lake_summary, aes(x = area_bins, y = Emit_areal_future)) +
#     geom_boxplot() +
#     scale_y_log10()
#
#     ggplot(top_emit_future, aes(x = frac_lakes_retro, y = cumsum_emit_diff/10^9, linetype = period_future,
#                                 group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
#       geom_line(size = 2) +
#       theme_classic() +
#       ylab(expression(Cumulative~Emissions~Difference~(Gg~C~year^-1))) +
#       xlab(expression(Fraction~of~Total~Emissions))+
#       theme(axis.text = element_text(size=16),
#             axis.title = element_text(size = 16),
#             legend.title = element_text(size =14),
#             legend.position = c(.25,.8),
#             legend.background = element_blank(),
#             legend.text = element_text(size = 14))+
#       geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
#       scale_color_continuous(guide = guide_colorbar(title = expression(Runoff+Baseflow~(mm~yr^-1))),
#                              low = 'lightblue',high = 'darkblue') +
#       scale_linetype_manual(name = 'period_future',
#                             values = c('2050s' = 'twodash',
#                                        '2080s' = 'solid'),
#                             labels = c('2050\'s', '2080\'s'),
#                             guide = guide_legend(title = expression(Period)))
      # geom_vline(xintercept = top_emit_future$cumsum_frac_emit_retro[which(round(top_emit_future$frac_lakes_retro,digits = 4) %in% c(.9))],
      #            linetype = 'dashed')

  inset_lim_b = .1 # where 90% of emissions occur
  inset_xmin_b = mean(top_bury_future$frac_lakes_retro[which(round(top_bury_future$cumsum_frac_bury_retro,digits = 3) == inset_lim_b)])
  inset_xmax_b = 1.0
  inset_ymin_b = min(top_bury_future$cumsum_bury_diff)
  inset_ymax_b = max(top_bury_future$cumsum_bury_diff)

  bury = ggplot(top_bury_future, aes(x = frac_lakes_retro, y = cumsum_bury_diff, linetype = period_future,
                                     group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 1.5,show.legend = F) +
    geom_rect(mapping = aes(xmin = inset_xmin_b, xmax = inset_xmax_b, ymin = inset_ymin_b, ymax= inset_ymax_b), fill = NA,
              color = 'black' , linetype = 'dotted', show.legend = F, size = 1) +
    theme_classic() +
    ylab(expression(Cumulative~Delta~Burial~('%'))) +
    xlab(expression(Cumulative~Fraction~of~Lakes))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))

  bury_inset = ggplot(top_bury_future, aes(x = frac_lakes_retro, y = cumsum_bury_diff, linetype = period_future,
                                           group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 1.5, show.legend = F) +
    xlim(c(inset_xmin_b, inset_xmax_b)) +
    theme_classic() +
    ylab(expression(Cumulative~Delta~Burial~('%'))) +
    xlab(expression(Cumulative~Fraction~of~Lakes))+
    theme(axis.text = element_text(size=12),
          axis.title = element_blank(),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))


  high_bury = top_bury_future %>%
    dplyr::filter(frac_lakes_retro > inset_xmin_b)

  hi = high_bury %>%
    select(Area_future, percentEvap_future) %>%
    summarise_all(median)

  #
#   emit_minus_bury = ggplot(emit_minus_bury_future, aes(x = cumsum_frac_emit_minus_bury_retro,
#                                                        y = cumsum_emit_minus_bury_diff/10^9, linetype = period_future,
#                                      group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
#     geom_line(size = 2) +
#     theme_classic() +
#     ylab(expression(Cumulative~Emit-Burial~Difference~(Gg~C~year^-1))) +
#     xlab(expression(Fraction~of~Total~Emit-Burial))+
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
                          # guide = guide_legend(title = expression(Period)))

  inset_lim_g = .1 # where 90% of gpp occur
  inset_xmin_g = mean(top_gpp_future$frac_lakes_retro[which(round(top_gpp_future$cumsum_frac_gpp_retro,digits = 3) == inset_lim_b)])
  inset_xmax_g = 1.0
  inset_ymin_g = min(top_gpp_future$cumsum_gpp_diff)
  inset_ymax_g = max(top_gpp_future$cumsum_gpp_diff)

  gpp = ggplot(top_gpp_future, aes(x = frac_lakes_retro, y = cumsum_gpp_diff, linetype = period_future,
                                     group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 1.5,show.legend = F) +
    geom_rect(mapping = aes(xmin = inset_xmin_g, xmax = inset_xmax_g, ymin = inset_ymin_g, ymax= inset_ymax_g), fill = NA,
              color = 'black' , linetype = 'dotted', show.legend = F, size = 1) +
    theme_classic() +
    ylab(expression(Cumulative~Delta~GPP~('%'))) +
    xlab(expression(Cumulative~Fraction~of~Lakes))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))

  gpp_inset = ggplot(top_gpp_future, aes(x = frac_lakes_retro, y = cumsum_gpp_diff, linetype = period_future,
                                           group = interaction(period_future, gcm_future), color = Runoff_and_baseflow_future)) +
    geom_line(size = 1.5, show.legend = F) +
    xlim(c(inset_xmin_g, inset_xmax_g)) +
    theme_classic() +
    ylab(expression(Cumulative~Delta~Burial~('%'))) +
    xlab(expression(Cumulative~Fraction~of~Lakes))+
    theme(axis.text = element_text(size=12),
          axis.title = element_blank(),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    scale_linetype_manual(name = 'period_future',
                          values = c('2050s' = 'twodash',
                                     '2080s' = 'solid'),
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))

  high_gpp = top_gpp_future %>%
    dplyr::filter(frac_lakes_retro > inset_xmin_g)

  hi = high_gpp %>%
    select(Area_future, percentEvap_future) %>%
    summarise_all(median)


  # g = plot_grid(emit, bury, labels = c('A', 'B'), align = 'h')

  g = ggdraw() +
    draw_plot(emit, x = 0, y = 0, width = .5, height = 1) +
    draw_plot(emit_inset, x= .1, y= .1, width = .18, height = .38) +
    draw_plot(bury, x = 0.5, y = 0, width = .5, height = 1) +
    draw_plot(bury_inset, x= .6, y= .1, width = .18, height = .38)


  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 14, height = 7)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
