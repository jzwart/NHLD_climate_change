fig_doc_gpp <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){
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
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins, percentEvap, GPP)

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
              DOC = median(doc_conc),
              GPP = sum(GPP)) %>%
    ungroup()

  ave_drivers <- drivers %>%
    group_by(period, gcm, var) %>%
    dplyr::summarise(var_value = sum(var_value)) %>%
    ungroup() %>%
    spread(key = 'var', value = 'var_value')

  c_and_drivers <- left_join(total, ave_drivers, by = c('period' = 'period', 'gcm' = 'gcm')) %>%
    mutate(period = factor(period, levels = c('Retro', '2050s', '2080s')))

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
    mutate(doc_ratio = doc_conc_future / doc_conc_retro,
           gpp_ratio = GPP_future / GPP_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(percentEvap_retro, .by_group = T) %>%
    ungroup()

  retro <- all %>%
    dplyr::filter(gcm == 'Retro')

  merged <- dplyr::filter(all, gcm != 'Retro') %>%
    left_join(retro, by = 'Permanent_', suffix = c('_future', '_retro'))

  merged$doc_change = merged$doc_conc_future/merged$doc_conc_retro

  # ggplot(dplyr::filter(merged, doc_conc_retro <30, doc_change > 1.2), aes(x = doc_conc_retro, y = GPP_future/GPP_retro, color = doc_change)) +
  #   geom_point() +
  #   ylim(c(0,4)) +
  #   ylab(label = 'GPP Future : GPP Historic') +
  #   xlab(label = 'Historic DOC Concentration') +
  #   geom_hline(yintercept = 1, linetype ='dashed', color ='red') +
  #   geom_point(data= dplyr::filter(merged, doc_conc_retro <30, doc_change < .9),
  #              aes(x = doc_conc_retro, y = GPP_future/GPP_retro), color ='red')


  doc_fhee = ggplot(doc_future, aes(x = percentEvap_retro, y = doc_ratio, color = (Precip_future - Evap_future))) +
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
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_smooth(aes(x = percentEvap_retro, y = doc_ratio, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid') +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1)

  doc_fhee

  gpp_doc_ratio = ggplot(dplyr::filter(merged, doc_conc_retro <=40), aes(x = doc_change, y = GPP_future/GPP_retro, color = doc_conc_retro)) +
    geom_point() +
    ylim(c(0,4)) +
    xlim(c(.5,3)) +
    theme_classic() +
    xlab(expression(Delta~DOC~(Future:Historic))) +
    ylab(expression(Delta~GPP~(Future:Historic)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.85,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
                           low = 'orange',high = 'darkblue') +
    geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
    geom_vline(xintercept = 1, linetype ='dashed', color ='black',size =1)

  gpp_doc_ratio

  # g = plot_grid(doc_fhee, gpp_doc_ratio, labels = c('A', 'B'), align = 'h')

  g = gpp_doc_ratio

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 7, height = 7)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
