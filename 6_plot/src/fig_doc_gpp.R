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
           Precip_lake = DirectP / Area,
           NEP = GPP*Vepi*.15-DOC_Respired) %>%
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Burial_phyto, Burial_tPOC, Burial_total, Area, DOC_load,
           HRT, Stage, Vol, doc_conc, dicLoadvResp, DOC_Respired, Sed_phyto, Sed_tPOC, NEP, Vepi,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins, percentEvap, GPP, FracRet)

  sum <- readRDS(scenarios) %>%
    dplyr::filter(season == 'open_water') %>%
    select(Permanent_, period, gcm, pco2)

  all <- left_join(all, sum, by = c('Permanent_' = 'Permanent_', 'period' = 'period', 'gcm' = 'gcm'))

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
    dplyr::filter(gcm == 'Retro') %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  merged <- dplyr::filter(all, gcm != 'Retro') %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm')) %>%
    left_join(retro, by = 'Permanent_', suffix = c('_future', '_retro'))

  merged$doc_change = merged$doc_conc_future/merged$doc_conc_retro

  # ggplot(dplyr::filter(merged, doc_conc_retro <40), aes(x = doc_conc_retro, y = GPP_future/GPP_retro)) +
  #   geom_point(aes( colour = doc_change)) +
  #   scale_color_gradient2(low = 'darkred', mid = t_col('grey95',30), high = 'darkblue', midpoint = 1.1) +
  #   ylim(c(0,4))
  #
  #   scale_alpha_continuous(range = c(.1,.4))
  #
  #   ylab(label = 'GPP Future : GPP Historic') +
  #   xlab(label = 'Historic DOC Concentration') +
  #   geom_hline(yintercept = 1, linetype ='dashed', color ='red') +
  #   geom_point(data= dplyr::filter(merged, doc_conc_retro <30, doc_change < .9),
  #              aes(x = doc_conc_retro, y = GPP_future/GPP_retro), color ='red')

  # picking the wettest and driest scenario for two time periods to see if it can help improve clarity of figure
  #   wettest = GFDL_CM3 ; driest = HadGEM2_AO
  doc_future = doc_future %>%
    dplyr::filter(gcm_future %in% c('GFDL_CM3','HadGEM2_AO'))


  doc_fhee = ggplot(doc_future,
               aes(y = abs(doc_conc_future-doc_conc_retro)/doc_conc_retro*100*ifelse(doc_conc_future>doc_conc_retro,1,-1),
                   x = percentEvap_future,
                   color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2) +
    theme_classic() +
    ylab(expression(Delta~DOC~concentration~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.35,.7),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    # scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
    #                        low = 'lightblue',high = 'darkblue') +
    scale_color_viridis_c(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1)+
    geom_smooth(aes(y = abs(doc_conc_future-doc_conc_retro)/doc_conc_retro*100*ifelse(doc_conc_future>doc_conc_retro,1,-1),
                    x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future),
                    linetype = period_future),
                method = 'loess', se = F, inherit.aes = F, size = 2) +
    ylim(c(-30,150))+
    scale_linetype_discrete(name='',
                            labels = c('2050\'s', '2080\'s'))

  doc_fhee = ggExtra::ggMarginal(doc_fhee, type = 'density',groupColour = T, size = 6, aes(size = 2))

  # picking the wettest and driest scenario for two time periods to see if it can help improve clarity of figure
  #   wettest = GFDL_CM3 ; driest = HadGEM2_AO
  merged = merged %>%
    dplyr::filter(gcm_future %in% c('GFDL_CM3','HadGEM2_AO'))


  gpp_doc_ratio = ggplot(dplyr::filter(merged, doc_conc_retro <=40),
                         aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
                             y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1), color = doc_conc_retro)) +
    geom_point() +
    ylim(c(-100,250)) +
    xlim(c(-30,150)) +
    theme_classic() +
    xlab(expression(Delta~DOC~concentration~('%'))) +
    ylab(expression(Delta~volumetric~GPP~('%')))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.4,.85),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Historic~DOC~(mg~L^-1))),
                           low = 'orange',high = 'darkblue') +
    geom_hline(yintercept = 0, linetype ='dashed', color ='black',size = 1) +
    geom_vline(xintercept = 0, linetype ='dashed', color ='black',size =1)

  # gpp_doc_ratio

  low_doc = ggplot(dplyr::filter(merged, doc_conc_retro <= 10),
                   aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
                       y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1),
                       color = percentEvap_future)) +
    geom_point() +
    ylim(c(-100,250)) +
    xlim(c(-30,150)) +
    theme_classic() +
    xlab(expression(Delta~DOC~('%'))) +
    ylab(expression(Delta~GPP~('%')))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.4,.85),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(FHEE)),
                           low = 'orange',high = 'darkblue') +
    geom_hline(yintercept = 0, linetype ='dashed', color ='black',size = 1) +
    geom_vline(xintercept = 0, linetype ='dashed', color ='black',size =1)

  high_doc = ggplot(dplyr::filter(merged, doc_conc_retro >=15),
                   aes(x = abs(doc_conc_future - doc_conc_retro)/doc_conc_retro *100*ifelse(doc_conc_future > doc_conc_retro,1,-1),
                       y = abs(GPP_future-GPP_retro)/GPP_retro*100*ifelse(GPP_future>GPP_retro,1,-1),
                       color = percentEvap_future)) +
    geom_point() +
    ylim(c(-100,250)) +
    xlim(c(-30,150)) +
    theme_classic() +
    xlab(expression(Delta~DOC~('%'))) +
    ylab(expression(Delta~GPP~('%')))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.4,.85),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(FHEE)),
                           low = 'orange',high = 'darkblue') +
    geom_hline(yintercept = 0, linetype ='dashed', color ='black',size = 1) +
    geom_vline(xintercept = 0, linetype ='dashed', color ='black',size =1)


  g = plot_grid(doc_fhee, gpp_doc_ratio, labels = c('a', 'b'), align = 'h')

  # g = plot_grid(doc_fhee, low_doc, high_doc, labels = c('a','b','c'), align = 'h', rows = 1)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 14, height = 7)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
