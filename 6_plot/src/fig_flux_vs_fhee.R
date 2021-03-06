fig_flux_vs_fhee <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){
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
           GPP = GPP * Vepi,
           NEP = GPP*.15 - DOC_Respired) %>%
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Area, DOC_load, HRT, Stage, Vol, doc_conc,emergent_d_epi,pH,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins, percentEvap, GPP, Vepi, dicLoadvResp, FracRet, fracCO2,
           DOC_Respired, NEP)

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

  region_retro <- c_and_drivers %>%
    dplyr::filter(gcm == 'Retro') %>%
    mutate(tmp = 'a')

  region_future <- c_and_drivers %>%
    dplyr::filter(gcm != 'Retro') %>%
    mutate(tmp = 'a') %>%
    left_join(region_retro, by = 'tmp', suffix = c('_future','_retro')) %>%
    select(-tmp)

  stage <- all %>%
    group_by(period, gcm) %>%
    arrange(percentEvap, .by_group = T) %>%
    ungroup() %>%
    left_join(ave_drivers, by = c('period' = 'period','gcm' = 'gcm'))

  stage_retro <- stage %>%
    dplyr::filter(gcm == 'Retro')

  stage_future <- stage %>%
    dplyr::filter(gcm != 'Retro') %>%
    left_join(stage_retro, by = 'Permanent_', suffix = c('_future','_retro')) %>%
    mutate(stage_ratio = Stage_future / Stage_retro,
           stage_diff = Stage_future - Stage_retro,
           area_ratio = Area_future / Area_retro) %>%
    group_by(period_future, gcm_future) %>%
    arrange(percentEvap_retro, .by_group = T) %>%
    ungroup()

  retro <- all %>%
    dplyr::filter(gcm == 'Retro')

  merged <- dplyr::filter(all, gcm != 'Retro') %>%
    left_join(retro, by = 'Permanent_', suffix = c('_future', '_retro'))

  merged$doc_change = merged$doc_conc_future/merged$doc_conc_retro

  # picking the wettest and driest scenario for two time periods to see if it can help improve clarity of figure
  #   wettest = GFDL_CM3 ; driest = HadGEM2_AO
  # stage_future = stage_future %>%
  #   dplyr::filter(gcm_future %in% c('GFDL_CM3','HadGEM2_AO'))


  emit = ggplot(stage_future,
         aes(y = abs(Emit_future-Emit_retro)/Emit_retro*100 * ifelse(Emit_future > Emit_retro, 1, -1),
             x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~mol~C~emissions~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    # scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
    #                        low = 'lightblue',high = 'darkblue') +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                                               begin = 0, end = 1, direction = -1) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = abs(Emit_future-Emit_retro)/Emit_retro*100 * ifelse(Emit_future > Emit_retro, 1, -1),
                    x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future),
                    linetype = period_future),
                method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
    ylim(c(-60,60))
    # ylim(c(.3,1.7))
  emit = ggExtra::ggMarginal(emit, type = 'density', groupColour = T, size = 6, aes(size = 2))

  bury = ggplot(stage_future,
                aes(y = abs(Bury_future-Bury_retro)/Bury_retro*100*ifelse(Bury_future > Bury_retro,1,-1),
                    x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~mol~C~burial~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    # scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
    #                        low = 'lightblue',high = 'darkblue') +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = abs(Bury_future-Bury_retro)/Bury_retro*100*ifelse(Bury_future > Bury_retro,1,-1),
                    x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future),
                    linetype = period_future),
                method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
    ylim(c(-60,60))
    # ylim(c(.2,2))
  bury = ggExtra::ggMarginal(bury, type = 'density', groupColour = T, size = 6, aes(size = 2))

  e_b = ggplot(stage_future,
               aes(y = abs((Emit_future-Bury_future)-(Emit_retro-Bury_retro))/(Emit_retro-Bury_retro)*100*
                     ifelse((Emit_future-Bury_future)>(Emit_retro-Bury_retro),1,-1),
                   x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~(mol~C~emissions-mol~C~burial)~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_text(size =14),
          legend.position = c(.25,.75),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    # scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
    #                        low = 'lightblue',high = 'darkblue') +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = abs((Emit_future-Bury_future)-(Emit_retro-Bury_retro))/(Emit_retro-Bury_retro)*100*
                      ifelse((Emit_future-Bury_future)>(Emit_retro-Bury_retro),1,-1),
                    x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future),
                    linetype = period_future),
                method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
    ylim(c(-80,150)) +
    scale_linetype_discrete(name='',
                            labels = c('2050\'s', '2080\'s'))
  # ylim(c(.2,2))
  e_b = ggExtra::ggMarginal(e_b, type = 'density', margins = 'y', groupColour = T, size = 6, aes(size = 2))

  gpp = ggplot(stage_future,
               aes(y = abs((GPP_future)-(GPP_retro))/(GPP_retro)*100*ifelse((GPP_future)>(GPP_retro),1,-1),
                   x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~mol~C~GPP~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.75),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    # scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
    #                        low = 'lightblue',high = 'darkblue') +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = abs((GPP_future)-(GPP_retro))/(GPP_retro)*100*ifelse((GPP_future)>(GPP_retro),1,-1),
                    x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future),
                    linetype = period_future),
                method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
    ylim(c(-100,100)) +
    scale_linetype_discrete(name='',
                            labels = c('2050\'s', '2080\'s'))

  gpp = ggExtra::ggMarginal(gpp, type = 'density', margins = 'y', groupColour = T, size = 6, aes(size = 2))

  nep = ggplot(stage_future,
               aes(y = abs((NEP_future)-(NEP_retro))/abs(NEP_retro)*100*ifelse((NEP_future)>(NEP_retro),1,-1),
                   x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~mol~C~NEP~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.75),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    # scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
    #                        low = 'lightblue',high = 'darkblue') +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = abs((NEP_future)-(NEP_retro))/abs(NEP_retro)*100*ifelse((NEP_future)>(NEP_retro),1,-1),
                    x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future),
                    linetype = period_future),
                method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F) +
    ylim(c(-100,100)) +
    scale_linetype_discrete(name='',
                            labels = c('2050\'s', '2080\'s'))

  nep = ggExtra::ggMarginal(nep, type = 'density', margins = 'y', groupColour = T, size = 6, aes(size = 2))


  frac_ret = ggplot(stage_future,
               aes(y = abs(FracRet_future*100-FracRet_retro*100)*ifelse(FracRet_future>FracRet_retro,1,-1), x = percentEvap_future,
                   color = (Precip_future - Evap_future), group = interaction(period_future, gcm_future),
                   linetype = period_future)) +
    geom_point(pch =16, alpha =.08, size = 2) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    theme_classic() +
    ylab(expression(Delta~percent~C~removed~('%'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_text(size =14),
          legend.position = c(.25,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    # scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
    #                        low = 'lightblue',high = 'darkblue') +
    scale_color_viridis(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                        begin = 0, end = 1, direction = -1) +
    geom_smooth(aes(y = abs(FracRet_future*100-FracRet_retro*100)*ifelse(FracRet_future>FracRet_retro,1,-1), x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = interaction(period_future, gcm_future), linetype = period_future),
                method = 'loess', se = F, inherit.aes = F, size = 2) +
    ylim(-30,30) +
    scale_linetype_discrete(name = 'period_future',
                          labels = c('2050\'s', '2080\'s'),
                          guide = guide_legend(title = expression(Period)))

  frac_ret

  frac_ret = ggExtra::ggMarginal(frac_ret, type = 'density', margins = 'y', groupColour = T, size = 6, aes(size = 2))


  g = plot_grid(emit, bury, e_b, frac_ret, gpp, nep,
                labels = c('a', 'b', 'c', 'd', 'e','f'), align = 'hv',nrow = 3)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 14, height = 20)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
