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
           Precip_lake = DirectP / Area) %>%
    select(Permanent_, period, gcm, Emit, Bury, Emit_areal, Bury_areal, Area, DOC_load, HRT, Stage, Vol, doc_conc,emergent_d_epi,pH,
           waterIn, fluvialOut, Precip_lake, ndays_ice, epiTemp, lakeSizeBins, percentEvap, GPP, dicLoadvResp, FracRet)

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

  emit = ggplot(stage_future,
         aes(y = Emit_future/Emit_retro, x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Emissions~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = Emit_future/Emit_retro, x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid', show.legend = F) +
    ylim(c(.3,1.7))
  emit = ggExtra::ggMarginal(emit, type = 'density', groupColour = T, size = 6, aes(size = 2))

  bury = ggplot(stage_future,
                aes(y = Bury_future/Bury_retro, x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~Burial~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = Bury_future/Bury_retro, x = percentEvap_future, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid', show.legend = F) +
    ylim(c(.2,2))
  bury = ggExtra::ggMarginal(bury, type = 'density', groupColour = T, size = 6, aes(size = 2))

  dic_v_resp = ggplot(stage_future,
                aes(y = dicLoadvResp_future/dicLoadvResp_retro, x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~DIC~Load~to~DIC~Produced~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = dicLoadvResp_future/dicLoadvResp_retro, x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid', show.legend = F) +
    ylim(c(.4,1.7))
  dic_v_resp = ggExtra::ggMarginal(dic_v_resp, type = 'density', margins = 'y',groupColour = T, size = 6, aes(size = 2))

  doc = ggplot(stage_future,
                aes(y = doc_conc_future/doc_conc_retro, x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2) +
    theme_classic() +
    ylab(expression(Delta~DOC~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.35,.7),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1)+
    geom_smooth(aes(y = doc_conc_future/doc_conc_retro, x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid') +
    ylim(c(.5,3))

  doc = ggExtra::ggMarginal(doc, type = 'density', margins = 'y',groupColour = T, size = 6, aes(size = 2))

  hrt = ggplot(stage_future,
               aes(y = HRT_future/HRT_retro, x = percentEvap_future,
                   color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1) +
    theme_classic() +
    ylab(expression(Delta~HRT~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_smooth(aes(y = HRT_future/HRT_retro, x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid', show.legend = F) +
    ylim(c(.3,1.8))

  hrt = ggExtra::ggMarginal(hrt, type = 'density', margins = 'y', groupColour = T, size = 6, aes(size = 2))

  stage = ggplot(stage_future,
               aes(y = stage_ratio, x = percentEvap_future,
                   color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1) +
    theme_classic() +
    ylab(expression(Delta~Stage~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_smooth(aes(y = stage_ratio, x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid', show.legend = F)

  stage = ggExtra::ggMarginal(stage, type = 'density', margins = 'y', groupColour = T, size = 6, aes(size = 2))


  ph=ggplot(stage_future,
            aes(y = pH_future - pH_retro, x = percentEvap_future,
                color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    theme_classic() +
    ylab(expression(Delta~pH~(Future-Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_smooth(aes(y = pH_future - pH_retro, x = percentEvap_future,
                    color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid', show.legend = F)+
    ylim(c(-.5,.5))
  ph=ggExtra::ggMarginal(ph, type = 'density', margins = 'y',groupColour = T, size = 6, aes(size = 2))

  # frac_ret = ggplot(stage_future,
  #              aes(y = FracRet_future/FracRet_retro, x = percentEvap_future,
  #                  color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
  #   geom_point(pch =16, alpha =.08, size = 2, show.legend = F) +
  #   geom_hline(yintercept = 1, linetype = 'dashed', size =1) +
  #   theme_classic() +
  #   ylab(expression(Delta~HRT~(Future:Historic))) +
  #   xlab(expression(FHEE))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_text(size =14),
  #         legend.position = c(.2,.9),
  #         legend.background = element_blank(),
  #         legend.text = element_text(size = 14))+
  #   scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
  #                          low = 'lightblue',high = 'darkblue') +
  #   geom_smooth(aes(y = FracRet_future/FracRet_retro, x = percentEvap_future,
  #                   color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
  #               method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid', show.legend = F) +
  #   ylim(c(.5,3))
  #
  # hrt = ggExtra::ggMarginal(hrt, type = 'density', margins = 'y', groupColour = T, size = 6, aes(size = 2))


  gpp_doc_ratio = ggplot(dplyr::filter(stage_future, doc_conc_retro <=40),
                         aes(x = doc_conc_future/doc_conc_retro, y = GPP_future/GPP_retro,
                             color = doc_conc_retro, group =(Precip_future - Evap_future))) +
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

  # d = ggplot(stage_future,
  #            aes(y = emergent_d_epi_future/emergent_d_epi_retro, x = percentEvap_future,
  #                color = (Precip_future - Evap_future), group = (Precip_future - Evap_future))) +
  #   geom_point(pch =16, alpha =.08, size = 2) +
  #   theme_classic() +
  #   ylab(expression(DOC~Turnover~Rate~(Future:Historic))) +
  #   xlab(expression(FHEE))+
  #   theme(axis.text = element_text(size=16),
  #         axis.title = element_text(size = 16),
  #         legend.title = element_text(size =14),
  #         legend.position = c(.2,.9),
  #         legend.background = element_blank(),
  #         legend.text = element_text(size = 14))+
  #   scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
  #                          low = 'lightblue',high = 'darkblue') +
  #   geom_smooth(aes(y = emergent_d_epi_future/emergent_d_epi_retro, x = percentEvap_future,
  #                   color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
  #               method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid') +
  #   geom_hline(yintercept = 1, linetype = 'dashed', size =1) +
  #   ylim(c(.3,1.8))
  #
  # d = ggExtra::ggMarginal(d, type = 'density', groupColour = T, size = 6, aes(size = 2))

  g = plot_grid(emit, bury, hrt, stage, dic_v_resp, doc,
                labels = c('A', 'B', 'C', 'D','E','F'), align = 'hv',nrow = 3)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 14, height = 20)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
