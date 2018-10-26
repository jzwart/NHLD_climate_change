fig_lake_stage <- function(fig_ind, transparent, scenarios, drivers_file, fig_cfg_yml, remake_file, gd_config){
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

  stage_fhee = ggplot(stage_future, aes(x = percentEvap_retro, y = stage_diff, color = (Precip_future - Evap_future))) +
    geom_point(size = 2, pch=16, alpha = .2) +
    theme_classic() +
    ylab(expression(Stage~Difference~(Future-Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.2),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_smooth(aes(x = percentEvap_retro, y = stage_diff, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid') +
    geom_hline(yintercept = 0, linetype = 'dashed', size =1)

  stage_fhee

  area_fhee = ggplot(stage_future, aes(x = percentEvap_retro, y = area_ratio, color = (Precip_future - Evap_future))) +
    geom_point(size = 2, pch=16, alpha = .2) +
    theme_classic() +
    ylab(expression(Area~Ratio~(Future:Historic))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.25,.2),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_smooth(aes(x = percentEvap_retro, y = area_ratio, color = (Precip_future - Evap_future), group = (Precip_future - Evap_future)),
                method = 'loess', se = F, inherit.aes = F, size = 2, linetype = 'solid') +
    geom_hline(yintercept = 1, linetype = 'dashed', size =1)

  area_fhee


  stage_doc_ratio = ggplot(dplyr::filter(stage_future, doc_conc_retro <=40),
                           aes(y = doc_conc_future/doc_conc_retro, x = stage_diff, color = (Precip_future - Evap_future))) +
    geom_point(pch =16, alpha =.4, size = 2) +
    theme_classic() +
    ylab(expression(DOC~Ratio~(Future:Historic))) +
    xlab(expression(Stage~Difference~(Future-Historic)))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.85,.8),
          legend.background = element_blank(),
          legend.text = element_text(size = 14))+
    scale_color_continuous(guide = guide_colorbar(title = expression(Precip-Evap~(mm~yr^-1))),
                           low = 'lightblue',high = 'darkblue') +
    geom_hline(yintercept = 1, linetype ='dashed', color ='black',size = 1) +
    geom_vline(xintercept = 0, linetype ='dashed', color ='black',size =1)

  stage_doc_ratio

  g = plot_grid(stage_fhee, stage_doc_ratio, labels = c('A', 'B', 'C'), align = 'h')


  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 14, height = 7)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
