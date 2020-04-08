fig_hrt_fhee <- function(fig_ind,
                         transparent,
                         scenarios,
                         drivers_file,
                         fig_cfg_yml,
                         remake_file,
                         gd_config){

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

  # Zach's color scheme
  # map = [93, 93, 93;
  #       219, 96, 48;
  #       0,    0,  0]/256;
  col_2050s = rgb(93/256,93/256,93/256)
  col_2080s = rgb(219/256,96/256,48/256)
  col_historic = rgb(0,0,0)

  hrt = ggplot(stage_future,
         aes(y = (HRT_future-HRT_retro)/365, x = percentEvap_future,
             color = period_future, group = period_future)) +
    geom_point(pch =16, alpha =.28, size = 2, show.legend = F) +
    theme_classic() +
    ylab(expression(Delta~HRT~('years'))) +
    xlab(expression(FHEE))+
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size = 16),
          legend.title = element_text(size =14),
          legend.position = c(.2,.9),
          legend.background = element_blank(),
          legend.text = element_text(size = 14),
          panel.border = element_rect(color = 'black', fill=NA, size=1))+
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = col_2050s,
                                  '2080s' = col_2080s),
                       labels = c('2050\'s', '2080\'s'))+
    geom_hline(yintercept = 0, linetype = 'dashed', size =1) +
    geom_smooth(aes(y = (HRT_future-HRT_retro)/365, x = percentEvap_future,
                    group = period_future,
                    linetype = period_future),color = 'black',
                method = 'loess', se = F, inherit.aes = F, size = 2, show.legend = F)+
    xlim(c(0,1))

  hrt = ggExtra::ggMarginal(hrt, type = 'density', groupColour = T, size = 8, aes(size = 8))

  hrt


  # g = plot_grid(emit, bury, e_b, frac_ret, gpp, nep,
  #               labels = c('a', 'b', 'c', 'd', 'e','f'), align = 'hv',nrow = 3)

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=hrt, width = 7, height = 7)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
