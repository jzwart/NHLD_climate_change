fig_month_box <- function(fig_ind, vars_ind_file, vars_yml, fig_cfg_yml, scenarios, var_lookup_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  var_lookup <- yaml::yaml.load_file(var_lookup_yml) # contains fig labels and units

  vars <- noquote(yaml::yaml.load_file(vars_yml)$var) # indicates which periods, seasons, and variable we want returned

  month <- readRDS(sc_retrieve(vars_ind_file, remake_file = remake_file))

  watersheds<-read.table('1_data/in/NHLDsheds_20170323.txt',
                         stringsAsFactors = F,
                         header=T,
                         sep = '\t')

  monthly_ave <- month %>%
    left_join(watersheds, by = 'Permanent_') %>%
    dplyr::filter(!is.na(Area_m2))

  # vars that we want plotted by watershed area or lake area
  monthly_ave <- monthly_ave %>%
    select(Permanent_, gcm, period, month, eval(vars)) %>%
    gather(key = 'var', value = 'month_mean', eval(vars)) %>% # turning into long format for plotting
    group_by(month, period, gcm, var) %>%
    summarise(med_all = median(month_mean)) %>%
    ungroup() %>%
    group_by(month, var, period) %>%
    summarise(med = median(med_all),
              min = min(med_all),
              max = max(med_all)) %>%
    ungroup() %>%
    mutate(month = as.Date(paste('2001',month,'01',sep='-'))) # creating date so it can plot on x-axis

  units <- var_lookup$units[vars] %>%
    bind_rows() %>% as.character()
  units <- paste(vars, '\n',units, sep ='')
  names(units) <- vars

  # box plot data
  box_plot_data <- data_frame()
  y <- c()
  for(i in 1:length(vars)){
    var_ind_file <- paste('2_analysis/out/', vars[i], '.rds.ind', sep = '')

    var_results <- readRDS(sc_retrieve(var_ind_file, remake_file = remake_file)) %>%
      dplyr::filter(season == 'all') %>%
      select(Permanent_, period, starts_with('ratio')) %>%
      tidyr::gather(key = 'var', value = 'ratio', starts_with('ratio'))

    box_plot_data <- bind_rows(box_plot_data, var_results)
    y <- c(y,var_lookup$ratio_var[vars[i]][[1]]) # what we want to plo
  }
  y = noquote(y)

  if('doc' %in% vars){
    box_plot_data <- box_plot_data %>%
      dplyr::filter(!var %in% c('ratio_doc_epi','ratio_doc_hypo'))
  }


  all <- readRDS(scenarios) %>%
    dplyr::filter(season == 'all') %>%
    select(Permanent_, period, gcm, Emit, Burial_total, Area, Stage, DOC_Load, HRT, Vol,FracRet, DOC_export, DOCl_epi, DOCr_epi, GPP,
           waterIn, fluvialOut, epiTemp, emergent_d_epi, doc_conc, dicLoadvResp, Vepi, DOC_Respired, sed_resp, DIC_load)# %>%
    #mutate(GPP = GPP * 12 * Vepi / Area)

  retro_total <- all %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
                     Bury = sum(Burial_total),
                     Area = sum(Area),
                     Water_in = sum(waterIn),
                     Vol = sum(Vol),
                     FracRet = sum(DOC_Load * FracRet) / sum(DOC_Load),
                     DOC_Load = sum(DOC_Load),
                     DOC_Export = sum(DOC_export),
                     DOC_Conc = sum(DOCl_epi,DOCr_epi) / sum(Vepi) * 12,
                     DICLoad_v_Resp = sum(DIC_load) / sum(DOC_Respired, sed_resp),
                     D_epi = sum(emergent_d_epi * Vepi) / sum(Vepi)) %>%
    ungroup() %>%
    dplyr::filter(gcm == 'Retro') %>%
    mutate(tmp = 'a')

  merged_total <- all %>%
    group_by(period, gcm) %>%
    dplyr::summarise(Emit = sum(Emit),
                     Bury = sum(Burial_total),
                     Area = sum(Area),
                     Water_in = sum(waterIn),
                     Vol = sum(Vol),
                     FracRet = sum(DOC_Load * FracRet) / sum(DOC_Load),
                     DOC_Load = sum(DOC_Load),
                     DOC_Export = sum(DOC_export),
                     DOC_Conc = sum(DOCl_epi,DOCr_epi) / sum(Vepi) * 12,
                     DICLoad_v_Resp = sum(DIC_load) / sum(DOC_Respired, sed_resp),
                     D_epi = sum(emergent_d_epi * Vepi) / sum(Vepi)) %>%
    ungroup() %>%
    dplyr::filter(gcm != 'Retro') %>%
    mutate(tmp = 'a') %>%
    left_join(retro_total, by = 'tmp', suffix = c('_future','_retro')) %>%
    select(-tmp,-period_retro, -gcm_retro)


  month_doc = ggplot(dplyr::filter(monthly_ave, var == 'doc'), aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro', var == 'doc'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(DOC~(mg~C~L^-1))) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_bury = ggplot(dplyr::filter(monthly_ave, var == 'bury'), aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro', var == 'bury'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(C~Burial~(mol~C~day^-1))) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_emit = ggplot(dplyr::filter(monthly_ave, var == 'emit'), aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro', var == 'emit'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line() +
    theme_classic() +
    ylab(bquote(CO[2]~Emissions~(mol~C~day^-1))) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.position = c(.75,.9),
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 10)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_d_epi = ggplot(dplyr::filter(monthly_ave, var == 'd_epi'), aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro', var == 'd_epi'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(DOC~Turnover~(day^-1))) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_dic_v_resp = ggplot(dplyr::filter(monthly_ave, var == 'dic_v_resp'), aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro', var == 'dic_v_resp'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(bquote(DIC~Load~to~DIC~Produced)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  month_frac_ret = ggplot(dplyr::filter(monthly_ave, var == 'frac_ret'), aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro', var == 'frac_ret'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(expression(Fraction~C~Mineralized)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))

  ggplot(dplyr::filter(monthly_ave, var == 'gpp'), aes(x = month, y = med, color = period, size = period, linetype = period)) +
    geom_ribbon(data = dplyr::filter(monthly_ave, period != 'Retro', var == 'gpp'),
                aes(x = month, y = med, ymax = max, ymin = min, color = period, fill = period),
                alpha = .2, size = .5, show.legend = F) +
    geom_line(show.legend = F) +
    theme_classic() +
    ylab(expression(Fraction~C~Mineralized)) +
    theme(axis.text = element_text(size=16),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16)) +
    scale_color_manual(name = 'period',
                       values = c('2050s' = fig_config$period$`2050s`,
                                  '2080s' = fig_config$period$`2080s`,
                                  'Retro' = fig_config$period$Retro),
                       labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`,
                                 'Retro' = fig_config$period$Retro),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_size_manual(name = 'period',
                      values = c('2050s' = 3,
                                 '2080s' = 3,
                                 'Retro' = 2),
                      labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_linetype_manual(name = 'period',
                          values = c('2050s' = 'solid',
                                     '2080s' = 'solid',
                                     'Retro' = 'dashed'),
                          labels = c('2050\'s', '2080\'s', 'Historic')) +
    scale_x_date(labels = scales::date_format('%b'))


  box_doc = ggplot(dplyr::filter(box_plot_data, var == 'ratio_doc_all'), aes(x = period, y = ratio, fill = period)) +
    geom_boxplot(outlier.shape = NA, show.legend = F, size = 1) +
    scale_y_continuous(limits = c(0.8, 1.4)) +
    theme_boxplot +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`),
                      labels = c('2050s', '2080s')) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank())

  emit_ratio = ggplot(merged, aes(x = period_future, y = Emit_future/Emit_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin(show.legend = F) +
    geom_point(data = merged_total, aes(x = period_future, y = Emit_future/Emit_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    theme_classic() +
    theme(axis.text = element_text(size=16),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, 80),
                                 '2080s' = t_col(fig_config$period$`2080s`, 80)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  bury_ratio = ggplot(merged, aes(x = period_future, y = Burial_total_future/Burial_total_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin(show.legend = F) +
    geom_point(data = merged_total, aes(x = period_future, y = Bury_future/Bury_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    theme_classic() +
    ylim(c(0,2.3)) +
    theme(axis.text = element_text(size=16),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, 80),
                                 '2080s' = t_col(fig_config$period$`2080s`, 80)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  dic_v_resp_ratio = ggplot(merged, aes(x = period_future, y = dicLoadvResp_future/dicLoadvResp_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin(show.legend = F) +
    geom_point(data = merged_total, aes(x = period_future, y = DICLoad_v_Resp_future/DICLoad_v_Resp_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    theme_classic() +
    ylim(c(0.2,4)) +
    theme(axis.text = element_text(size=16),
          axis.title.y = element_text(size = 16),
          axis.title.x = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, 80),
                                 '2080s' = t_col(fig_config$period$`2080s`, 80)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  frac_ret_ratio = ggplot(merged, aes(x = period_future, y = FracRet_future/FracRet_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin(show.legend = F) +
    geom_point(data = merged_total, aes(x = period_future, y = FracRet_future/FracRet_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    theme_classic() +
    ylim(c(0,2)) +
    theme(axis.text = element_text(size=16),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, 80),
                                 '2080s' = t_col(fig_config$period$`2080s`, 80)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  d_ratio = ggplot(merged, aes(x = period_future, y = emergent_d_epi_future/emergent_d_epi_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    geom_violin(show.legend = F) +
    geom_point(data = merged_total, aes(x = period_future, y = D_epi_future/D_epi_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    theme_classic() +
    # ylim(c(0,2.3)) +
    theme(axis.text = element_text(size=16),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, 80),
                                 '2080s' = t_col(fig_config$period$`2080s`, 80)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  doc_ratio = ggplot(merged, aes(x = period_future, y = doc_conc_future/doc_conc_retro, color = period_future, fill = period_future,
                                  size =period_future, shape = period_future)) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    # geom_violin(show.legend = F) +
    geom_boxplot(outlier.shape = NA, show.legend = F, size = 1) +
    geom_point(data = merged_total, aes(x = period_future, y = DOC_Conc_future/DOC_Conc_retro), show.legend = F, size = 3,
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    theme_classic() +
    ylim(c(0.85,1.5)) +
    theme(axis.text = element_text(size=16),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = c(.15,.85),
          legend.text = element_text(size = 12),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank()) +
    scale_color_manual(name = 'period_future',
                       values = c('2050s' = t_col(fig_config$period$`2050s`, 0),
                                  '2080s' = t_col(fig_config$period$`2080s`, 0)),
                       labels = c('2050\'s', '2080\'s')) +
    scale_fill_manual(name = 'period_future',
                      values = c('2050s' = t_col(fig_config$period$`2050s`, 80),
                                 '2080s' = t_col(fig_config$period$`2080s`, 80)),
                      labels = c('2050\'s', '2080\'s')) +
    scale_size_manual(name = 'period_future',
                      values = c('2050s' = 2,
                                 '2080s' = 2),
                      labels = c('2050\'s', '2080\'s')) +
    scale_shape_manual(name = 'period_future',
                       values = c('2050s' = 16,
                                  '2080s' = 16),
                       labels = c('2050\'s', '2080\'s'))

  box_d = ggplot(dplyr::filter(box_plot_data, var == 'ratio_d_epi'), aes(x = period, y = ratio, fill = period)) +
    geom_boxplot(outlier.shape = NA, show.legend = F, size = 1) +
    scale_y_continuous(limits = c(0.95, 1.2)) +
    theme_boxplot +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`),
                      labels = c('2050s', '2080s')) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank())

  box_dic_v_resp = ggplot(dplyr::filter(box_plot_data, var == 'ratio_dic_v_resp'), aes(x = period, y = ratio, fill = period)) +
    geom_boxplot(outlier.shape = NA, show.legend = F, size = 1) +
    scale_y_continuous(limits = c(0.8, 1.2)) +
    theme_boxplot +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`),
                      labels = c('2050s', '2080s')) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank())

  box_emit = ggplot(dplyr::filter(box_plot_data, var == 'ratio_emit'), aes(x = period, y = ratio, fill = period)) +
    geom_boxplot(outlier.shape = NA, show.legend = F, size = 1) +
    scale_y_continuous(limits = c(0.8, 1.3)) +
    theme_boxplot +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`),
                      labels = c('2050s', '2080s')) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank())

  box_bury = ggplot(dplyr::filter(box_plot_data, var == 'ratio_bury'), aes(x = period, y = ratio, fill = period)) +
    geom_boxplot(outlier.shape = NA, show.legend = F, size = 1) +
    scale_y_continuous(limits = c(0.85, 1.1)) +
    theme_boxplot +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`),
                      labels = c('2050s', '2080s')) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank())

  box_frac_ret = ggplot(dplyr::filter(box_plot_data, var == 'ratio_frac_ret'), aes(x = period, y = ratio, fill = period)) +
    geom_boxplot(outlier.shape = NA, show.legend = F, size = 1) +
    scale_y_continuous(limits = c(0.9, 1.3)) +
    theme_boxplot +
    scale_fill_manual(name = 'period',
                      values = c('2050s' = fig_config$period$`2050s`,
                                 '2080s' = fig_config$period$`2080s`),
                      labels = c('2050s', '2080s')) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    theme(axis.title = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank())

  g = ggdraw() +
    draw_plot(month_emit, x = 0, y = .66, width = .3, height = .3) +
    draw_plot(emit_ratio, x= .3, y= .7, width = .2, height = .2) +
    draw_plot(month_bury, x = .5, y = .66, width = .3, height = .3) +
    draw_plot(bury_ratio, x= .8, y= .7, width = .2, height = .2) +
    draw_plot(month_dic_v_resp, x = 0, y = .33, width = .3, height = .3) +
    draw_plot(dic_v_resp_ratio, x= .3, y= .37, width = .2, height = .2) +
    draw_plot(month_frac_ret, x = .5, y = .33, width = .3, height = .3) +
    draw_plot(frac_ret_ratio, x= .8, y= .37, width = .2, height = .2) +
    draw_plot(month_d_epi, x = 0, y = 0, width = .3, height = .3) +
    draw_plot(d_ratio, x= .3, y= 0.04, width = .2, height = .2) +
    draw_plot(month_doc, x = .5, y = 0, width = .3, height = .3) +
    draw_plot(doc_ratio, x= .8, y= 0.04, width = .2, height = .2)

  # emit
  # bury
  # dic load to produced
  # fraction C minearlized
  #

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 14, height = 12)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
