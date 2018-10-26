fig_month_box <- function(fig_ind, vars_ind_file, vars_yml, fig_cfg_yml, scenarios, var_lookup_yml, remake_file, gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  var_lookup <- yaml::yaml.load_file(var_lookup_yml) # contains fig labels and units

  vars <- noquote(yaml::yaml.load_file(vars_yml)$var) # indicates which periods, seasons, and variable we want returned

  month <- readRDS(sc_retrieve(vars_ind_file, remake_file = remake_file))

  sum_month <- month %>%
    select(Permanent_, gcm, period, month, eval(vars)) %>%
    gather(key = 'var', value = 'month_mean', eval(vars)) %>% # turning into long format for plotting
    group_by(month, period, gcm, var) %>%
    summarise(sum_all = sum(month_mean)) %>%
    ungroup() %>%
    mutate(month = as.Date(paste('2001',month,'01',sep='-'))) %>% # creating date so it can plot on x-axis
    group_by(period, gcm, month, var) %>%
    arrange(month) %>%
    mutate(cumulative_sum = cumsum(sum_all)) %>%
    ungroup()


  sum_month_emit = ggplot(dplyr::filter(sum_month, var == 'emit'), aes(x = month, y = sum_all/10^9, group = interaction(period, gcm))) +
    geom_line() +
    theme_classic() +
    ylab(bquote(Emissions~(mol~C))) +
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


  sum_month_emit


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
