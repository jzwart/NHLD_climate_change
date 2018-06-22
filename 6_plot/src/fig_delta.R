fig_delta <- function(fig_ind, var_cfg_file, fig_cfg_yml, var_lookup_yml, remake_file, gd_config){
  # read in data and make delta figure; color 2050s and 2080s
  #  make average

  var_cfg <- yaml::yaml.load_file(var_cfg_file) # indicates which periods, seasons, and variable we want returned

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  var_lookup <- yaml::yaml.load_file(var_lookup_yml) # contains fig labels and units

  var <- var_cfg$variable

  var_ind_file <- paste('2_analysis/out/', var, '.rds.ind', sep = '')

  var_results <- readRDS(sc_retrieve(var_ind_file, remake_file = remake_file))

  y <- noquote(var_lookup$ratio_var[var][[1]]) # what we want to plot

  ratio <- var_results %>%
    select(Permanent_, period, season, starts_with('ratio')) %>%
    dplyr::filter(var_results[[y]] < 6) # removing crazy outliers

  ylab <- paste('Future ', var_lookup$labs[var], '/ Historic ', var_lookup$labs[var], sep='')

  # how to graphically show ratio change??
  g <- ggplot(ratio, aes(x = period, y = ratio[[y]], fill = season)) +
    geom_boxplot() +
    theme_boxplot +
    scale_fill_manual(name = 'season',
                      values = c('all' = fig_config$season$all,
                                 'med' = fig_config$season$open_water),
                      labels = c('Annual', 'Open Water')) +
    geom_hline(yintercept = 1, linetype = 'dashed', size = 1) +
    labs(y = ylab, x = '')

  # save and post to Drive
  fig_file <- as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width=7, height=7)
  gd_put(remote_ind=fig_ind, local_source=fig_file, config_file=gd_config)
}
