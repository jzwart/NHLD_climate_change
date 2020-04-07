fig_hydro_map <- function(fig_ind,
                          transparent,
                          remake_file,
                          fig_cfg_yml,
                          basemap,
                          gd_config){

  fig_config <- yaml::yaml.load_file(fig_cfg_yml) # colors for figs

  panel_label_size = 28

  g = ggdraw() +
    draw_plot(basemap, x = 0, y = 0, width = 1, height = 1)# +
    #draw_label('j', x = 0, y = .41, size = panel_label_size) # panel for map

  #g

  fig_file = as_data_file(fig_ind)
  ggsave(fig_file, plot=g, width = 12, height = 12)
  gd_put(remote_ind = fig_ind, local_source = fig_file, config_file = gd_config)
}
