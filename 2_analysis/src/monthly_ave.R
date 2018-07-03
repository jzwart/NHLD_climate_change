monthly_ave <- function(){
  all_results <- readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))

  var_cfg <- yaml::yaml.load_file(var_cfg_file) # indicates which periods, seasons, and variable we want returned



}
