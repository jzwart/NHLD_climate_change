# general function for pulling out variables of interest and seeing change in response to scenarios 

change_in_var <- function(ind_file, raw_ind_file, remake_file, var_cfg_file, gd_config){
  
  all_results <- readRDS(sc_retrieve(raw_ind_file, remake_file = remake_file))
  
  var_cfg_file <- yaml::yaml.load_file(var_cfg_file)
  
  results <- all_results[grep(period,names(all_results))] %>% 
    bind_rows()
  
  retro <- all_results[grep('Retro', names(all_results))]
  
  for
  
}
