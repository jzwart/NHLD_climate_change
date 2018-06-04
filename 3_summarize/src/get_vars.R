get_vars <- function(ind_file, remake_file, vars_loc, gd_config){

  vars_ind = list.files(vars_loc) %>%
    data_frame() %>%
    dplyr::filter(grepl('.ind',.)) %>%
    do(with(.,{
      lapply(., function(var_ind){
         out <- readRDS(sc_retrieve(var_ind, remake_file = remake_file))
      }) %>% bind_rows()
    }))


}
