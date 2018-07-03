gather_forecasts <- function(ind_file, task_df, task_plan, load_model, gd_config) {

  # read in all model output files to gather
  model_outputs <- lapply(task_plan, function(task) {
    output_rds <- task$steps[[sprintf('forecast_%s', load_model)]]$target_name
    output <- tryCatch({
      readRDS(output_rds)$preds %>%
        mutate(task_name=task$task_name) %>%
        left_join(task_df, by='task_name')
    }, error=function(e) {
      message(sprintf('could not read or munge %s: %s', output_rds, e$message))
      NULL
    })
    return(output)
  }) %>%
    bind_rows()

  # save and post the output, returning an indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(model_outputs, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
