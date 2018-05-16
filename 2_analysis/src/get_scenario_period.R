# getting all summary data from 

get_scenario_period <- function(){
  summary_all <- c()
  summary_sum <- c()
  for(i in 7:12){
    cur <- eval(parse(text=paste(scenario_lookup$scenario[i],'all',sep = '_')))
    cursum <- eval(parse(text=paste(scenario_lookup$scenario[i],'sum',sep = '_')))
    
    summary_all_2080s <- rbind(summary_all_2080s,cur)
    summary_sum_2080s <- rbind(summary_sum_2080s,cursum)
  }
}
