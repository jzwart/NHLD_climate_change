conc_sens_summary <- function(ind_file, var_lims_file, gd_config){
  list_out <- list()

  dir <- 'D:/results_6/' # we only used middle of road, 2080s scenario ()

  runs <- list.files(dir)
  conc_scenario_lookup <- read.csv('4_conc_sens/in/conc_scenarios.csv', stringsAsFactors = F) #
  lake_lookup <- read.csv('4_conc_sens/in/allruns_6.csv', stringsAsFactors = F)

  skip=6*365 # days to skip; first 6 years (spin up)

  threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes
  all <- data.frame()
  for(i in 1:length(runs)){
    print(i)

    cur_lake_id <- strsplit(runs[i],'_')[[1]][1]
    cur_conc_scen_id <- strsplit(runs[i],'_')[[1]][2]
    cur_lake_perm_id <- lake_lookup$currID[lake_lookup$crun==cur_lake_id]
    cur_conc_scenario <- conc_scenario_lookup[conc_scenario_lookup$conc_id == cur_conc_scen_id,]

    cur <- readRDS(file.path(dir,runs[i]))

    cur<-na.omit(cur)
    if(length(cur$time)<2){
      next
    }
    cur_lake_perm_id <- toupper(cur_lake_perm_id) # making all uppercase; important for JAZ and ZJH lakes

    cur<-cur[skip:(nrow(cur)-0),3:ncol(cur)] # skipping first X number of days
    cur<-cur[cur$Vol>0,] # only keeping days when there's actually water
    ndays_ice <- nrow(cur[cur$LakeE==0,])

    # removing outliers based on % of original volume
    vol_frac <- cur$Vol/cur$Vol[1]
    cur <- cur[vol_frac>threshold,]

    cur<-data.frame(t(apply(cur,MARGIN = 2,FUN = mean)))
    cur$Permanent_<- cur_lake_perm_id
    cur$ndays_open_ice <- ndays_open_ice
    cur$ndays_ice <- ndays_ice

    cur$alk <- ifelse(cur$GWin+cur$Baseflow>0,10^(3.2815*(1-exp(-1.2765*log10(cur$GWin+cur$Baseflow+cur$SWin)/3.2815))),0)
    cur$HRT <- cur$Vol/(cur$GWin + cur$SWin + cur$DirectP + cur$Baseflow + cur$IceMelt)
    cur$HRT_woEvap <- cur$Vol / (cur$GWout + cur$SWout)
    cur$FracRet <- 1 - (cur$DOC_export / cur$DOC_Load)
    curSum$FracRet <- 1 - (curSum$DOC_export / curSum$DOC_Load)
    cur$DIC_load <- cur$GWin * 0.7025 * cur_conc_scenario$dic_change + cur$SWin * 0.9041667 * cur_conc_scenario$dic_change + cur$Baseflow * 0.9041667 * cur_conc_scenario$dic_change + cur$DirectP * 0.0833333 + cur$IceMelt * 0.01360082 # mol C day-1
    cur$sed_resp <- cur$Sed_phyto * 0.75 + cur$Sed_tPOC * 0.1 # 75% of phyto C and 10% of tpoc C was converted to co2
    cur$waterIn <- cur$GWin + cur$SWin + cur$DirectP + cur$Baseflow + cur$IceMelt
    cur$fluvialOut <- cur$GWout + cur$SWout # m3 day-1
    cur$dicLoadvResp <- cur$DIC_load / (cur$DOC_Respired + cur$sed_resp)
    cur$percentEvap <- cur$LakeE / (cur$LakeE + cur$GWout + cur$SWout)
    cur$Burial_total <- cur$Burial_phyto + cur$Burial_tPOC
    cur$pco2 <- 0.952 * cur$fracCO2 * cur$DIC_epi / cur$Vepi * 1000 * 29.41
    cur$doc_conc <- (cur$DOCr_epi+cur$DOCl_epi+cur$DOCr_hypo+cur$DOCl_hypo)/cur$Vol*12
    cur$tp_load <- cur$GWin * 0.0007742 * cur_conc_scenario$p_change + cur$SWin * 0.001612903 * cur_conc_scenario$p_change +
      cur$Baseflow * 0.001612903 * cur_conc_scenario$p_change + cur$DirectP * 0.0003225806 + cur$IceMelt * 0.0003225806 # mol P day-1

    lakeSizeBins <- c(0,0.01,.1,1,10,100)*1e6 # breaks for max cutoff of lake size from Downing et al. 2006
    cur$lakeSizeBins <- cut(cur$Area, breaks = lakeSizeBins)
    cur$doc_change <- cur_conc_scenario$doc_change
    cur$dic_change <- cur_conc_scenario$dic_change
    cur$p_change <- cur_conc_scenario$p_change

    cur$period <- '2080s'
    cur$season <- 'all'
    cur$gcm <- 'CESM1_CAM5'

    all<-rbind(all,cur)
  }
  out <- all
  rm(all)

  # QA/QC for variable limits ; lakes to omit
  lakes_omit <- out %>%
    dplyr::filter(out$doc_conc<var_lims$doc$min | out$doc_conc>var_lims$doc$max,
                  out$kD<var_lims$kd$min | out$kD>var_lims$kd$max) %>%
    select(Permanent_) %>%
    unique()

  out <- out %>%
    dplyr::filter(!out$Permanent_ %in% lakes_omit$Permanent_)

  data_file = as_data_file(ind_file)
  saveRDS(out, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
