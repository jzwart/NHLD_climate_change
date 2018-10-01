# checks for all lakes in each scenario

scenarios_qa <- function(ind_file, scenarios_ind_file, var_lims_file, retro_ind_file, retro_paper_data, remake_file, gd_config){
  # check to see if all lakes are in each scenario
  out = data_frame()

  var_lims <- yaml::yaml.load_file(var_lims_file) # variable limits for QA / QC

  # lakes in retro paper
  retro_paper_lakes <- readRDS(retro_paper_data)

  scenarios_agg <- readRDS(sc_retrieve(scenarios_ind_file, remake_file = remake_file))

  retro_agg <- readRDS(sc_retrieve(retro_ind_file, remake_file = remake_file))

  scenarios_agg[[25]] <- retro_agg[[1]]
  scenarios_agg[[26]] <- retro_agg[[2]]
  names(scenarios_agg)[c(25:26)] <- c('Retro_all', 'Retro_sum')

  dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Condor_Results/'
  dir_lookup <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/'

  scenarios <- list.files(dir)
  scenario_lookup <- read.csv(file.path(dir_lookup,'scenarios.csv'),
                              stringsAsFactors = F) #

  lakes <- c()
  for(i in 1:length(scenario_lookup$scenario)){
    cur <- scenarios_agg[[i]]
    lakes <- c(lakes, cur$Permanent_)
  }
  lakes <- lakes[!duplicated(lakes)] # all lakes among sceario runs

  for(i in 1:length(scenario_lookup$scenario)){ # lakes common to every scenario run
    cur <- scenarios_agg[[i]]
    lakes <- lakes[lakes%in%cur$Permanent_]
  }

  scenario_lookup<-rbind(scenario_lookup,c(12,'Retro'))
  for(i in 1:length(scenario_lookup$scenario)){ # keeping only lakes common to every scenario run

    cur <- scenarios_agg[[which(names(scenarios_agg)==paste0(scenario_lookup$scenario[i],'_all'))]]
    curSum <- scenarios_agg[[which(names(scenarios_agg)==paste0(scenario_lookup$scenario[i],'_all'))]]

    cur <- cur[cur$Permanent_%in%lakes,]
    curSum <- curSum[curSum$Permanent_%in%lakes,]
    cur <- cur[sort.list(cur$Permanent_),]
    curSum  <- curSum[sort.list(curSum$Permanent_),]

    cur$alk <- ifelse(cur$GWin+cur$Baseflow>0,10^(3.2815*(1-exp(-1.2765*log10(cur$GWin+cur$Baseflow+cur$SWin)/3.2815))),0)
    curSum$alk <- cur$alk
    cur$HRT <- cur$Vol/(cur$GWin + cur$SWin + cur$DirectP + cur$Baseflow + cur$IceMelt)
    cur$HRT_woEvap <- cur$Vol / (cur$GWout + cur$SWout)
    curSum$HRT <- curSum$Vol / (curSum$GWin + curSum$SWin + curSum$DirectP + curSum$Baseflow + curSum$IceMelt)
    curSum$HRT_woEvap <- curSum$Vol / (curSum$GWout + curSum$SWout)
    cur$FracRet <- 1 - (cur$DOC_export / cur$DOC_Load)
    curSum$FracRet <- 1 - (curSum$DOC_export / curSum$DOC_Load)
    cur$DIC_load <- cur$GWin * 0.7025 + cur$SWin * 0.9041667 + cur$Baseflow * 0.9041667 + cur$DirectP * 0.0833333 + cur$IceMelt * 0.01360082 # mol C day-1
    curSum$DIC_load <- curSum$GWin * 0.7025 + curSum$SWin * 0.9041667 + curSum$Baseflow * 0.9041667 + curSum$DirectP * 0.0833333 + curSum$IceMelt * 0.01360082 # mol C day-1
    cur$sed_resp <- cur$Sed_phyto * 0.75 + cur$Sed_tPOC * 0.1 # 75% of phyto C and 10% of tpoc C was converted to co2
    curSum$sed_resp <- curSum$Sed_phyto * 0.75 + curSum$Sed_tPOC * 0.1 # 75% of phyto C and 10% of tpoc C was converted to co2
    cur$waterIn <- cur$GWin + cur$SWin + cur$DirectP + cur$Baseflow + cur$IceMelt
    curSum$waterIn <- curSum$GWin + curSum$SWin + curSum$DirectP + curSum$Baseflow + curSum$IceMelt
    cur$fluvialOut <- cur$GWout + cur$SWout # m3 day-1
    curSum$fluvialOut <- curSum$GWout + curSum$SWout
    cur$dicLoadvResp <- cur$DIC_load / (cur$DOC_Respired + cur$sed_resp)
    curSum$dicLoadvResp <- curSum$DIC_load / (curSum$DOC_Respired + curSum$sed_resp)
    cur$percentEvap <- cur$LakeE / (cur$LakeE + cur$GWout + cur$SWout)
    curSum$percentEvap <- curSum$LakeE / (curSum$LakeE + curSum$GWout + curSum$SWout)
    cur$Burial_total <- cur$Burial_phyto + cur$Burial_tPOC
    curSum$Burial_total <- curSum$Burial_phyto + curSum$Burial_tPOC
    cur$pco2 <- 0.952 * cur$fracCO2 * cur$DIC_epi / cur$Vepi * 1000 * 29.41
    curSum$pco2 <- 0.952 * curSum$fracCO2 * curSum$DIC_epi / curSum$Vepi * 1000 * 29.41
    cur$doc_conc <- (cur$DOCr_epi+cur$DOCl_epi+cur$DOCr_hypo+cur$DOCl_hypo)/cur$Vol*12
    curSum$doc_conc <- (curSum$DOCr_epi+curSum$DOCl_epi+curSum$DOCr_hypo+curSum$DOCl_hypo)/curSum$Vol*12
    cur$tp_load <- cur$GWin * 0.0007742 + cur$SWin * 0.001612903 +
      cur$Baseflow * 0.001612903 + cur$DirectP * 0.0003225806 + cur$IceMelt * 0.0003225806 # mol P day-1
    curSum$tp_load <- curSum$GWin * 0.0007742 + curSum$SWin * 0.001612903 +
      curSum$Baseflow * 0.001612903 + curSum$DirectP * 0.0003225806 + curSum$IceMelt * 0.0003225806 # mol P day-1

    lakeSizeBins <- c(0,0.01,.1,1,10,100)*1e6 # breaks for max cutoff of lake size from Downing et al. 2006
    cur$lakeSizeBins <- cut(cur$Area, breaks = lakeSizeBins)
    curSum$lakeSizeBins <- cut(curSum$Area, breaks = lakeSizeBins)
    cur$period <- ifelse(scenario_lookup$scenario[i] == 'Retro',
                         'Retro',
                         ifelse(length(grep('2080s', scenario_lookup$scenario[i]))>0, '2080s', '2050s'))
    curSum$period <- ifelse(scenario_lookup$scenario[i] == 'Retro',
                         'Retro',
                         ifelse(length(grep('2080s', scenario_lookup$scenario[i]))>0, '2080s', '2050s'))
    cur$season <- 'all'
    curSum$season <- 'open_water'
    if(i <= 12){
      cur$gcm <- strsplit(scenario_lookup$scenario[i], '_2')[[1]][1]
      curSum$gcm <- strsplit(scenario_lookup$scenario[i], '_2')[[1]][1]
    }else {
      cur$gcm <- 'Retro'
      curSum$gcm <- 'Retro'
    }

    # out[[q]] <- cur
    # out[[q+1]] <- curSum
    #
    # out_name_all = paste(scenario_lookup$scenario[i], 'all', sep = '_')
    # out_name_sum = paste(scenario_lookup$scenario[i], 'sum', sep = '_')
    #
    # names(out)[q:(q+1)] <- c(out_name_all, out_name_sum)

    out <- bind_rows(out, cur, curSum)
  }

  # QA/QC for variable limits ; lakes to omit
  lakes_omit <- out %>%
    dplyr::filter(out$doc_conc<var_lims$doc$min | out$doc_conc>var_lims$doc$max,
                  out$kD<var_lims$kd$min | out$kD>var_lims$kd$max) %>%
    select(Permanent_) %>%
    unique()

  out <- out %>%
    dplyr::filter(!out$Permanent_ %in% lakes_omit$Permanent_,
                  out$Permanent_ %in% retro_paper_lakes$Permanent_)


  data_file = as_data_file(ind_file)
  saveRDS(out, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}

