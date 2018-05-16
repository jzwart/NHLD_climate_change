# analysis of condor output; JAZ 2018-01-10 

dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Condor_Results/'
dir_lookup <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/'

scenarios <- list.files(dir)
scenario_lookup <- read.csv('D:/MyPapers/NHLD Climate Change/Results/C_model_output/scenarios.csv',stringsAsFactors = F)

skip=6*365 # days to skip; first 6 years (spin up)

watersheds<-read.table('/Users/jzwart/Documents/Jake/MyPapers/Regional Lake Carbon Model - ECI/Data/C model forcing data/NHLDsheds_20170323.txt',
                       stringsAsFactors = F,header=T,sep = '\t')

threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes 
for(i in 1:length(scenarios)){
  print(i)
  
  cur_scenario_id <- strsplit(scenarios[i],'_')[[1]][2]
  cur_scenario <- scenario_lookup$scenario[scenario_lookup$job==cur_scenario_id]
  
  condor_lookup <- read.csv(file.path(dir_lookup,paste('allruns_',cur_scenario_id,'.csv',sep='')),stringsAsFactors = F)
  
  sub_dir <- file.path(dir,scenarios[i])
  
  files <- list.files(sub_dir)
  if(length(files[grep('adj',files)])>0){
    files <- files[-grep('adj',files)]
  }
  
  sum <- data.frame() # open ice only
  all <- data.frame() 
  for(j in 1:length(files)){ # length(files
    print(c(i,j))

    cur <- readRDS(file.path(sub_dir,files[j]))
    lake<-strsplit(tolower(files[j]),'_c_model.rds',fixed = T)[[1]]
    if(scenarios[i]!='Present'){ # all scenarios other than Present were run on Condor and need Condor lookup table 
      lake <- condor_lookup$currID[condor_lookup$crun==as.numeric(lake)]
    }
    cur<-na.omit(cur)
    if(length(cur$time)<2){
      next
    }
    lake <- toupper(lake) # making all uppercase; important for JAZ and ZJH lakes 
    
    cur<-cur[skip:(nrow(cur)-0),3:ncol(cur)] # skipping first X number of days
    cur<-cur[cur$Vol>0,] # only keeping days when there's actually water
    curSum=cur[cur$LakeE>0,] # only open ice periods
    ndays_open_ice <- nrow(curSum)
    ndays_ice <- nrow(cur[cur$LakeE==0,])
    
    # removing outliers based on % of original volume 
    vol_frac <- cur$Vol/cur$Vol[1]
    cur <- cur[vol_frac>threshold,]
    
    cur<-data.frame(t(apply(cur,MARGIN = 2,FUN = mean)))
    curSum<-data.frame(t(apply(curSum,MARGIN = 2,FUN = mean)))
    cur$Permanent_<-lake
    curSum$Permanent_<-lake
    curSum$ndays_open_ice <- ndays_open_ice
    curSum$ndays_ice <- ndays_ice
    cur$ndays_open_ice <- ndays_open_ice
    cur$ndays_ice <- ndays_ice
    
    sum<-rbind(sum,curSum)
    all<-rbind(all,cur)
  }
  sum <- merge(sum, watersheds, by='Permanent_')
  all <- merge(all, watersheds, by='Permanent_')
  
  assign(paste(cur_scenario,'all',sep = '_'),value = all)
  assign(paste(cur_scenario,'sum',sep = '_'),value = sum)
}

# for present day results 
dir <- 'D:/MyPapers/NHLD Climate Change/Results/C_model_output/Present/20170819/'

skip=6*365 # days to skip; first 6 years (spin up)

watersheds<-read.table('/Users/jzwart/Documents/Jake/MyPapers/Regional Lake Carbon Model - ECI/Data/C model forcing data/NHLDsheds_20170323.txt',
                       stringsAsFactors = F,header=T,sep = '\t')

threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes 
  
  cur_scenario <- 'Present'
  
  files <- list.files(dir)
  if(length(files[grep('adj',files)])>0){
    files <- files[-grep('adj',files)]
  }
  
  sum <- data.frame() # open ice only
  all <- data.frame() 
  for(j in 1:length(files)){ # length(files
    print(c(i,j))
    
    cur <- readRDS(file.path(dir,files[j]))
    lake<-strsplit(tolower(files[j]),'_c_model.rds',fixed = T)[[1]]
    
    cur<-na.omit(cur)
    if(length(cur$time)<2){
      next
    }
    lake <- toupper(lake) # making all uppercase; important for JAZ and ZJH lakes 
    
    cur<-cur[skip:(nrow(cur)-0),3:ncol(cur)] # skipping first X number of days
    cur<-cur[cur$Vol>0,] # only keeping days when there's actually water
    curSum=cur[cur$LakeE>0,] # only open ice periods
    ndays_open_ice <- nrow(curSum)
    ndays_ice <- nrow(cur[cur$LakeE==0,])
    
    # removing outliers based on % of original volume 
    vol_frac <- cur$Vol/cur$Vol[1]
    cur <- cur[vol_frac>threshold,]
    
    cur<-data.frame(t(apply(cur,MARGIN = 2,FUN = mean)))
    curSum<-data.frame(t(apply(curSum,MARGIN = 2,FUN = mean)))
    cur$Permanent_<-lake
    curSum$Permanent_<-lake
    curSum$ndays_open_ice <- ndays_open_ice
    curSum$ndays_ice <- ndays_ice
    cur$ndays_open_ice <- ndays_open_ice
    cur$ndays_ice <- ndays_ice
    
    sum<-rbind(sum,curSum)
    all<-rbind(all,cur)
  }
  sum <- merge(sum, watersheds, by='Permanent_')
  all <- merge(all, watersheds, by='Permanent_')
  
assign(paste(cur_scenario,'all',sep = '_'),value = all)
assign(paste(cur_scenario,'sum',sep = '_'),value = sum)


###########################
load('C:/Users/jzwart/NHLD_climate_change/R Data/c_model_summary.RData')
############
library(LSD)
library(maptools)
library(Hmisc)

# check to see if all lakes are in each scenario
lakes <- c()
for(i in 1:length(scenario_lookup$scenario)){
  cur <- eval(parse(text=paste(scenario_lookup$scenario[i],'all',sep = '_')))
  lakes <- c(lakes, cur$Permanent_)
}
lakes <- lakes[!duplicated(lakes)] # all lakes among sceario runs 

for(i in 1:length(scenario_lookup$scenario)){ # lakes common to every scenario run 
  cur <- eval(parse(text=paste(scenario_lookup$scenario[i],'all',sep = '_')))
  lakes <- lakes[lakes%in%cur$Permanent_]
}
scenario_lookup<-rbind(scenario_lookup,c(12,'Present'))
for(i in 1:length(scenario_lookup$scenario)){ # keeping only lakes common to every scenario run 
  cur <- eval(parse(text=paste(scenario_lookup$scenario[i],'all',sep = '_')))
  curSum <- eval(parse(text=paste(scenario_lookup$scenario[i],'sum',sep = '_')))
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
  
  lakeSizeBins <- c(0,0.01,.1,1,10,100)*1e6 # breaks for max cutoff of lake size from Downing et al. 2006
  cur$lakeSizeBins <- cut(cur$Area, breaks = lakeSizeBins)
  curSum$lakeSizeBins <- cut(curSum$Area, breaks = lakeSizeBins)
  
  assign(paste(scenario_lookup$scenario[i],'all',sep = '_'),value = cur)
  assign(paste(scenario_lookup$scenario[i],'sum',sep = '_'),value = curSum)
}


summary((GFDL_CM3_2080s_sum$DOCr_epi+GFDL_CM3_2080s_sum$DOCl_epi)/GFDL_CM3_2080s_sum$Vepi*12)

boxplot((Present_sum$DOCr_epi+Present_sum$DOCl_epi)/Present_sum$Vepi*12,outline = F)
boxplot((GFDL_CM3_2080s_sum$DOCr_epi+GFDL_CM3_2080s_sum$DOCl_epi)/GFDL_CM3_2080s_sum$Vepi*12,outline = F)

# Crystal 
ntlLookUp<-data.frame(lakeID=c('AL','BM','CR','SP','TR','TB','CB'),Permanent_=c(69886156,69886284,69886510,69886444,69886228,69886158,123148117))
cr <- HadGEM2_AO_2080s_sum[HadGEM2_AO_2080s_sum$Permanent_==69886510,]
cr_pres <- Present_sum[Present_sum$Permanent_==69886510,]

(cr$DOCr_epi+cr$DOCl_epi)/cr$Vepi*12
(cr_pres$DOCr_epi+cr_pres$DOCl_epi)/cr_pres$Vepi*12


summary_all_2080s <- c()
summary_sum_2080s <- c()
for(i in 7:12){
  cur <- eval(parse(text=paste(scenario_lookup$scenario[i],'all',sep = '_')))
  cursum <- eval(parse(text=paste(scenario_lookup$scenario[i],'sum',sep = '_')))
  
  summary_all_2080s <- rbind(summary_all_2080s,cur)
  summary_sum_2080s <- rbind(summary_sum_2080s,cursum)
}

doc <- aggregate(summary_sum_2080s$doc_conc~summary_sum_2080s$Permanent_,FUN = mean)
doc_sd <- aggregate(summary_sum_2080s$doc_conc~summary_sum_2080s$Permanent_,FUN = sd)
colnames(doc) <- c('Permanent_','doc_conc_future')
colnames(doc_sd) <- c('Permanent_','doc_conc_future_sd')
stage <- aggregate(summary_sum_2080s$Stage~summary_sum_2080s$Permanent_,FUN = mean)
stage_sd <- aggregate(summary_sum_2080s$Stage~summary_sum_2080s$Permanent_,FUN = sd)
colnames(stage) <- c('Permanent_','stage_future')
colnames(stage_sd) <- c('Permanent_','stage_future_sd')

doc <- merge(doc,doc_sd,by = 'Permanent_')
doc <- merge(doc,Present_sum[,c('Permanent_','percentEvap','doc_conc','Stage')])
doc <- merge(doc,stage,by='Permanent_')
doc <- merge(doc,stage_sd,by='Permanent_')

plot(doc$doc_conc_future~doc$percentEvap,ylim=c(0,100))
plot(doc$doc_conc_future_sd/doc$doc_conc_future~doc$percentEvap,ylim=c(0,1))
plot(doc$doc_conc_future/doc$doc_conc,ylim=c(0.5,2.2))
plot(doc$doc_conc_future/doc$doc_conc~doc$percentEvap,ylim=c(0.5,2.2))

boxplot(doc[,c('doc_conc_future','doc_conc')],outline = F)
temp <- doc[doc$doc_conc<100&doc$doc_conc_future<100,]
hist(temp$doc_conc,xlim=c(0,50),freq = T,breaks = seq(0,100,by = .5),col='grey')
hist(temp$doc_conc_future,add=T,xlim=c(0,70),freq = T,breaks = seq(0,100,by = .5),col=rgb(1,0,0,alpha = .3))


windows()
par(mar=c(6,6,5,2))
cex.axis=2.5
cex.lab=2.5
cex=2
col=colorpalette(c('grey70','black'))
lim=c(0,60)

plot(doc$doc_conc_future~doc$doc_conc,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,ylim=lim,xlim=lim,
     xlab='Historic DOC',ylab='2080s DOC')
arrows(x0 = doc$doc_conc,y0 = doc$doc_conc_future,x1 = doc$doc_conc,y1 = (doc$doc_conc_future+doc$doc_conc_future_sd),length = 0.05,angle = 90,code = 3,
       col='grey70')
arrows(x0 = doc$doc_conc,y0 = doc$doc_conc_future,x1 = doc$doc_conc,y1 = (doc$doc_conc_future-doc$doc_conc_future_sd),length = 0.05,angle = 90,code = 3,
       col='grey70')
points(doc$doc_conc_future~doc$doc_conc,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,ylim=lim,xlim=lim,
     xlab='Historic DOC',ylab='2080s DOC')
abline(0,1,lty=2,lwd=4,col='red')

## 
windows()
par(mar=c(6,6,5,2))
cex.axis=2.5
cex.lab=2.5
cex=2
col=colorpalette(c('grey70','black'))
lim=c(0.5,2.2)
plot(doc$doc_conc_future/doc$doc_conc~doc$percentEvap,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,ylim=lim,xlim=c(0,1),
     xlab='Historic Fraction Evap',ylab='2080s DOC / Histroic DOC')
abline(1,0,lty=2,lwd=4,col='red')

windows()
par(mar=c(6,6,5,2))
cex.axis=2.5
cex.lab=2.5
cex=2
col=colorpalette(c('grey70','black'))
lim=c(0,.6)
plot(doc$doc_conc_future_sd/doc$doc_conc_future~doc$percentEvap,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,ylim=lim,xlim=c(0,1),
     xlab='Historic Fraction Evap',ylab='CV Future DOC')


windows()
par(mar=c(6,6,5,2))
cex.axis=2.5
cex.lab=2.5
cex=2
col=colorpalette(c('grey70','black'))
ylim=c(0.5,2.2)
xlim=c(.6,1.05)
doc$delta_stage <- doc$stage_future/doc$Stage
plot(doc$doc_conc_future/doc$doc_conc~doc$delta_stage,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,xlim=xlim,ylim=ylim,
     xlab='2080s Stage / Historic Stage',ylab='2080s DOC / Historic DOC')
abline(1,0,lwd=4,lty=2,col='red')
abline(v = 1,lwd=4,lty=2,col='red')

windows()
par(mar=c(6,6,5,2))
cex.axis=2.5
cex.lab=2.5
cex=2
col=colorpalette(c('grey70','black'))
lim=c(.6,1.1)
plot(doc$stage_future-doc$Stage~doc$percentEvap,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,xlim=c(0,1),
     xlab='Historic Fraction Evap',ylab='2080s Stage / Historic Stage')




pco2 <- aggregate(summary_all_2080s$pco2~summary_all_2080s$Permanent_,FUN = mean)
pco2_sd <- aggregate(summary_all_2080s$pco2~summary_all_2080s$Permanent_,FUN = sd)
colnames(pco2) <- c('Permanent_','pco2_future')
colnames(pco2_sd) <- c('Permanent_','pco2_future_sd')

pco2 <- merge(pco2,pco2_sd,by = 'Permanent_')
pco2 <- merge(pco2,Present_all[,c('Permanent_','percentEvap','pco2')])

plot(pco2$pco2_future~pco2$percentEvap,ylim=c(0,10000))
plot(pco2$pco2_future_sd/pco2$pco2_future~doc$percentEvap,ylim=c(0,1))
plot(pco2$pco2_future/pco2$pco2,ylim=c(0.5,2.2))
plot(pco2$pco2_future/pco2$pco2~pco2$percentEvap,ylim=c(0.5,2.2))

boxplot(pco2[,c('pco2_future','pco2')],outline = F)

windows()
par(mar=c(6,6,5,2))
cex.axis=2.5
cex.lab=2.5
cex=2
col=colorpalette(c('grey70','black'))
lim=c(300,14000)
plot(pco2$pco2_future~pco2$pco2,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,ylim=lim,xlim=lim,
     xlab='Historic pCO2',ylab='2080s pCO2')
arrows(x0 = pco2$pco2,y0 = pco2$pco2_future,x1 = pco2$pco2,y1 = (pco2$pco2_future+pco2$pco2_future_sd),length = 0.05,angle = 90,code = 3,
       col='grey70')
arrows(x0 = pco2$pco2,y0 = pco2$pco2_future,x1 = pco2$pco2,y1 = (pco2$pco2_future-pco2$pco2_future_sd),length = 0.05,angle = 90,code = 3,
       col='grey70')
points(pco2$pco2_future~pco2$pco2,col=rgb(0,0,0,alpha = .2),pch=19,cex=cex,main='',cex.axis=cex.axis,cex.lab=cex.lab,ylim=lim,xlim=lim,
       xlab='Historic pCO2',ylab='2080s pCO2')
abline(0,1,lty=2,lwd=4,col='red')

summary(pco2$pco2_future)
summary(pco2$pco2)

plot(pco2$pco2_future/pco2$pco2~pco2$percentEvap,ylim=c(0,2.3))


# emissions / lake area 
pco2 <- aggregate(summary_all_2080s$Emit/summary_all_2080s$Area~summary_all_2080s$Permanent_,FUN = mean)
pco2_sd <- aggregate(summary_all_2080s$Emit/summary_all_2080s$Area~summary_all_2080s$Permanent_,FUN = sd)
colnames(pco2) <- c('Permanent_','emit_future')
colnames(pco2_sd) <- c('Permanent_','emit_future_sd')

pco2 <- merge(pco2,pco2_sd,by = 'Permanent_')
pco2 <- merge(pco2,Present_all[,c('Permanent_','percentEvap','Emit','Area')])
pco2$Emit_area <- pco2$Emit/pco2$Area

summary(pco2$emit_future)
summary(pco2$Emit_area)

sum(pco2$emit_future)
sum(pco2$Emit_area)

plot(pco2$emit_future/pco2$Emit_area~pco2$percentEvap,ylim=c(0,2))

# emissions total 
pco2 <- aggregate(summary_all_2080s$Emit~summary_all_2080s$Permanent_,FUN = mean)
pco2_sd <- aggregate(summary_all_2080s$Emit~summary_all_2080s$Permanent_,FUN = sd)
colnames(pco2) <- c('Permanent_','emit_future')
colnames(pco2_sd) <- c('Permanent_','emit_future_sd')

pco2 <- merge(pco2,pco2_sd,by = 'Permanent_')
pco2 <- merge(pco2,Present_all[,c('Permanent_','percentEvap','Emit')])

summary(pco2$emit_future)
summary(pco2$Emit)

sum(pco2$emit_future)
sum(pco2$Emit)

plot(pco2$emit_future/pco2$Emit~pco2$percentEvap,ylim=c(0,2))



# SW discharge; on average less surface water in 
swin <- aggregate(summary_all_2080s$SWin~summary_all_2080s$Permanent_,FUN = mean)
colnames(swin) <- c('Permanent_','swin_future')

swin <- merge(swin,Present_all[,c('Permanent_','SWin')])

summary(swin$swin_future)
summary(swin$SWin)

swin <- aggregate(summary_all_2080s$SWin+summary_all_2080s$Baseflow~summary_all_2080s$Permanent_,FUN = mean)
colnames(swin) <- c('Permanent_','swin_future')

swin <- merge(swin,Present_all[,c('Permanent_','SWin','Baseflow')])
swin$SWin <- swin$SWin+swin$Baseflow

summary(swin$swin_future)
summary(swin$SWin)


# GW discharge; on average more groundwater in 
GWin <- aggregate(summary_all_2080s$GWin~summary_all_2080s$Permanent_,FUN = mean)
colnames(GWin) <- c('Permanent_','GWin_future')

GWin <- merge(GWin,Present_all[,c('Permanent_','GWin')])

summary(GWin$GWin_future)
summary(GWin$GWin)

GWin <- aggregate(summary_all_2080s$GWin+summary_all_2080s$Baseflow~summary_all_2080s$Permanent_,FUN = mean)
colnames(GWin) <- c('Permanent_','GWin_future')

GWin <- merge(GWin,Present_all[,c('Permanent_','GWin','percentEvap')])

summary(GWin$GWin_future)
summary(GWin$GWin)

plot(GWin$GWin_future/GWin$GWin~GWin$percentEvap,ylim=c(-1,10))
abline(1,0,lty=2,lwd=2,col='red')

boxplot(GWin[,c('GWin_future','GWin')],outline = F)


# GWout; more GWout future 
GWout <- aggregate(summary_all_2080s$GWout~summary_all_2080s$Permanent_,FUN = mean)
colnames(GWout) <- c('Permanent_','GWout_future')

GWout <- merge(GWout,Present_all[,c('Permanent_','GWout')])

summary(GWout$GWout_future)
summary(GWout$GWout)

GWout <- aggregate(summary_all_2080s$GWout+summary_all_2080s$Baseflow~summary_all_2080s$Permanent_,FUN = mean)
colnames(GWout) <- c('Permanent_','GWout_future')

GWout <- merge(GWout,Present_all[,c('Permanent_','GWout','percentEvap')])

summary(GWout$GWout_future)
summary(GWout$GWout)

plot(GWout$GWout_future/GWout$GWout~GWout$percentEvap,ylim=c(-1,10))
abline(1,0,lty=2,lwd=2,col='red')

boxplot(GWout[,c('GWout_future','GWout')],outline = F)





##########################################################################


CESM1_CAM5_all$LakeE/CESM1_CAM5_all$Area - Present_all$LakeE/Present_all$Area

(CESM1_CAM5_all$DirectP/CESM1_CAM5_all$Area - Present_all$DirectP/Present_all$Area)
(CESM1_CAM5_all$DirectP/CESM1_CAM5_all$Area) / (Present_all$DirectP/Present_all$Area)
CESM1_CAM5_all$Area/Present_all$Area
CESM1_CAM5_all$Vol/Present_all$Vol

(CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area) / (Present_all$Emit/Present_all$Area)
sum(CESM1_CAM5_all$Emit)/sum(Present_all$Emit)
sum(CESM1_CAM5_sum$Emit)/sum(Present_sum$Emit)
sum(CESM1_CAM5_all$Burial_total)/sum(Present_all$Burial_total)

hist(CESM1_CAM5_all$FracRet/Present_all$FracRet)
plot(CESM1_CAM5_all$FracRet/Present_all$FracRet)

plot(CESM1_CAM5_all$Vol/Present_all$Vol~log(Present_all$WALA))

sum(CESM1_CAM5_all$FracRet*CESM1_CAM5_all$DOC_Load)/sum(Present_all$FracRet*Present_all$DOC_Load)

CESM1_CAM5_all$emergent_d_epi/Present_all$emergent_d_epi

CESM1_CAM5_all$epiTemp/Present_all$epiTemp

CESM1_CAM5_all$HRT/Present_all$HRT

hist(CESM1_CAM5_all$HRT)
hist(Present_all$HRT,add=T)
summary(CESM1_CAM5_all$HRT)
summary(Present_all$HRT)

plot(CESM1_CAM5_all$HRT/Present_all$HRT~log(CESM1_CAM5_all$WALA)) 
plot(CESM1_CAM5_all$Baseflow/Present_all$Baseflow~log(CESM1_CAM5_all$WALA))
plot(CESM1_CAM5_all$LakeE/Present_all$LakeE~log(CESM1_CAM5_all$WALA))
plot(CESM1_CAM5_all$SWin/Present_all$SWin~log(CESM1_CAM5_all$WALA))
plot(CESM1_CAM5_all$GWin/Present_all$GWin~log(CESM1_CAM5_all$WALA))
plot(CESM1_CAM5_all$DirectP/Present_all$DirectP~log(CESM1_CAM5_all$WALA))
# direct precipitation contributes more to small WA:LA lakes so increased precip and increased evap means increased water in for small WALA and 
#    decreased water in for large WALA 
plot(CESM1_CAM5_all$percentEvap/Present_all$percentEvap~log(CESM1_CAM5_all$WALA))
plot(CESM1_CAM5_all$waterIn/Present_all$waterIn~log(CESM1_CAM5_all$WALA))

plot(((CESM1_CAM5_all$DOCr_epi+CESM1_CAM5_all$DOCr_hypo)/CESM1_CAM5_all$Vol)/((Present_all$DOCr_epi+Present_all$DOCr_hypo)/Present_all$Vol)~
       log(CESM1_CAM5_all$WALA),ylim=c(0.5,1.5)) # small WALA lakes are most variable to change 

plot((CESM1_CAM5_all$DOCr_epi/CESM1_CAM5_all$Vepi)/(Present_all$DOCr_epi/Present_all$Vepi)~CESM1_CAM5_all$percentEvap,
     ylim=c(0.5,1.5)) # small WALA lakes are most variable to change 

plot(CESM1_CAM5_all$percentEvap~Present_all$percentEvap)
abline(0,1,lty=2,lwd=2)

plot(log(CESM1_CAM5_all$Emit)~log(Present_all$Emit))
abline(0,1)

sum(CESM1_CAM5_all$Area)/sum(Present_all$Area) # lake area decreases
summary((CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area) / (Present_all$Emit/Present_all$Area)) # emissions per lake area increased 
hist((CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area) / (Present_all$Emit/Present_all$Area)) # emissions per lake area increased 

summary((CESM1_CAM5_all$DOCr_epi+CESM1_CAM5_all$DOCl_epi)/CESM1_CAM5_all$Vepi*12)
summary((Present_all$DOCr_epi+Present_all$DOCl_epi)/Present_all$Vepi*12)

summary(((CESM1_CAM5_all$DOCr_epi+CESM1_CAM5_all$DOCl_epi)/CESM1_CAM5_all$Vepi)/((Present_all$DOCr_epi+Present_all$DOCl_epi)/Present_all$Vepi))


CESM1_CAM5_all<-CESM1_CAM5_all[(CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area) / (Present_all$Emit/Present_all$Area)<5,]
Present_all <- Present_all[Present_all$Permanent_%in%CESM1_CAM5_all$Permanent_,]

sum(CESM1_CAM5_all$Area)/sum(Present_all$Area) # lake area decreases
summary((CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area) / (Present_all$Emit/Present_all$Area)) # emissions per lake area increased 
hist((CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area) / (Present_all$Emit/Present_all$Area)) # emissions per lake area increased 

sum(CESM1_CAM5_all$Emit)/sum(Present_all$Emit)

plot((CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area)/(Present_all$Emit/Present_all$Area)~CESM1_CAM5_all$percentEvap)
plot(CESM1_CAM5_all$Emit/Present_all$Emit~CESM1_CAM5_all$percentEvap)

plot(CESM1_CAM5_all$Area/Present_all$Area~CESM1_CAM5_all$percentEvap)

plot(CESM1_CAM5_all$Emit/Present_all$Emit~log(CESM1_CAM5_all$Area))
plot((CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area)/(Present_all$Emit/Present_all$Area)~log(CESM1_CAM5_all$Area))
plot(CESM1_CAM5_all$Area/Present_all$Area~log(CESM1_CAM5_all$Area))
CESM1_CAM5_all$area_change <- CESM1_CAM5_all$Area/Present_all$Area
plot((CESM1_CAM5_all$Emit/Present_all$Emit)~CESM1_CAM5_all$area_change)
plot((CESM1_CAM5_all$Emit/CESM1_CAM5_all$Area)/(Present_all$Emit/Present_all$Area)~CESM1_CAM5_all$area_change)

CESM1_CAM5_all_smallWALA<-CESM1_CAM5_all[CESM1_CAM5_all$WALA<1000,]
CESM1_CAM5_all_smallWALA<-CESM1_CAM5_all_smallWALA[!is.na(CESM1_CAM5_all_smallWALA$Emit),]
Present_all_smallWALA <- Present_all[Present_all$Permanent_%in%CESM1_CAM5_all_smallWALA$Permanent_,]

sum(CESM1_CAM5_all_smallWALA$Emit)/sum(Present_all_smallWALA$Emit)
sum(CESM1_CAM5_all_smallWALA$Burial_total)/sum(Present_all_smallWALA$Burial_total)

summary(CESM1_CAM5_all_smallWALA$pH/Present_all_smallWALA$pH)

plot(CESM1_CAM5_all_smallWALA$pH~CESM1_CAM5_all_smallWALA$Area)
plot(CESM1_CAM5_all_smallWALA$pH/Present_all_smallWALA$pH~CESM1_CAM5_all_smallWALA$percentEvap,ylim=c(0.98,1.03))

summary(CESM1_CAM5_all_smallWALA$pH)
summary(Present_all_smallWALA$pH)

plot(CESM1_CAM5_sum$pH/Present_sum$pH~CESM1_CAM5_sum$percentEvap,ylim=c(0.98,1.03))
summary(CESM1_CAM5_sum$pH)
summary(Present_sum$pH)

summary(CESM1_CAM5_sum$emergent_d_epi/Present_sum$emergent_d_epi)
summary(CESM1_CAM5_sum$emergent_d_hypo/Present_sum$emergent_d_hypo)

doc <- c()
for(i in 1:length(scenario_lookup$scenario)){
  cur <- eval(parse(text=paste(scenario_lookup$scenario[i],'all',sep = '_')))
  doc <- rbind(doc,summary((cur$DOCr_epi+cur$DOCr_hypo)/cur$Vol*12))
  boxplot((cur$DOCr_epi+cur$DOCr_hypo)/cur$Vol*12,outline = F)
}




