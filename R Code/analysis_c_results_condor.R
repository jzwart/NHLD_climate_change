# analysis of condor output; JAZ 2018-01-10 

dir <- 'D:/Condor_Results/'

scenarios <- list.files(dir)

skip=6*365 # days to skip; first 6 years (spin up)

watersheds<-read.table('/Users/jzwart/Documents/Jake/MyPapers/Regional Lake Carbon Model - ECI/Data/C model forcing data/NHLDsheds_20170323.txt',
                       stringsAsFactors = F,header=T,sep = '\t')

threshold <- 0.05 # if volume of lake is below X% of original volume, don't include this in analyses because DOC / kD / etc.. were blowing up at low volumes 
for(i in 1:length(scenarios)){
  print(i)
  
  condor_lookup <- read.csv('/Users/jzwart/NHLD_climate_change/Condor/allruns_condor_lookup.csv',stringsAsFactors = F)
  
  sub_dir <- file.path(dir,scenarios[i])
  # folder <- list.files(sub_dir)
  # file_path <- file.path(sub_dir,folder[length(folder)]) # take most recent run (sorted by date)
  files <- list.files(sub_dir)
  if(length(files[grep('adj',files)])>0){
    files <- files[-grep('adj',files)]
  }
  
  sum <- data.frame() # open ice only
  all <- data.frame() 
  for(j in 1:length(files)){ # length(files
    print(c(i,j))
    # cur<-read.table(file.path(dir,files[j]),stringsAsFactors = F,sep='\t',header=T) # when I was reading in .txt files
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
  
  assign(paste(scenarios[i],'all',sep = '_'),value = all)
  assign(paste(scenarios[i],'sum',sep = '_'),value = sum)
}

# check to see if all lakes are in each scenario
lakes <- c()
for(i in 1:length(scenarios)){
  cur <- eval(parse(text=paste(scenarios[i],'all',sep = '_')))
  lakes <- c(lakes, cur$Permanent_)
}
lakes <- lakes[!duplicated(lakes)] # all lakes among sceario runs 

for(i in 1:length(scenarios)){ # lakes common to every scenario run 
  cur <- eval(parse(text=paste(scenarios[i],'all',sep = '_')))
  lakes <- lakes[lakes%in%cur$Permanent_]
}
for(i in 1:length(scenarios)){ # keeping only lakes common to every scenario run 
  cur <- eval(parse(text=paste(scenarios[i],'all',sep = '_')))
  curSum <- eval(parse(text=paste(scenarios[i],'sum',sep = '_')))
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
  
  lakeSizeBins <- c(0,0.01,.1,1,10,100)*1e6 # breaks for max cutoff of lake size from Downing et al. 2006
  cur$lakeSizeBins <- cut(cur$Area, breaks = lakeSizeBins)
  curSum$lakeSizeBins <- cut(curSum$Area, breaks = lakeSizeBins)
  
  assign(paste(scenarios[i],'all',sep = '_'),value = cur)
  assign(paste(scenarios[i],'sum',sep = '_'),value = curSum)
}


















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




