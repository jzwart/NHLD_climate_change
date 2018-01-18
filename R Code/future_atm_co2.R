# creating CO2 concentrations for 2050s and 2080s based on keeling curve and RCP 8.5 [CO2] 

rcp <- read.table('/Users/jzwart/NHLD_climate_change/Data/Atm/RCP85_MIDYR_CONC/RCP85_MIDYR_CONC.DAT',skip=38,header=T,stringsAsFactors = F)

rcp <- rcp[,c("YEARS","CO2")]
colnames(rcp) <- c('YEARS','rcp_co2')

keeling <- read.table('/Users/jzwart/NHLD_climate_change/Data/Atm/keelingCurve.txt',stringsAsFactors = F,sep =' ')
colnames(keeling)<-c('site_code','year','month','day','hour','minute','second','co2','value_unc','nvalue','latitude','longitude','altitude','elevation','intake_height','instrument','qcflag')
keeling$datetime<-as.Date(paste(keeling$year,keeling$month,keeling$day),format='%Y %m %d')
keeling$co2<-ifelse(keeling$co2==-999.99,NA,keeling$co2)
keeling$co2[1]<-keeling$co2[min(which(!is.na(keeling$co2)))]
keeling$co2[length(keeling$site_code)]<-keeling$co2[max(which(!is.na(keeling$co2)))]
keeling$co2<-approx(keeling$datetime,keeling$co2,keeling$datetime)$y #filling in NA's / missing data 
keeling<-keeling[,c('datetime','co2')]
# 
# test <- keeling
# 
# test$YEARS <- strftime(test$datetime,format = '%Y')
# test <- merge(test,rcp,all.x=T)
# 
# windows()
# plot(test$co2~test$datetime)
# lines(test$CO2~test$datetime,col='red') # RCP annual averages look good 

# windows()
# smooth_keeling <- zoo::rollmean(keeling$co2,k=365,align='center')
# plot(smooth_keeling)
# 
# windows()
# smooth_keeling <- forecast::ma(keeling$co2,order=365,centre = T)
# plot(smooth_keeling)

# signal_keeling <- test$co2-test$CO2

# plot(signal_keeling,type='l')
# 
# plot(smooth_keeling)

# keeling$smooth_co2 <- forecast::ma(keeling$co2,order=365,centre = T)
# windows()
# plot(keeling$co2~keeling$datetime,type='l')
# lines(keeling$smooth_co2~keeling$datetime,col='red')
# lines(test$CO2~test$datetime,col='blue')
# 
# keeling$signal_co2 <- keeling$co2-keeling$smooth_co2
# plot(keeling$signal_co2)
# mean(keeling$signal_co2,na.rm=T) # should be close to zero 

# forcings date range for historic 
date_hist <- read.table('/Users/jzwart/NHLD_climate_change/Data/Water/NHLD_Results_2017_11_29_CESM1_CAM5/Date_OUT_ALL.txt',stringsAsFactors = F,header = F,
                        sep='\t')
datetime<-as.Date(paste(date_hist$V1,date_hist$V2,date_hist$V3),format = '%Y %m %d')

keeling_out <- keeling[keeling$datetime>=min(datetime)&keeling$datetime<=max(datetime),]

keeling_out$YEARS <- strftime(keeling_out$datetime,format = '%Y')
keeling_out <- merge(keeling_out, rcp, all.x=T)

keeling_out$signal_co2 <- keeling_out$co2-keeling_out$rcp_co2
plot(keeling_out$signal_co2,type='l')
mean(keeling_out$signal_co2) # should be fairly close to zero 

# RCP forcings are based on keeling at start of the year (e.g. 1990-01-01) and then stay the same until end of year (e.g. 1990-12-31) 
# pull out signal from RCP and add this onto future 2050s and 2080s (centered around 2055 and 2085)
# datetime length is 34 
sim_years <- round(as.numeric(datetime[length(datetime)]-datetime[1])/365) # how many years we span in simulation 
start_month_day <- strftime(datetime[1],format = '%m-%d')
end_month_day <- strftime(datetime[length(datetime)],format = '%m-%d')

seq_2050s <- seq(from=as.Date(paste(2055-sim_years/2,start_month_day,sep='-')),to=as.Date(paste(2055+sim_years/2,end_month_day,sep='-')),
                 by='day') # sequence of simulation days 
seq_2080s <- seq(from=as.Date(paste(2085-sim_years/2,start_month_day,sep='-')),to=as.Date(paste(2085+sim_years/2,end_month_day,sep='-')),
                 by='day') # sequence of simulation days 

co2_2050s <- data.frame(datetime = seq_2050s, YEARS = strftime(seq_2050s, format = '%Y'),
                        signal_co2 = keeling_out$signal_co2[1:length(seq_2050s)]) 
co2_2080s <- data.frame(datetime = seq_2080s, YEARS = strftime(seq_2080s, format = '%Y'),
                        signal_co2 = keeling_out$signal_co2[1:length(seq_2080s)])# have to cut two days off of signal because of different seq of dates in 2080s 

co2_2050s <- merge(co2_2050s, rcp, all.x=T)
co2_2080s <- merge(co2_2080s, rcp, all.x=T)

co2_2050s$co2 <- co2_2050s$signal_co2+co2_2050s$rcp_co2
co2_2080s$co2 <- co2_2080s$signal_co2+co2_2080s$rcp_co2

co2_2050s$orig_datetime <- datetime[1:length(co2_2050s$YEARS)] # these dates are how Zach saves model output for scearios
co2_2080s$orig_datetime <- datetime[1:length(co2_2080s$YEARS)]

# windows()
# plot(co2_2050s$co2~co2_2050s$datetime,type='l')
# 
# plot(co2_2080s$co2~co2_2080s$datetime,type='l')

write.table(co2_2050s, '/Users/jzwart/NHLD_climate_change/Data/Atm/co2_2050s.txt',sep='\t',quote=F,row.names=F)
write.table(co2_2080s, '/Users/jzwart/NHLD_climate_change/Data/Atm/co2_2080s.txt',sep='\t',quote=F,row.names=F)







