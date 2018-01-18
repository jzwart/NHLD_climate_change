# creating lookup table for Forcing files for Condor runs 
# JAZ; 2018-01-08 
require(sp)
require(rgeos)
require(rgdal)
require(maptools)

runs <- read.csv('/Users/jzwart/NHLD_climate_change/Condor/allruns.csv',stringsAsFactors = F)

forceDir <- '/Users/jzwart/NHLD_climate_change/Data/Water/NHLD_Results_2017_11_29_CESM1_CAM5/DisAgg_FORCE/'

forceFiles <- list.files(forceDir)


lakeShedDir<-'/Users/jzwart/NHLD_climate_change/Data/GIS/'

# lake location, area, and perimeter 
lakeLocation<-readShapeSpatial(file.path(lakeShedDir,'LakeLocations/NHLDandBuffLakes_Original_ZJH_Rev1.shp'))
lakeLocation<-data.frame(lakeLocation)
lakeLocation$Permanent_<-as.character(lakeLocation$Permanent_)

# coordinate lookup table for forcing data 
forceCoordList<-strsplit(list.files(forceDir),'_')
if(length(grep('format.txt',forceCoordList,fixed=T))>0){
  forceCoordList<-forceCoordList[-grep('format.txt',forceCoordList,fixed=T)]
}
forceCoord<-data.frame()
for(i in 1:length(forceCoordList)){
  cur<-data.frame(Longitude=forceCoordList[[i]][3],Latitude=forceCoordList[[i]][2])
  forceCoord<-rbind(forceCoord,cur)
}
forceCoord$Longitude<-as.numeric(as.character(forceCoord$Longitude))
forceCoord$Latitude<-as.numeric(as.character(forceCoord$Latitude))
sp.forceCoord<-forceCoord
coordinates(sp.forceCoord)<- ~Longitude+Latitude

lakes <- runs$currID
runs$force <- rep(NA,length(runs$currID))

for(i in 1:length(lakes)){
  # location of lake used for finding correct forcing data 
  curLakeLoc<-lakeLocation[lakeLocation$Permanent_==lakes[i],]
  sp.curLakeLoc<-curLakeLoc
  coordinates(sp.curLakeLoc)<- ~Longitude+Latitude
  # find closest forcing data 
  curForceCoord<-forceCoord[which(gDistance(sp.forceCoord,sp.curLakeLoc,byid=T)==gDistance(sp.forceCoord,sp.curLakeLoc)),]
  # curForce<-read.table(file.path(forceDir,paste('FORCE',curForceCoord$Latitude,curForceCoord$Longitude,sep='_')),header=F,stringsAsFactors = F)
  curForce<-paste('FORCE',curForceCoord$Latitude,curForceCoord$Longitude,sep='_')
  runs$force[i] <- curForce
}


runs

write.csv(x = runs,'/Users/jzwart/NHLD_climate_change/allruns.csv',row.names = F)







