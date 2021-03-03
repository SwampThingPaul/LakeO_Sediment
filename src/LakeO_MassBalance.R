## 
## Lake Okeechobee Bathymetry and Retention Time Calc
##
## Code was compiled by Paul Julian
## contact info: pauljulianphd@gmail.com

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape)
library(openxlsx)

# GIS libraries 
# library(sp)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(gstat)

#thin plate spline https://rspatial.org/raster/analysis/4-interpolation.html
library(fields)
library(raster)

## Paths
wd="C:/Julian_LaCie/_GitHub/LakeO_Sediment"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]
GIS.path.gen=paste0(dirname(dirname(wd)),"/_GISData")

# Helper variables
# epsg.io
nad83=CRS(SRS_string ="EPSG:4269")
wgs84=CRS(SRS_string ="EPSG:4326")
utm17=CRS(SRS_string ="EPSG:26917")

# GIS Data ----------------------------------------------------------------
# ogrListLayers(GIS.path)
lakeO=spTransform(readOGR(paste0(GIS.path.gen,"/SFWMD"),"LakeOkeechobee_general"),wkt(utm17))
plot(lakeO)

# Lake Okeechobee 2001 raw XYZ bathymetry data
# https://pubs.usgs.gov/ds/1031/ds1031_data.html
# bath=spTransform(readOGR(paste0(paths[4],"/Bathymetry"),"DS1031-LOkee_WGS84_NAVD88-G99_SB.xyz"),wkt(utm17))
# bath$NGVD29=bath$NAVD88-(-1.32)

# plot(bath)
# plot(lakeO,border="red",add=T)

# m.to.ft(mean(bath$NGVD29))
mean.z=8.55

bath=raster::raster(paste0(paths[4],"/LakeOkeechobee_Usace/spatial/export_raster/Bathym_50ft.tif"))
proj4string(bath)<-utm17
bath=bath-(-1.32)
bath.m=bath*0.3048

plot(bath)
plot(bath.m)

# bath resolution is 50ft x 50ft grid cells
# bath.m2=aggregate(bath.m,5,fun=mean,expand=T,na.rm=T)
# res(bath.m2)
# plot(bath.m2)

## Idea from 
## Jones CN, Evenson GR, McLaughlin DL, et al (2018) 
## Estimating restorable wetland water storage at landscape scales. 
## Hydrological Processes 32:305–313. 
## doi: https://doi.org/10.1002/hyp.11405

Con<-function(condition,trueValue,falseValue){
  return(condition*trueValue+(!condition)*falseValue)
}

inundate<-function(z,temp.grd){
  area<-Con(temp.grd>z,0,1)
  volume<-((z-temp.grd)*area)*ft.to.m(raster::res(area)[1])*ft.to.m(raster::res(area)[2])
  rslt=data.frame(z=z,
                  area=cellStats(area,'sum')*ft.to.m(raster::res(area)[1]),
                  volume=cellStats(volume,'sum'))
  return(rslt)
}

plot(bath)
plot(bath*0+minValue(bath))
# 
# depth=ft.to.m(12.5)-bath.m
# plot(bath.m)
# plot(depth)
# depth2=mask(bath.m,bath.m>0,maskvalue=0)
# plot(depth2)

z.val=seq(8.8,18.6,0.2)
rslt=data.frame()
for(i in 1:length(z.val)){
tmp=inundate(ft.to.m(z.val[i]),bath.m)
rslt=rbind(tmp,rslt)
print(i)
}
rslt$area.km2=rslt$area*1e-6
rslt$volume.km3=rslt$volume*1e-9
rslt
write.csv(rslt[,c("z","area.km2","volume.km3")],paste0(export.path,"Okeechobee_StageAreaVolume.csv"),row.names = F)

# png(filename=paste0(plot.path,"LakeO_AreaVolumeStage.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2,0.25,0.5),oma=c(3,2.5,0.75,0.1));

xlim.val=c(2.5,5.6);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(0,180);by.y=40;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(area.km2~z,rslt,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(rslt,pt_line(z,area.km2,2,"dodgerblue1",1.5,21,"dodgerblue1",cex=1,pt.lwd=0.1))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Area (km\u00B2)")

ylim.val=c(0,10);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(volume.km3~z,rslt,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(rslt,pt_line(z,volume.km3,2,"dodgerblue1",1.5,21,"dodgerblue1",cex=1,pt.lwd=0.1))
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.5,"Volume (km\u00B3)")
mtext(side=1,line=2,"Stage Elevation (m, NGVD29)")
dev.off()


# stage -------------------------------------------------------------------
dates=date.fun(c("1978-05-01","2020-05-01"))

lakeO.stg=DBHYDRO_daily(dates[1],dates[2],"00268")
lakeO.stg$Date.EST=date.fun(lakeO.stg$Date)
lakeO.stg$WY=WY(lakeO.stg$Date.EST)
lakeO.stg$month=as.numeric(format(lakeO.stg$Date.EST,"%m"))
lakeO.stg$CY=as.numeric(format(lakeO.stg$Date.EST,"%Y"))
lakeO.stg$vol.km3=((lakeO.stg$Data.Value-mean.z)*0.0003048)*(area(lakeO)*1e-6)

plot(Data.Value~Date.EST,lakeO.stg)

plot(vol.km3~Date,lakeO.stg)
plot(Data.Value~Date,lakeO.stg,ylim=c(8,20))
abline(h=8.55)

WY.vol=ddply(lakeO.stg,c("WY"),summarise,mean.vol.km3=mean(vol.km3,na.rm=T))
plot(mean.vol.km3~WY,WY.vol,type="l")
# retention time lake volume divided by Q in or out

mon.vol=ddply(lakeO.stg,c("CY","month","WY"),summarise,mean.vol.km3=mean(vol.km3,na.rm=T))


# in-situ WQ --------------------------------------------------------------
wq.param=data.frame(Test.Number=c(16,18,20,21,23,25,80,89,100),
                    Param=c("TSS","NOx","NH4","TKN","OP","TP","TN","DOC","TOC"))
wq.param=subset(wq.param,!(Param%in%c("TSS","NH4","DOC","TOC","OP")))


lake.wq.sites=c(paste0("L00",c(1,4:8)),"LZ30","LZ40")
lake.wq.dat=data.frame()
for(i in 1:length(lake.wq.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],lake.wq.sites[i],wq.param$Test.Number)
  lake.wq.dat=rbind(tmp,lake.wq.dat)
  print(paste0(i,": ",lake.wq.sites[i]))
}

lake.wq.dat=merge(lake.wq.dat,wq.param,"Test.Number")
lake.wq.dat$month=as.numeric(format(lake.wq.dat$Date.EST,"%m"))
lake.wq.dat$CY=as.numeric(format(lake.wq.dat$Date.EST,"%Y"))
lake.wq.dat$WY=WY(lake.wq.dat$Date.EST)

lake.wq.mon=cast(lake.wq.dat,CY+month+WY~Param,value="HalfMDL",mean)

plot(lake.wq.mon$TP)

LakePMass=merge(lake.wq.mon[,c("month","CY","WY","TP")],mon.vol,c("CY","month","WY"))
LakePMass$M_lake=with(LakePMass,(TP*(mean.vol.km3*1e12))*1e-6)

ddply(LakePMass,"WY",summarise,TM_lake=sum(M_lake))

# Discharge ---------------------------------------------------------------
flow.dbkeys=read.xlsx(paste0(data.path,"discharge/LakeO_DBKEYS_V3.xlsx"),sheet=1)

flow.dat=data.frame()
for(i in 1:nrow(flow.dbkeys)){
  tmp=DBHYDRO_daily(dates[1],dates[2],flow.dbkeys$DBKEY[i])
  tmp$DBKEY=as.character(flow.dbkeys$DBKEY[i])
  flow.dat=rbind(tmp,flow.dat)
  print(paste(i,": ",flow.dbkeys$DBKEY[i]))
}
# write.csv(flow.dat,paste0(data.path,"discharge/WY1979_2020_dailyQ.csv"),row.names=F)

flow.data=merge(flow.dat,flow.dbkeys[,c("DBKEY","STRUCT","ALIAS","Priority","Basin","Inflow_direction","Outflow","WQSite")],"DBKEY")
flow.data$WY=WY(flow.data$Date)

flow.xtab=data.frame(cast(flow.data,Date+WY+STRUCT+ALIAS+Inflow_direction+Outflow+Basin+WQSite~Priority,value="Data.Value",fun.aggregate=function(x) ifelse(sum(x,na.rm=T)==0,NA,sum(x,na.rm=T))))
flow.xtab$Date.EST=date.fun(flow.xtab$Date)

flow.xtab$fflow.cfs=with(flow.xtab,ifelse(is.na(P1),P2,P1));#if only two priorities
flow.xtab$fflow.cfs=with(flow.xtab,fflow.cfs*Inflow_direction)#all inflows are positive and all negative values are outflow
flow.xtab$direct=with(flow.xtab,ifelse(fflow.cfs<0,"Outflow","Inflow"))
flow.xtab$month=as.numeric(format(flow.xtab$Date,"%m"))
flow.xtab$CY=as.numeric(format(flow.xtab$Date,"%Y"))

mon.seq=data.frame(Date.EST=date.fun(seq(dates[1],dates[2],"1 months")))
mon.seq$month=as.numeric(format(mon.seq$Date.EST,"%m"))
mon.seq$CY=as.numeric(format(mon.seq$Date.EST,"%Y"))
mon.seq$WY=WY(mon.seq$Date.EST)

flow.WY.sum=cast(flow.xtab,WY~direct,value="fflow.cfs",fun.aggregate=function(x) sum(cfs.to.km3d(abs(x)),na.rm=T))
flow.WY.sum=subset(flow.WY.sum,WY%in%seq(1979,2020,1))
flow.WY.sum=merge(flow.WY.sum,WY.vol,"WY")
flow.WY.sum$ret.time=with(flow.WY.sum,mean.vol.km3/Outflow)

plot(ret.time~WY,flow.WY.sum,type="b")
