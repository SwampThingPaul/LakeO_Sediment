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
rgeos::gArea(lakeO)*1e-6

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

# bath2=raster::raster(paste0(paths[4],"/LakeOkeechobee_CompBathym_2014_Sfwmd/exported/LakeO_10ft"))
# proj4string(bath2)<-utm17
# bath2=bath2-(-1.32)
# bath2.m=bath2*0.3048

plot(bath2.m)
test=bath2.m<5
test2=bath2.m<4.9
plot(test)
plot(test2)


# https://stackoverflow.com/a/14470056
layout(matrix(1:2,1,2))
par(family="serif",mar=c(1,1,1,1),oma=c(0.5,0.5,0.5,1));
plot(bath.m<4.9,axes=F,box=F,legend=F)
mtext(side=3,"50 Ft Bath (<4.9 m)")
plot(test2,axes=F,box=F)
mtext(side=3,"10 Ft Bath (<4.9 m)")
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
  area<-temp.grd<z# Con(temp.grd<z,0,1)
  volume<-((z-temp.grd)*area)*ft.to.m(raster::res(area)[1])*ft.to.m(raster::res(area)[2])
  rslt=data.frame(z=z,
                  area=cellStats(area,sum)*ft.to.m(raster::res(area))[1]*ft.to.m(raster::res(area))[2],
                  volume=cellStats(volume,sum))
  return(rslt)
}

# 
# depth=ft.to.m(12.5)-bath.m
# plot(bath.m)
# plot(depth)
# depth2=mask(bath.m,bath.m>0,maskvalue=0)
# plot(depth2)


z.val=seq(8.8,18.55,0.1)
rslt=data.frame()
for(i in 1:length(z.val)){
tmp=inundate(ft.to.m(z.val[i]),bath.m)
rslt=rbind(tmp,rslt)
print(i)
}
rslt$area.km2=rslt$area*1e-6
rslt$volume.km3=rslt$volume*1e-9
rslt

# write.csv(rslt[,c("z","area.km2","volume.km3")],paste0(export.path,"Okeechobee_StageAreaVolume.csv"),row.names = F)

# rslt2=data.frame()
# for(i in 1:length(z.val)){
#   tmp=inundate(ft.to.m(z.val[i]),bath2.m)
#   rslt2=rbind(tmp,rslt2)
#   print(i)
# }
# rslt2$area.km2=rslt2$area*1e-6
# rslt2$volume.km3=rslt2$volume*1e-9
# rslt2
# write.csv(rslt2[,c("z","area.km2","volume.km3")],paste0(export.path,"Okeechobee_StageAreaVolume_10ftBath.csv"),row.names = F)
rslt2=read.csv(paste0(export.path,"Okeechobee_StageAreaVolume_10ftBath.csv"))
rslt2=subset(rslt2,z<5)

plot(area.km2~z,rslt2,type="l")
with(rslt,lines(z,area.km2,col="red"))

plot(volume.km3~z,rslt2,type="l")
with(rslt,lines(z,volume.km3,col="red"))


plot(area.km2~z,rslt)
stg.area=lm(area.km2~poly(z,5),rslt)
summary(stg.area)
gvlma::gvlma(stg.area)
lines(rslt$z,predict(stg.area),col="red")

plot(volume.km3~z,rslt)
stg.vol=lm(volume.km3~poly(z,2),rslt)
summary(stg.vol)
gvlma::gvlma(stg.vol)
lines(rslt$z,predict(stg.vol),col="red")

# write.csv(rslt[,c("z","area.km2","volume.km3")],paste0(export.path,"Okeechobee_StageAreaVolume.csv"),row.names = F)

# png(filename=paste0(plot.path,"LakeO_AreaVolumeStage.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2,0.25,0.5),oma=c(3,2.5,0.75,0.1));

xlim.val=c(2.5,5.6);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(1100,1800);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(area.km2~z,rslt,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(rslt,pt_line(z,area.km2,2,"dodgerblue1",1.5,21,"dodgerblue1",cex=1,pt.lwd=0.1))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Area (km\u00B2)")

ylim.val=c(1,7);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(volume.km3~z,rslt,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(rslt,pt_line(z,volume.km3,2,"dodgerblue1",1.5,21,"dodgerblue1",cex=1,pt.lwd=0.1))
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Volume (km\u00B3)")
mtext(side=1,line=2,"Stage Elevation (m, NGVD29)")
dev.off()


# png(filename=paste0(plot.path,"LakeO_AreaVolumeStage2.png"),width=4,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2,0.25,0.5),oma=c(3,2.5,0.75,0.1));

xlim.val=c(2.5,5.6);by.x=0.5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/2)
ylim.val=c(1100,1800);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(area.km2~z,rslt,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(rslt,lines(z,area.km2,col="dodgerblue1",lwd=1.5))
with(rslt2,lines(z,area.km2,col="indianred1",lwd=1.5))
axis_fun(1,xmaj,xmin,NA,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Area (km\u00B2)")
legend("topleft",legend=c("50 ft Bathymetry","10 ft Bathymetry"),
       lty=c(1),col=c("dodgerblue1","indianred1"),lwd=2,
       pt.cex=1,ncol=1,cex=1,bty="n",xpd=NA,xjust=0.5,y.intersp=1,x.intersp=0.75)

ylim.val=c(1,7);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(volume.km3~z,rslt,ylim=ylim.val,xlim=xlim.val,type="n",ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(rslt,lines(z,volume.km3,col="dodgerblue1",lwd=1.5))
with(rslt2,lines(z,volume.km3,col="indianred1",lwd=1.5))
axis_fun(1,xmaj,xmin,format(xmaj),line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.75,"Volume (km\u00B3)")
mtext(side=1,line=2,"Stage Elevation (m, NGVD29)")
dev.off()

# stage -------------------------------------------------------------------
WYs=seq(1979,2020,1)
dates=date.fun(c("1977-05-01","2020-05-01"))

lakeO.stg=DBHYDRO_daily(dates[1],dates[2],"00268")
lakeO.stg$Date.EST=date.fun(lakeO.stg$Date)
lakeO.stg$WY=WY(lakeO.stg$Date.EST)
lakeO.stg$month=as.numeric(format(lakeO.stg$Date.EST,"%m"))
lakeO.stg$CY=as.numeric(format(lakeO.stg$Date.EST,"%Y"))

plot(Data.Value~Date.EST,lakeO.stg)

# use models above for volume and area
lakeO.stg=cbind(lakeO.stg,data.frame(Vol.km3=predict(stg.vol,newdata=data.frame(z=ft.to.m(lakeO.stg$Data.Value)))))
lakeO.stg=cbind(lakeO.stg,data.frame(Area.km2=predict(stg.area,newdata=data.frame(z=ft.to.m(lakeO.stg$Data.Value)))))

mon.vol=ddply(lakeO.stg,c("CY","month","WY"),summarise,mean.vol.km3=mean(Vol.km3,na.rm=T),mean.area.km2=mean(Area.km2,na.rm=T))
WY.vol=ddply(lakeO.stg,c("WY"),summarise,mean.vol.km3=mean(Vol.km3,na.rm=T),mean.area.km2=mean(Area.km2,na.rm=T))



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
lake.wq.WY=cast(lake.wq.dat,WY~Param,value="HalfMDL",mean)
lake.wq.WY$TP_3Yr=with(lake.wq.WY,c(rep(NA,2),zoo::rollapply(TP,width=3,FUN=function(x)mean(x,na.rm=T))))

plot(lake.wq.mon$TP)
plot(TP~WY,lake.wq.WY)
with(lake.wq.WY,lines(WY,TP_3Yr,col="red"))

LakePMass=merge(lake.wq.mon[,c("month","CY","WY","TP","TN")],mon.vol,c("CY","month","WY"))
LakePMass$M_lake=with(LakePMass,(TP*(mean.vol.km3*1e12))*1e-6)

LakePMass.WY=ddply(LakePMass,"WY",summarise,TM_lake=sum(M_lake))
LakePMass.WY$deltaM_lake=with(LakePMass.WY,c(NA,diff(TM_lake)))
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

flow.mon.sum=cast(flow.xtab,STRUCT+ALIAS+Basin+WQSite+WY+CY+month~direct,value="fflow.cfs",fun.aggregate=function(x) sum(abs(x),na.rm=T))
flow.mon.sum=flow.mon.sum[,c("STRUCT", "ALIAS", "Basin", "WQSite", "WY", "CY", "month","Inflow", "Outflow")]

# Estimate flow for L31E, HP7 & Inflow 1,2,3
# Uncertainty is high using this method due to Q monitoring at C41H78, 
# see _docs/LOOP_C41H78_Justification_Final.docx
L61E_HP7=data.frame()
for(i in 1:nrow(mon.seq)){
  tmp.dat=subset(flow.mon.sum,month==mon.seq$month[i]&CY==mon.seq$CY[i])
  C41H78=if(nrow(subset(tmp.dat,ALIAS=="C41H78_I"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="C41H78_I")}
  G76=if(nrow(subset(tmp.dat,ALIAS=="G76_C"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="G76_C")}
  G207=if(nrow(subset(tmp.dat,ALIAS=="G207"))==0){data.frame(Outflow=0)}else{subset(tmp.dat,ALIAS=="G207")}
  S71=if(nrow(subset(tmp.dat,ALIAS=="S71_S"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="S71_S")}
  L61E_HP7.Q=C41H78$Inflow-((G207$Outflow*-1)+G76$Inflow+S71$Inflow)
  L61E_HP7.Q=ifelse(L61E_HP7.Q<0,0,L61E_HP7.Q)
  final.dat=data.frame(STRUCT="L61E_HP7",ALIAS="L61E_HP7",Basin="N",WQSite="L61E_HP7",WY=mon.seq$WY[i],CY=mon.seq$CY[i],month=mon.seq$month[i],Inflow=L61E_HP7.Q,Outflow=0)
  L61E_HP7=rbind(L61E_HP7,final.dat)
  print(i)
}
plot(L61E_HP7$Inflow)

flow.mon.sum2=rbind(subset(flow.mon.sum,ALIAS!="C41H78_I"),L61E_HP7)

WY.Tflow=ddply(flow.mon.sum,"WY",summarise,inflow.Q.cfs=sum(Inflow,na.rm=T),outflow.Q.cfs=sum(Outflow,na.rm=T))
WY.Tflow=merge(WY.Tflow,WY.vol,"WY")
WY.Tflow$ret.time=with(WY.Tflow,mean.vol.km3/cfs.to.km3d(outflow.Q.cfs)); # years
WY.Tflow=subset(WY.Tflow,WY%in%WYs)
WY.Tflow$HLR=with(WY.Tflow,cfs.to.m3d(inflow.Q.cfs)/rgeos::gArea(lakeO));# meters
plot(ret.time~WY,WY.Tflow,type="b")

# Water Quality -----------------------------------------------------------
wq.sites=ddply(flow.dbkeys,"WQSite",summarise,N.val=N.obs(ALIAS))

wq.param=data.frame(Test.Number=c(16,18,20,21,23,25,80,89,100),
                    Param=c("TSS","NOx","NH4","TKN","OP","TP","TN","DOC","TOC"))
wq.param=subset(wq.param,!(Param%in%c("TSS","NH4","DOC","TOC","OP")))
wq.dat=data.frame()
for(i in 1:nrow(wq.sites)){
  tmp=DBHYDRO_WQ(dates[1],dates[2],wq.sites$WQSite[i],wq.param$Test.Number)
  wq.dat=rbind(tmp,wq.dat)
  print(paste0(i,": ",wq.sites$WQSite[i]))
}

wq.dat=merge(wq.dat,wq.param,"Test.Number")
unique(wq.dat$Collection.Method)
wq.dat=subset(wq.dat,Collection.Method=="G") # Grab sample only 
wq.dat$month=as.numeric(format(wq.dat$Date.EST,"%m"))
wq.dat$CY=as.numeric(format(wq.dat$Date.EST,"%Y"))
wq.dat$WY=WY(wq.dat$Date.EST)
N.TP=ddply(subset(wq.dat,Param=="TP"),c("WY","Station.ID"),summarise,N.val=N.obs(HalfMDL),mean.val=mean(HalfMDL,na.rm=T))
N.TP

# checking for more than one sample per day
# test=cast(subset(wq.dat,Project.Code!="LAB"),Station.ID+Date.EST~Param,value="HalfMDL",fun.aggregate = function(x)N.obs(x))
# subset(test,TP>2)
# subset(wq.dat,Param=="TP"&Station.ID=="S65E"&Date.EST==date.fun("1990-06-18"))

# Daily mean WQ removing LAB project code.
wq.dat.xtab=cast(subset(wq.dat,Project.Code!="LAB"),Station.ID+Date.EST~Param,value="HalfMDL",mean)
wq.dat.xtab$TN=with(wq.dat.xtab,TN_Combine(NOx,TKN,TN))
head(wq.dat.xtab)


ALIAS.vals=ddply(flow.dbkeys,"ALIAS",summarise,N.val=N.obs(DBKEY))
date.fill.frame=expand.grid(Date.EST=date.fun(seq(dates[1],dates[2],"1 days")),
                            ALIAS=ALIAS.vals$ALIAS)
flow.vars=c("STRUCT","ALIAS","WQSite","Basin","Date.EST","WY","direct","fflow.cfs")
wq.vars=c("Date.EST","Station.ID","TP","TN")

flow.wq=merge(flow.xtab[,flow.vars],wq.dat.xtab[,wq.vars],by.x=c("Date.EST","WQSite"),by.y=c("Date.EST","Station.ID"),all.x=T)
head(flow.wq)
flow.wq=merge(date.fill.frame,flow.wq,c("Date.EST","ALIAS"),all.y=T)
flow.wq=flow.wq[order(flow.wq$ALIAS,flow.wq$Date.EST),]

# ddply(flow.wq,c("ALIAS","STRUCT",'WQSite'),summarise,N.val=N.obs(TP))
flow.wq$TP.int=with(flow.wq,ave(TP,ALIAS,FUN = function(x)dat.interp(x)))
flow.wq$TPLoad.kg=with(flow.wq,Load.Calc.kg(abs(fflow.cfs),TP.int))
flow.wq$TN.int=with(flow.wq,ave(TN,ALIAS,FUN = function(x)dat.interp(x)))
flow.wq$TNLoad.kg=with(flow.wq,Load.Calc.kg(abs(fflow.cfs),TN.int))

# plot(fflow.cfs~Date.EST,subset(flow.wq,STRUCT=="CU10"))
# plot(TPLoad.kg~Date.EST,subset(flow.wq,STRUCT=="CU10"))
# plot(TNLoad.kg~Date.EST,subset(flow.wq,STRUCT=="CU10"))

flow.wq$month=as.numeric(format(flow.wq$Date.EST,"%m"))
flow.wq$CY=as.numeric(format(flow.wq$Date.EST,"%Y"))

TPload.mon.sum=cast(flow.wq,STRUCT+ALIAS+WQSite+Basin+WY+CY+month~direct,value="TPLoad.kg",fun.aggregate=function(x) sum(x,na.rm=T))
TPload.mon.sum=TPload.mon.sum[,c("STRUCT", "ALIAS","WQSite", "Basin", "WY", "CY", "month","Inflow", "Outflow")]

TNload.mon.sum=cast(flow.wq,STRUCT+ALIAS+WQSite+Basin+WY+CY+month~direct,value="TNLoad.kg",fun.aggregate=function(x) sum(x,na.rm=T))
TNload.mon.sum=TNload.mon.sum[,c("STRUCT", "ALIAS","WQSite", "Basin", "WY", "CY", "month","Inflow", "Outflow")]

# Estimate load for L31E, HP7 & Inflow 1,2,3
# overly complicated and highly variable estimates of flow and load due to
# shallow open channel discharge (see _docs/LOOP_C41H78_Justification_Final.docx)
L61E_HP7.TPload=data.frame()
for(i in 1:nrow(mon.seq)){
  tmp.dat=subset(TPload.mon.sum,month==mon.seq$month[i]&CY==mon.seq$CY[i])
  C41H78=if(nrow(subset(tmp.dat,ALIAS=="C41H78_I"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="C41H78_I")}
  G76=if(nrow(subset(tmp.dat,ALIAS=="G76_C"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="G76_C")}
  G207=if(nrow(subset(tmp.dat,ALIAS=="G207"))==0){data.frame(Outflow=0)}else{subset(tmp.dat,ALIAS=="G207")}
  S71=if(nrow(subset(tmp.dat,ALIAS=="S71_S"))==0){data.frame(Inflow=0)}else{subset(tmp.dat,ALIAS=="S71_S")}
  L61E_HP7.load.val=C41H78$Inflow-((G207$Outflow*-1)+G76$Inflow+S71$Inflow)
  L61E_HP7.load.val=ifelse(L61E_HP7.load.val<0,0,L61E_HP7.load.val)
  final.dat=data.frame(STRUCT="L61E_HP7",ALIAS="L61E_HP7",WQSite=NA,Basin="N",WY=mon.seq$WY[i],CY=mon.seq$CY[i],month=mon.seq$month[i],Inflow=L61E_HP7.load.val,Outflow=0)
  L61E_HP7.TPload=rbind(L61E_HP7.TPload,final.dat)
  print(i)
}
plot(L61E_HP7.TPload$Inflow)
TPload.mon.sum2=rbind(subset(TPload.mon.sum,ALIAS!="C41H78_I"),L61E_HP7.TPload)

# WY.TNLoad=ddply(TNload.mon.sum,"WY",summarise,inflow.load=sum(Inflow,na.rm=T),outflow.load=sum(Outflow,na.rm=T))
WY.TPLoad=ddply(TPload.mon.sum,"WY",summarise,inflow.load=sum(Inflow,na.rm=T),outflow.load=sum(Outflow,na.rm=T))
WY.TPLoad=subset(WY.TPLoad,WY%in%WYs)

WY.TPLoad=merge(WY.TPLoad,LakePMass.WY,"WY")
WY.TPLoad$s_yr=with(WY.TPLoad,((inflow.load-outflow.load)-deltaM_lake)/TM_lake)

plot((inflow.load-TM_lake)~WY,WY.TPLoad)
plot(s_yr~WY,WY.TPLoad)

WY.TPLoad$L_p=with(WY.TPLoad,inflow.load/rgeos::gArea(lakeO))*1000
WY.TPLoad.flow=merge(WY.TPLoad,WY.Tflow,"WY")

# WY.TPLoad.flow$P_lake=with(WY.TPLoad.flow,L_p/(HLR*(1+ret.time^0.5)))
WY.TPLoad.flow$P_lake=with(WY.TPLoad.flow,0.682*L_p/(HLR*(1+sqrt(ret.time)))^0.934);# Havens & James 1997 - Eq 3

WY.TPLoad.flow$P_lake_3Yr=with(WY.TPLoad.flow,c(rep(NA,2),zoo::rollapply(P_lake,width=3,FUN=function(x)mean(x,na.rm=T))))


# png(filename=paste0(plot.path,"Vollenweider.png"),width=5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2,3.75,1,0.25));

xlim.val=c(1979,2020);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,0.30);by.y=0.050;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(TP~WY,lake.wq.WY,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
with(lake.wq.WY,pt_line(WY,TP,2,"indianred1",1,21,"indianred1",pt.lwd=0.1,cex=1))
with(lake.wq.WY,lines(WY,TP_3Yr,col=adjustcolor("dodgerblue1",0.5),lwd=2))
with(WY.TPLoad.flow,pt_line(WY,P_lake,2,"grey",1,21,"grey",pt.lwd=0.1,cex=1))
with(WY.TPLoad.flow,lines(WY,P_lake_3Yr,col=adjustcolor("black",0.5),lwd=2))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj*1000);box(lwd=1)
mtext(side=1,line=1.75,"Water Year")
mtext(side=2,line=3,"Lake TP (\u03BCg L\u207B\u00B9)")
legend("topleft",legend=c("Spatially Averaged (Obs)","3-Yr moving avg (Obs)","Vollenweider Model","3-Yr moving avg (Modeled)"),
       pch=c(21,NA,21,NA),lty=c(NA,1,NA,1),lwd=c(0.1,2,0.1,2),
       col=c("black",adjustcolor("dodgerblue1",0.5),"black",adjustcolor("black",0.5)),
       pt.bg=c("indianred1",NA,"grey",NA),pt.cex=1,
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()

# png(filename=paste0(plot.path,"NetSettle.png"),width=5,height=3,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(1,0.75,0.25,1),oma=c(2,3.75,1,0.25));

xlim.val=c(1979,2021);by.x=10;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(-0.5,1.25);by.y=0.5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(s_yr~WY,WY.TPLoad,ylim=ylim.val,xlim=xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey")
abline(h=0)
with(WY.TPLoad,pt_line(WY,s_yr,2,"dodgerblue1",1,21,"dodgerblue1",pt.lwd=0.1,cex=1))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=1,line=1.75,"Water Year")
mtext(side=2,line=2.5,"Net P Sedimentation\nCoefficient (WY\u207B\u00B9)")
text(2021.25,0.625,"Sink",srt=90,col="red")
text(2021.25,-0.25,"Source",srt=90,col="red")
dev.off()

plot(s_yr~WY,WY.TPLoad)

sum(WY.TPLoad$s_yr<0)/N.obs(WY.TPLoad$s_yr)
