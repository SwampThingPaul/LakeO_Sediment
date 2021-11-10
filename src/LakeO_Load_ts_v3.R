##
## Lake Okeechobee flow and water quality (inflow/outflow)
## 
## Code was compiled by Paul Julian
## contact infor: paul.julian@dep.state.fl.us 

#Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

#Libraries
library(AnalystHelper)
library(plyr);
library(reshape);

library(RColorBrewer)
library(openxlsx)
library(zyp)
library(flextable)
library(magrittr)

#GIS libraries
library(rgdal)
library(sp)
library(raster)
library(maptools)
library(tmap)

# original script/DBKEYS in .../Work/LakeOkeechobee/...
wd="C:/Julian_LaCie/_GitHub/LakeO_Sediment"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/"))
# Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

GIS.path.gen="C:/Julian_LaCie/_GISData"

# -------------------------------------------------------------------------
nad83.pro=CRS(SRS_string="EPSG:4269")
utm17=CRS(SRS_string="EPSG:26917")

tmap_mode("view")

# -------------------------------------------------------------------------
dates=date.fun(c("1978-05-01","2021-04-30"))

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
# Sanity Check
# sum(cfs.to.acftd(subset(flow.mon.sum2,WY==2019)$Inflow),na.rm=T)
# ((2020085 - 1989631)/1989631)*100
## less than 5% of the report values (1.5%diff)

# sum(cfs.to.acftd(subset(flow.mon.sum2,WY==2019)$Outflow),na.rm=T)
# ((2308631 - 2352132)/2352132)*100
## less than 5% of the report values (1.85%diff)

# Compare estimated Q for L31E, HP7 & Inflow 1,2,3 versus not. 
# test=merge(ddply(flow.mon.sum2,"WY",summarise,TFlow.acft=sum(cfs.to.acftd(Inflow),na.rm=T)),
#            ddply(flow.mon.sum,"WY",summarise,TFlow.acft2=sum(cfs.to.acftd(Inflow),na.rm=T)),"WY")
# plot(TFlow.acft~TFlow.acft2,test);abline(0,1)

all.out=ddply(subset(flow.mon.sum,WY%in%seq(1979,2020,1)),"WY",summarise,Tflow=sum(cfs.to.acftd(Outflow),na.rm=T))
mean(subset(all.out,WY%in%seq(2016,2020,1))$Tflow)/1000
mean(subset(all.out,WY%in%seq(2008,2020,1))$Tflow)/10e2

south.out=ddply(subset(flow.mon.sum,Basin=="S"&WY%in%seq(1979,2020,1)),"WY",summarise,Tflow=sum(cfs.to.acftd(Outflow),na.rm=T))
south.out=merge(south.out,data.frame(WY=2016:2020,WY5mean=mean(subset(south.out,WY%in%seq(2016,2020,1))$Tflow)),all.x=T)
south.out=merge(south.out,data.frame(WY=2008:2020,LORS08mean=mean(subset(south.out,WY%in%seq(2008,2020,1))$Tflow)),all.x=T)

# png(filename=paste0(plot.path,"LakeO_QSouth.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
# layout(matrix(1:2,2,1))
par(family="serif",mar=c(1,2.25,0.25,0.5),oma=c(3,2.5,0.75,0.1));

xlim.val=c(1979,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)
ylim.val=c(0,4e6);by.y=1e6;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

plot(Tflow~WY,all.out,type="n",xlim=xlim.val,ylim=ylim.val,axes=F,ann=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(all.out,pt_line(WY,Tflow,2,"dodgerblue1",1,21,"dodgerblue1"))
with(south.out,pt_line(WY,Tflow,2,"indianred1",1,21,"indianred1"))
with(south.out,lines(WY,WY5mean,lwd=2,col="forestgreen",lty=2))
with(south.out,lines(WY,LORS08mean,lwd=3,col="indianred1"))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj/1000);box(lwd=1)
mtext(side=2,line=2.75,"Discharge (x1000 Ac-Ft WY\u207B\u00B9)")
mtext(side=1,line=2,"Water Year")

legend("topleft",legend=c("Total Outflow","Total Outflow South","5WY Avg","LORS08 Avg"),
       pch=c(21,21,NA,NA),lty=c(NA,NA,2,1),lwd=c(0.1,0.1,2,2),
       col=c("black","black","forestgreen","indianred1"),
       pt.bg=c("dodgerblue1","indianred1",NA,NA),pt.cex=1,
       ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5)
dev.off()
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
## Sanity check
# sum(kg.to.mt(subset(TPload.mon.sum2,WY==2019)$Inflow),na.rm=T)
# ((409 - 404)/404)*100
## less than 5% of the report values (1.2%diff)

# sum(kg.to.mt(subset(TPload.mon.sum2,WY==2019)$Outflow),na.rm=T)

# test=merge(ddply(TPload.mon.sum2,"WY",summarise,TLoad2=sum(Inflow,na.rm=T)),
# ddply(TPload.mon.sum,"WY",summarise,TLoad=sum(Inflow,na.rm=T)),"WY")
# plot(TLoad2~TLoad,test);abline(0,1)

# Annual Laod -------------------------------------------------------------
WY.TPLoad=ddply(TPload.mon.sum,"WY",summarise,inflow.load=sum(Inflow,na.rm=T),outflow.load=sum(Outflow,na.rm=T))
WY.TNLoad=ddply(TNload.mon.sum,"WY",summarise,inflow.load=sum(Inflow,na.rm=T),outflow.load=sum(Outflow,na.rm=T))
WY.Tflow=ddply(flow.mon.sum,"WY",summarise,inflow.Q=sum(cfs.to.acftd(Inflow),na.rm=T),outflow.Q=sum(cfs.to.acftd(Outflow),na.rm=T))

WY.TPLoad.Q=merge(WY.TPLoad,WY.Tflow,"WY")
WY.TPLoad.Q$inflow.FWM=with(WY.TPLoad.Q,(inflow.load/(inflow.Q*1.233e6))*1e9)
WY.TPLoad.Q$outflow.FWM=with(WY.TPLoad.Q,(outflow.load/(outflow.Q*1.233e6))*1e9)

WY.TNLoad.Q=merge(WY.TNLoad,WY.Tflow,"WY")
WY.TNLoad.Q$inflow.FWM=with(WY.TNLoad.Q,(inflow.load/(inflow.Q*1.233e6))*1e6)
WY.TNLoad.Q$outflow.FWM=with(WY.TNLoad.Q,(outflow.load/(outflow.Q*1.233e6))*1e6)


plot(inflow.FWM~WY,WY.TLoad.Q,type="l")
plot(outflow.FWM~WY,WY.TLoad.Q,type="l")

WY.TPLoad.Q$TPret=with(WY.TPLoad.Q,outflow.load-inflow.load)
plot(TPret~WY,WY.TPLoad.Q,type="l")

# Annual Trends -----------------------------------------------------------

Q.trend.in=with(WY.TPLoad.Q,cor.test(inflow.Q,WY,method="kendall"))
Q.trend.out=with(WY.TPLoad.Q,cor.test(outflow.Q,WY,method="kendall"))
Q.sen.in=zyp.sen(inflow.Q~WY,WY.TPLoad.Q)
Q.sen.out=zyp.sen(outflow.Q~WY,WY.TPLoad.Q)
Q.trend=data.frame(Parameter="discharge",
           Direction=c("Inflow","Outflow"),
           tau=c(Q.trend.in$estimate,Q.trend.out$estimate),
           pval=c(Q.trend.in$p.value,Q.trend.out$p.value),
           slope=c(as.numeric(coef(Q.sen.in)[2]),as.numeric(coef(Q.sen.out)[2])))

TPLoad.trend.in=with(WY.TPLoad.Q,cor.test(inflow.load,WY,method="kendall"))
TPLoad.trend.out=with(WY.TPLoad.Q,cor.test(outflow.load,WY,method="kendall"))
TPLoad.sen.in=zyp.sen(inflow.load~WY,WY.TPLoad.Q)
TPLoad.sen.out=zyp.sen(outflow.load~WY,WY.TPLoad.Q)
TPLoad.trend=data.frame(Parameter="TPLoad",
           Direction=c("Inflow","Outflow"),
           tau=c(TPLoad.trend.in$estimate,TPLoad.trend.out$estimate),
           pval=c(TPLoad.trend.in$p.value,TPLoad.trend.out$p.value),
           slope=c(as.numeric(coef(TPLoad.sen.in)[2]),as.numeric(coef(TPLoad.sen.out)[2])))

TPFWM.trend.in=with(WY.TPLoad.Q,cor.test(inflow.FWM,WY,method="kendall"))
TPFWM.trend.out=with(WY.TPLoad.Q,cor.test(outflow.FWM,WY,method="kendall"))
TPFWM.sen.in=zyp.sen(inflow.FWM~WY,WY.TPLoad.Q)
TPFWM.sen.out=zyp.sen(outflow.FWM~WY,WY.TPLoad.Q)
TPFWM.trend=data.frame(Parameter="TPFWM",
           Direction=c("Inflow","Outflow"),
           tau=c(TPFWM.trend.in$estimate,TPFWM.trend.out$estimate),
           pval=c(TPFWM.trend.in$p.value,TPFWM.trend.out$p.value),
           slope=c(as.numeric(coef(TPFWM.sen.in)[2]),as.numeric(coef(TPFWM.sen.out)[2])))


TNLoad.trend.in=with(WY.TNLoad.Q,cor.test(inflow.Q,WY,method="kendall"))
TNLoad.trend.out=with(WY.TNLoad.Q,cor.test(outflow.Q,WY,method="kendall"))
TNLoad.sen.in=zyp.sen(inflow.load~WY,WY.TNLoad.Q)
TNLoad.sen.out=zyp.sen(outflow.load~WY,WY.TNLoad.Q)
TNLoad.trend=data.frame(Parameter="TNLoad",
           Direction=c("Inflow","Outflow"),
           tau=c(TNLoad.trend.in$estimate,TNLoad.trend.out$estimate),
           pval=c(TNLoad.trend.in$p.value,TNLoad.trend.out$p.value),
           slope=c(as.numeric(coef(TNLoad.sen.in)[2]),as.numeric(coef(TNLoad.sen.out)[2])))


TNFWM.trend.in=with(WY.TNLoad.Q,cor.test(inflow.FWM,WY,method="kendall"))
TNFWM.trend.out=with(WY.TNLoad.Q,cor.test(outflow.FWM,WY,method="kendall"))
TNFWM.sen.in=zyp.sen(inflow.FWM~WY,WY.TNLoad.Q)
TNFWM.sen.out=zyp.sen(outflow.FWM~WY,WY.TNLoad.Q)
TNFWM.trend=data.frame(Parameter="TNFWM",
           Direction=c("Inflow","Outflow"),
           tau=c(TNFWM.trend.in$estimate,TNFWM.trend.out$estimate),
           pval=c(TNFWM.trend.in$p.value,TNFWM.trend.out$p.value),
           slope=c(as.numeric(coef(TNFWM.sen.in)[2]),as.numeric(coef(TNFWM.sen.out)[2])))
rslt=rbind(Q.trend,TPLoad.trend,TPFWM.trend,TNLoad.trend,TNFWM.trend)


# Tables
subset(rslt,Direction=="Inflow")[,c("Parameter","tau","pval","slope")]%>%
  flextable()%>%
  colformat_num(j=2,digits=2)%>%
  # colformat_num(j=4,digits=2)%>%
  align(j=2:4,align="center",part="all")%>%
  compose(j = 3, value = as_paragraph(ifelse(pval<0.05,"<0.05",round(pval,2))) )%>%
  compose(j = 4, value = as_paragraph(ifelse(slope<1,as.character(round(slope,3)),round(slope,0))) )%>%
  compose(j=1,value=as_paragraph(c("Discharge \u00B9","TP Load \u00B2","TP FWM \u00B3","TN Load \u00B2","TN FWM \u2074")))%>%
  set_header_labels(tau = "Kendall's\n\u03C4", pval = "\u03C1-value", slope="Thiel-Sen\nSlope")%>%
  font(fontname="Times New Roman",part="all")%>%
  bold(part="header")%>%fontsize(size=9)%>%
  width(width=c(0.9,0.9,0.7,0.8))%>%print(preview="pptx")

subset(rslt,Direction=="Outflow")[,c("Parameter","tau","pval","slope")]%>%
  flextable()%>%
  colformat_num(j=2,digits=2)%>%
  # colformat_num(j=4,digits=2)%>%
  align(j=2:4,align="center",part="all")%>%
  compose(j = 3, value = as_paragraph(ifelse(pval<0.05,"<0.05",format(round(pval,2),nsmall=2))) )%>%
  compose(j = 4, value = as_paragraph(ifelse(slope<1,as.character(round(slope,3)),round(slope,0))) )%>%
  compose(j=1,value=as_paragraph(c("Discharge","TP Load","TP FWM","TN Load","TN FWM")))%>%
  set_header_labels(tau = "Kendall's\n\u03C4", pval = "\u03C1-value", slope="Thiel-Sen\nSlope")%>%
  font(fontname="Times New Roman",part="all")%>%
  bold(part="header")%>%fontsize(size=9)%>%
  width(width=c(0.9,0.9,0.7,0.8))%>%
  footnote(i=1,j=1,value=as_paragraph("Units:"),ref_symbol=" ",part="header")%>%
  footnote(i=1,j=1,value=as_paragraph(" Ac-Ft WY\u207B\u00B9"),ref_symbol=" 1")%>%
  footnote(i=c(2,4),j=1,value=as_paragraph(" kg WY\u207B\u00B9"),ref_symbol=" 2")%>% 
  footnote(i=3,j=1,value=as_paragraph(" \u03BCg L\u207B\u00B9 WY\u207B\u00B9"),ref_symbol=" 3")%>%
  footnote(i=5,j=1,value=as_paragraph(" mg L\u207B\u00B9 WY\u207B\u00B9"),ref_symbol=" 4")%>%print(preview="pptx")

# change point?

# tiff(filename=paste0(plot.path,"LakeO_Load.tiff"),width=6.5,height=5,units="in",res=200,type="windows",bg="white",compression=c("lzw"))
# png(filename=paste0(plot.path,"LakeO_Load.png"),width=6.5,height=5,units="in",res=200,type="windows",bg="white")
layout(matrix(1:6,3,2))
par(family="serif",mar=c(1,2.25,0.25,2.5),oma=c(3,2.5,0.75,0.1));
xlim.val=c(1979,2020);by.x=5;xmaj=seq(xlim.val[1],xlim.val[2],by.x);xmin=seq(xlim.val[1],xlim.val[2],by.x/by.x)

ylim.val=c(0,50e5);by.y=10e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.Q~WY,WY.TPLoad.Q,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(WY.TPLoad.Q,pt_line(WY,inflow.Q,1,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.lwd=0.1,cex=1.25))
with(WY.TPLoad.Q,pt_line(WY,outflow.Q,1,adjustcolor("dodgerblue1",0.5),1,21,adjustcolor("dodgerblue1",0.5),pt.lwd=0.1,cex=1.25))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,ymaj/10e4);box(lwd=1)
mtext(side=2,line=2.25,"Discharge\n(x10\u2074 Ac-Ft WY\u207B\u00B9)",cex=0.75)
legend("topleft",legend=c("Inflow","Outflow"),
       pch=c(21,21),
       lty=c(NA),lwd=c(0.1,0.1),
       col="black",
       pt.bg=adjustcolor(c("indianred1","dodgerblue1"),0.5),
       pt.cex=1.5,ncol=2,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0,yjust=0.5)


ylim.val=c(0,12e5);by.y=2e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.load~WY,WY.TPLoad.Q,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(WY.TPLoad.Q,pt_line(WY,inflow.load,1,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.lwd=0.1,cex=1.25))
with(WY.TPLoad.Q,pt_line(WY,outflow.load,1,adjustcolor("dodgerblue1",0.5),1,21,adjustcolor("dodgerblue1",0.5),pt.lwd=0.1,cex=1.25))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,ymaj/10e4);box(lwd=1)
mtext(side=2,line=2.25,"TP Load\n(x10\u2074 kg WY\u207B\u00B9)",cex=0.75)

ylim.val=c(0,400);by.y=100;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.FWM~WY,WY.TPLoad.Q,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(WY.TPLoad.Q,pt_line(WY,inflow.FWM,1,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.lwd=0.1,cex=1.25))
with(WY.TPLoad.Q,pt_line(WY,outflow.FWM,1,adjustcolor("dodgerblue1",0.5),1,21,adjustcolor("dodgerblue1",0.5),pt.lwd=0.1,cex=1.25))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2.25,"TP FWM\n(\u03BCg L\u207B\u00B9)",cex=0.75)
mtext(side=1,line=3,"Water Year\n(May - April)",cex=0.90)

# empty plot
ylim.val=c(0,50e5);by.y=10e5;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.Q~WY,WY.TPLoad.Q,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)

ylim.val=c(0,1.5e7);by.y=0.5e7;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.load~WY,WY.TNLoad.Q,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(WY.TNLoad.Q,pt_line(WY,inflow.load,1,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.lwd=0.1,cex=1.25))
with(WY.TNLoad.Q,pt_line(WY,outflow.load,1,adjustcolor("dodgerblue1",0.5),1,21,adjustcolor("dodgerblue1",0.5),pt.lwd=0.1,cex=1.25))
axis_fun(1,xmaj,xmin,NA)
axis_fun(2,ymaj,ymin,format(ymaj/10e6));box(lwd=1)
mtext(side=2,line=2,"TN Load\n(x10\u2076 kg WY\u207B\u00B9)",cex=0.75)

ylim.val=c(0,5);by.y=2;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)
plot(inflow.FWM~WY,WY.TNLoad.Q,type="n",xlim=xlim.val,ylim=ylim.val,ann=F,axes=F)
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
with(WY.TNLoad.Q,pt_line(WY,inflow.FWM,1,adjustcolor("indianred1",0.5),1,21,adjustcolor("indianred1",0.5),pt.lwd=0.1,cex=1.25))
with(WY.TNLoad.Q,pt_line(WY,outflow.FWM,1,adjustcolor("dodgerblue1",0.5),1,21,adjustcolor("dodgerblue1",0.5),pt.lwd=0.1,cex=1.25))
axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
mtext(side=2,line=2,"TN FWM\n(mg L\u207B\u00B9)",cex=0.75)
mtext(side=1,line=3,"Water Year\n(May - April)",cex=0.90)
dev.off()


# Monthly Load ------------------------------------------------------------

mon.TLoad=ddply(load.mon.sum,c("CY","month","WY"),summarise,inflow.load=sum(Inflow,na.rm=T),outflow.load=sum(Outflow,na.rm=T))
mon.Tflow=ddply(flow.mon.sum,c("CY","month","WY"),summarise,inflow.Q=sum(cfs.to.acftd(Inflow),na.rm=T),outflow.Q=sum(cfs.to.acftd(Outflow),na.rm=T))

mon.TLoad.Q=merge(mon.TLoad,mon.Tflow,c("CY","month","WY"))
mon.TLoad.Q$inflow.FWM=with(mon.TLoad.Q,(inflow.load/(inflow.Q*1.233e6))*1e9)
mon.TLoad.Q$outflow.FWM=with(mon.TLoad.Q,(outflow.load/(outflow.Q*1.233e6))*1e9)

plot(mon.TLoad.Q$inflow.FWM)
plot(mon.TLoad.Q$outflow.FWM)

# Experimentation with GAM ------------------------------------------------


library(mgcv)
library(gratia)

mod=gam(log(inflow.load)~
          s(month,bs="bs",k=12)+
          s(WY,k=40)+
          ti(month,WY,bs=c('bs','tp')),
        data=mon.TLoad)
nvar=3;layout(matrix(1:nvar,1,nvar))
plot(mod,residuals=T,pch=21)
summary(mod)

nvar=4;layout(matrix(1:nvar,1,nvar))
gam.check(mod,pch=21)

shapiro.test(residuals(mod))

draw(mod)
# draw(penalty(mod))
