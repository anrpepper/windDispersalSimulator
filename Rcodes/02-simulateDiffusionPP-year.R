#This script is to be run after the "01-Download-rWind-WIND.R" script and supposing that the wind files are stored in a F:/ drive, otherwise change ddroot value (line 13)

#which year of data to run?
year = 2015

#eventually, this script can be parallelized uncommenting the 2 following lines:
#args <- commandArgs(TRUE)
#year=args[1]
#and using a batch command of the style (for e.g. for 2015):
#R  CMD BATCH --vanilla "--args 2015" 02-simulateDiffusionPP-year.R  PP2015.Rout 

#on which drive are the wind data stored?
ddroot="F:/"

library(raster)
library(rWind)
library(lubridate)
library(ggplot2)
library(maps)
library(spatstat)
library(rgdal)

#raster options:
rasterOptions(tmpdir=paste0(ddroot,"tmpR/"), tmptime=24)

#general stuffs:
myO=owin(xrange=c(-25,45),yrange=c(0,40))
partMonthStart=c(1,11,21)
lastday=c(31,28,31,30,31,30,31,31,30,31,30,31)

#store results in:
dir.create(paste0("res/year",year))

#Lets consider we have one year conditions and do the simulations until end of June:
results<-NULL  
for(sim in 1:3){
  arrivedChad=NULL
  month=3
  partOfMonth=3
  #initialize "particules" in Kenya, Somalia and Ethiopia in 3rd decade of March:
  initpp=ppp(x=runif(10000,35,45),y=runif(10000,0,10),window=myO)
  dir.create(paste0("res/year",year,"/sim",sim))
  png(paste0("res/year",year,"/sim",sim,"/simPP-000.png"),width=9.6,height=8,units="in",res=150)
  map("world",xlim=c(-25,45),ylim=c(0,40),main="")
  plot(initpp,pch=".",add=T,col=2)
  plot(myO,add=T,lwd=4)
  title("Initialization")
  dev.off()
  npp=initpp
  jj=1
  for(ii in 1:10){
    partMonthEnd=c(10,20,lastday[month])
    for(day in  partMonthStart[partOfMonth]:partMonthEnd[partOfMonth]){
      hours=seq(ymd_hms(paste(year, month, day, 09, 00, 00, sep="-")),
                       ymd_hms(paste(year, month, day, 18, 00, 00, sep="-")), by="3 hours")
      
      selfilesDir=paste0(ddroot,"WIND/DIR/WindDIR.", format(hours, "%Y%m%d%H"), ".tif")
      selfilesSpeed=paste0(ddroot,"WIND/SPD/WindSPD.", format(hours, "%Y%m%d%H"), ".tif")
      for(h in 1:4){
        rAvD<-raster(selfilesDir[h])
        rAvS<-raster(selfilesSpeed[h])
        res=ds2uv(values(rAvD),values(rAvS))
        um=res[,1]
        vm=res[,2]
        d=data.frame(coordinates(rAvD),u=um,v=vm)
        for(i in 1:npp$n){
           wi=d[which(abs(round(npp[i]$x,1)-d$x)<=0.25 & abs(round(npp[i]$y,1)-d$y)<=0.25),3:4]
           xfact= 111 - (npp[i]$y * (111-85)/40)    #85km at 40°N, 111km at 0°N (always 111km/° for latitude)
           shiftv=wi*3*3600*0.5 /(c(xfact,111)*1000)   #computation of actual distance covered in 3h in degree (with transformation)
           npp[i]=shift(npp[i],unlist(shiftv))
        }
        npp=ppp(npp$x,npp$y,window=myO)#reject the ones outside
      }
      #need to check if entered west of 23°E and north of 13°N (repro area of Tchad)
      arrivedChad= c(arrivedChad, Narr<-sum(npp$x < 23 & npp$y>13) )
      png(paste0("res/year",year,"/sim",sim,"/simPP-",ifelse(jj<=9,paste0("00",jj),ifelse(jj<=99,paste0("0",jj),jj),".png"),width=9.6,height=8,units="in",res=150)
      map("world",xlim=c(-25,45),ylim=c(0,40),main="")
      plot(npp,pch=".",add=T,col=2)
      plot(myO,add=T,lwd=4)
      title(paste(format(hours[1],"%Y%m%d"),"arrived in Chad:",Narr/100,"%"))    
      dev.off()
      print(format(hours[1],"%Y%m%d"))
      jj=jj+1
    }
    if(partOfMonth == 3){
      partOfMonth=1
      month=month+1
      if(month==13){month=1}
    }else{
      partOfMonth=partOfMonth + 1 
    }
  }
  results=cbind(results,arrivedChad)
}
save.image(file=paste0("simulateDiffusionPP-",year,".RData"))

