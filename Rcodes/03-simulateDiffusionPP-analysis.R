#This script is to be run after the "02-simulateDiffusionPP-year.R" script

#########################################
## VISUALISATION OF SIMULATION RESULTS ##
#########################################

#call FFMPEG to create videos (supposed to have FFMPEG installed on your system):
for(year in 2015:2019){
   for(sim in 1:1){
      pth=paste0("res/year",year,"/sim",sim,"/")
      owd<-setwd(pth)
      cmd=paste0("ffmpeg -y -framerate 5 -i simPP-%03d.png -s 960x800 -c:v libx264 -pix_fmt yuv420p vidyear",year,"sim",sim,".avi" )
      system(cmd)
      setwd(owd)
      file.copy(paste0(pth,"vidyear",year,"sim",sim,".avi"),paste0("res/vidyear",year,"sim",sim,".avi"),overwrite = TRUE)
   }
}

#call ImageMagick to create gifs (supposed to have ImageMagick installed on your system):
for(year in 2015:2019){
   for(sim in 1:1){
      pth=paste0("res/year",year,"/sim",sim,"/")
      owd<-setwd(pth)
      cmd=paste0("magick convert -delay 20 -crop 1310x900+60+100 *.png +repage vidyear",year,"sim",sim,".gif" )
      system(cmd)
      setwd(owd)
      file.copy(paste0(pth,"vidyear",year,"sim",sim,".gif"),paste0("res/vidyear",year,"sim",sim,".gif"),overwrite = TRUE)
      
   }
}

#################################################
##   SYNTHETIZE SIMULATION RESULTS (5 YEARS)   ##
#################################################
library(ggplot2)
library(lubridate)
#load results from each simulated year:
allres=NULL
for(year in 2015:2019){
   load(paste0("simulateDiffusionPP-",year,".RData"))
   res=results
   colnames(res)=paste0(year,".",1:3)
   allres=cbind(allres,res)
}
allres=as.data.frame(allres)
allres$dates=seq(ymd(paste(year, 3, 21, sep="-")),ymd(paste(year, 6, 30, sep="-")), by="1 day")

forplot=data.frame(dates=allres$dates,mean2015=apply(allres[,1:3],1,mean),min2015=apply(allres[,1:3],1,min),max2015=apply(allres[,1:3],1,max),mean2016=apply(allres[,4:6],1,mean),min2016=apply(allres[,4:6],1,min),max2016=apply(allres[,4:6],1,max),
mean2017=apply(allres[,7:9],1,mean),min2017=apply(allres[,7:9],1,min),max2017=apply(allres[,7:9],1,max),mean2018=apply(allres[,10:12],1,mean),min2018=apply(allres[,10:12],1,min),max2018=apply(allres[,10:12],1,max),mean2019=apply(allres[,13:15],1,mean),min2019=apply(allres[,13:15],1,min),max2019=apply(allres[,13:15],1,max))

#Figure 2
ggplot(forplot,aes(dates,mean2015,col="2015")) +  
theme_bw() +  
geom_line(size=1) + geom_line(aes(y=min2015,col="minmax"),linetype=2,colour="gray") + geom_line(aes(y=max2015,col="minmax"),linetype=2,colour="gray") +
scale_x_date(date_labels = "%d-%b") + 
xlab("") + ylab("# particules <25°E & >10°N") +  labs(colour = "Données de vents") +
geom_line(aes(dates,mean2016,col="2016"),size=1) +  
geom_line(aes(y=min2016,col="minmax"),linetype=2,colour="gray") + 
geom_line(aes(y=max2016,col="minmax"),linetype=2,colour="gray") + 
geom_line(aes(dates,mean2017,col="2017"),size=1) +  
geom_line(aes(y=min2017,col="minmax"),linetype=2,colour="gray") + 
geom_line(aes(y=max2017,col="minmax"),linetype=2,colour="gray") + 
geom_line(aes(dates,mean2018,col="2018"),size=1) +  
geom_line(aes(y=min2018,col="minmax"),linetype=2,colour="gray") + 
geom_line(aes(y=max2018,col="minmax"),linetype=2,colour="gray") +
geom_line(aes(dates,mean2019,col="2019"),size=1) +  
geom_line(aes(y=min2019,col="minmax"),linetype=2,colour="gray") + 
geom_line(aes(y=max2019,col="minmax"),linetype=2,colour="gray") 


