#Script to download the wind data (take time!! and good internet connection)
library(rWind)
library(lubridate)
library(fields)
library(shape)
library(raster)
ddroot="F:/"  #change your drive or place of storage of the winds here (but then also on script 02-...)
rasterOptions(tmpdir=paste0(ddroot,"tmpR/"), tmptime=24)
# Choice of the start date here (année, mois, jour, heures, minutes, secondes)
dts <- seq(ymd_hms(paste(2015, 01, 01, 00, 00, 00, sep="-")),
           ymd_hms(paste(2015, 01, 01, 21, 00, 00, sep="-")), by="3 hours")
# Choice of the number of days to download
nbdays=365*5+1   #+1 for leap year 2016
for(ds in 0:nbdays){
        date = dts + (3600*24*ds)      
        ww <- wind.dl_2(date, -25, 45, 0, 40)
        wind_layer <- wind2raster(ww)
        for(hours in 1:8){
                  # hours = 1 > 00:00, hours = 2 > 03:00, hours = 3 > 06:00, hours = 4 > 09:00
                  # hours = 5 > 12:00, hours = 6 > 15:00, hours = 7 > 18:00, hours = 8 > 21:00
                  writeRaster(wind_layer[[hours]][[1]], file=paste0(ddroot,"WIND/DIR/WindDIR.", format(date[hours], "%Y%m%d%H"), ".tif"))
                  writeRaster(wind_layer[[hours]][[2]], file=paste0(ddroot,"WIND/SPD/WindSPD.", format(date[hours], "%Y%m%d%H"), ".tif"))
        }
}
