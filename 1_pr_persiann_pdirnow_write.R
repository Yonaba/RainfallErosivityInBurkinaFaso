setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(ncdf4)
library(stringr)
library(lubridate)

product <- "persiann"

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")
rownames(stations) <- stations$Name

df <- data.frame(matrix(nrow=0, ncol=nrow(stations)+1))
colnames(df) <- c("datetime",stations$Name)

ncs <- list.files(paste0("data/",product,"/"), pattern = "*.nc")

for (i in 1:length(ncs)) {
  #i <- 200
  ncname <- ncs[i]
  print(paste0("Processing ", ncname," (",i, "/",length(ncs),")"))
  
  nc <- nc_open(paste0("data/",product,"/",ncname))
  mvar <- "precip"
  var <- ncvar_get(nc,mvar)
  
  ncorigin <- str_match(ncname, "(\\d+).nc")[2]
  yr <- str_sub(ncorigin,1,4)
  mon <- str_sub(ncorigin,-2,-1)
  nc.origin <- as.Date(paste0(yr,"-",mon,"-","01"))  
  dtime <- nc.origin + hours(ncvar_get(nc,"datetime"))
  
  lon <- nc$dim$lon$vals
  lat <- nc$dim$lat$vals
  nc_close(nc)
  
  vals <- data.frame(matrix(nrow=dim(var)[3], ncol=nrow(stations)+1))
  colnames(vals) <- c("datetime",stations$Name)
  vals[,"datetime"] <- dtime
  
  for (s in stations$Name) {
    #s <- "OUAGADOUGOU"
    mlon <- stations[s,"Longitude"]
    mlat <- stations[s,"Latitude"]
    
    lon_idx <- which.min(abs(lon-mlon))
    lat_idx <- which.min(abs(lat-mlat))
    
    vv <- var[lon_idx, lat_idx,]
    vv[vv < 0] <- NA
    
    vals[,s] <- vv
  }
  df <- rbind(df, vals)  
}

df$datetime <- as.character(df$datetime)

#Reads back using 'as.POSIXct(xx,format="%Y-%m-%d %H:%M:%S",tz="UTC")'
df$datetime <- ifelse(nchar(df$datetime) == 10, paste(df$datetime, "00:00:00"), df$datetime)

tseq <- as.character(seq(as.POSIXct("2001-01-01 00:00:00"), 
                         as.POSIXct("2020-12-31 23:00:00"), 
                         by="hour"))

tseq <- ifelse(nchar(tseq) == 10, paste(tseq, "00:00:00"), tseq)
tseq <- data.frame(datetime = tseq)
df <- merge(tseq, df, by = "datetime", all = T)
df[is.na(df)] <- 0
df[,2:ncol(df)][df[,2:ncol(df)] > 253] <- 0

write.csv(df, file = paste0("processing/0_hourly/",product,"_pr_bf_hourly.csv"), row.names = F)
print("finished")

