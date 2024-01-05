setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(ncdf4)
library(stringr)
library(lubridate)

product <- "cmorph"

to180 <- function(coord) {
  icoord <- coord > 180
  coord[icoord] <- coord[icoord]-360
  coord
}

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")
rownames(stations) <- stations$Name

df <- data.frame(matrix(nrow=0, ncol=nrow(stations)+1))
colnames(df) <- c("datetime",stations$Name)

ncs <- list.files(paste0("data/",product,"/"), pattern = "*.nc", recursive = T)

for (i in 1:length(ncs)) {
  #i <- 4393
  ncname <- ncs[i]
  print(paste0("Processing ", ncname," (",i, "/",length(ncs),")"))
  
  nc <- nc_open(paste0("data/",product,"/",ncname))
  mvar <- "cmorph"
  var <- ncvar_get(nc,mvar)
  
  dtime <- as.POSIXct("1970-01-01 00:00:00") + 
    seconds(ncvar_get(nc,"time"))
  
  lon <- to180(nc$dim$lon$vals)
  lat <- nc$dim$lat$vals
  nc_close(nc)
  
  vals <- data.frame(matrix(nrow=1, ncol=nrow(stations)+1))
  colnames(vals) <- c("datetime",stations$Name)
  vals[,"datetime"] <- dtime
  
  for (s in stations$Name) {
    #s <- "OUAGADOUGOU"
    mlon <- stations[s,"Longitude"]
    mlat <- stations[s,"Latitude"]
    
    lon_idx <- which.min(abs(lon-mlon))
    lat_idx <- which.min(abs(lat-mlat))
    
    vv <- var[lon_idx, lat_idx]
    vv[vv < 0] <- NA
    
    vals[,s] <- vv
  }
  df <- rbind(df, vals)  
}

df$datetime <- as.character(df$datetime)

#Reads back using 'as.POSIXct(xx,format="%Y-%m-%d %H:%M:%S",tz="UTC")'
df$datetime <- ifelse(nchar(df$datetime) == 10,paste(df$datetime, "00:00:00"),df$datetime)

tseq <- as.character(seq(as.POSIXct("2001-01-01 00:00:00"), 
                         as.POSIXct("2020-12-31 23:00:00"), 
                         by="hour"))

tseq <- ifelse(nchar(tseq) == 10, paste(tseq, "00:00:00"),tseq)
tseq <- data.frame(datetime = tseq)
df <- merge(tseq, df, by = "datetime", all = T)
df[is.na(df)] <- 0

write.csv(df, file = paste0("processing/0_hourly/",product,"_pr_bf_hourly.csv"), row.names = F)
print("finished")

