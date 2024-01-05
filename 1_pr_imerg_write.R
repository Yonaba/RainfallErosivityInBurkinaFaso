setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(ncdf4)
library(stringr)
library(dplyr)
library(lubridate)

product <- "imerg"

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")
rownames(stations) <- stations$Name

years <- list.files(paste0("data/",product,"/"))
#years <- c("2019")

for (yr in years) {
  #yr <- "2018"
  print(paste0("Processing : ", yr))
  
  ncs <- list.files(paste0("data/",product,"/",yr), pattern = "*.nc4")
  df <- data.frame(matrix(nrow=0, ncol=nrow(stations)+1))
  colnames(df) <- c("datetime",stations$Name)
  
  for (i in 1:length(ncs)) {
    #i <- 2500*2
    ncname <- ncs[i]
    print(paste0("Processing ", ncname," (",i, "/",length(ncs),")"))
    
    nc <- nc_open(paste0("data/",product,"/",yr,"/",ncname))
    mvar <- "precipitationCal"
    var <- ncvar_get(nc,mvar)
    
    dtime <- as.POSIXct("1970-01-01 00:00:00") + 
      seconds(ncvar_get(nc,"time"))
    
    lon <- nc$dim$lon$vals
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
      #print(paste0(s, " ", lon_idx, " ",lat_idx))
      
      vv <- var[lat_idx, lon_idx]
      vv[vv < 0] <- NA
      
      vals[,s] <- vv
    }
    df <- rbind(df, vals)  
  }
  
  dtime <- as.character(df$datetime)
  dtime <- as.character(as.POSIXct(dtime, format="%Y-%m-%d %H",tz="UTC"))
  dtime <- ifelse(is.na(dtime), as.character(df$datetime),dtime)
  dtime <- as.character(ifelse(nchar(dtime) == 10,paste(dtime, "00:00:00"), dtime))  
  
  ddf <- df
  ddf$datetime <- dtime
  ddf <- aggregate(.~datetime, ddf, sum)
  
  tseq <- as.character(seq(as.POSIXct(paste0(yr,"-01-01 00:00:00")),
                           as.POSIXct(paste0(yr,"-12-31 23:00:00")), 
                           by="hour"))
  
  tseq <- ifelse(nchar(tseq) == 10,paste(tseq, "00:00:00"),tseq)
  tseq <- data.frame(datetime = tseq)
  ddf <- merge(tseq, ddf, by = "datetime", all = T)
  ddf[is.na(ddf)] <- 0
  
  write.csv(ddf, file = paste0("processing/_tmphour_/",product,"_",yr,"_pr_bf_hourly.csv"), row.names = F)
}

#bind all files
csvs <- paste0("processing/_tmphour_/",list.files(paste0("processing/_tmphour_/"), pattern = "_hourly.csv"))
df <- csvs %>% lapply(read.csv) %>% bind_rows 
write.csv(df, file = paste0("processing/0_hourly/",product,"_pr_bf_hourly.csv"), row.names = F)

print("finished")

