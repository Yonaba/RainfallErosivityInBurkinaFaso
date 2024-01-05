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
  #yr <- "2019"
  print(paste0("Processing : ", yr))
  
  ncs <- list.files(paste0("data/",product,"/",yr), pattern = "*.nc4")
  df <- data.frame(matrix(nrow=0, ncol=nrow(stations)+1))
  colnames(df) <- c("datetime",stations$Name)
  
  for (i in 1:length(ncs)) {
    #i <- 1500
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
      
      vals[,s] <- (vv / 0.5)
    }
    df <- rbind(df, vals)  
  }
  
  ddf <- df
  ddf$datetime <- as.character(as.POSIXct(ddf$datetime,format="%Y-%m-%d %H:%M",tz="UTC"))
  ddf$datetime <- as.character(ifelse(nchar(ddf$datetime) == 10,paste(ddf$datetime, "00:00:00"), ddf$datetime))
  
  tseq30 <- as.character(seq(as.POSIXct(paste0(yr,"-01-01 00:00:00")),
                             as.POSIXct(paste0(yr,"-12-31 00:00:00")), 
                             by="30 min"))
  tseq30 <- ifelse(nchar(tseq30) == 10,paste(tseq30, "00:00:00"),tseq30)
  tseq30 <- data.frame(datetime = tseq30)
  
  ddf <- merge(tseq30, ddf, by = "datetime", all = T)
  ddf[is.na(ddf)] <- 0
  
  #Reads back using 'as.POSIXct(xx,format="%Y-%m-%d %H:%M:%S",tz="UTC")'
  # ddf$datetime <- ifelse(nchar(ddf$datetime) == 10,
  #                        paste(ddf$datetime, "00:00:00"),
  #                        ddf$datetime)
  
  write.csv(ddf, file = paste0("processing/_tmp30_/",product,"_",yr,"_pr_bf_30mn.csv"), row.names = F)
}

#bind all files
csvs <- paste0("processing/_tmp30_/",list.files(paste0("processing/_tmp30_/"), pattern = "_pr_bf_30mn.csv"))
df <- csvs %>% lapply(read.csv) %>% bind_rows 

tseq30 <- as.character(seq(as.POSIXct(paste0("2001-01-01 00:00:00")),
                           as.POSIXct(paste0("2020-12-31 23:30:00")), 
                           by="30 min"))

tseq30 <- ifelse(nchar(tseq30) == 10,paste(tseq30, "00:00:00"),tseq30)
tseq30 <- data.frame(datetime = tseq30)

ddf <- merge(tseq30, df, by = "datetime", all = T)
ddf[is.na(ddf)] <- 0

write.csv(ddf, file = paste0("processing/0_30mn/",product,"_pr_bf_30mn.csv"), row.names = F)
print("finished")

