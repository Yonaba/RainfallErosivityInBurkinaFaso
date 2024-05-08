setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(modifiedmk)
library(patchwork)
library(ggpubr)

write_aggregate_timescale <- function(product, folder) {
  #product<- "persiann"
  #folder <- "0_hourly"
  #fend <- ifelse(folder=="0_hourly_bc","_pr_bf_hourly12.7.csv","_pr_bf_hourly.csv")
  fend <- "_pr_bf_hourly.csv"
  df <- read.csv(paste0("processing/",folder,"/",product,fend),header = T, sep = ",", dec = ".")
  df$datetime <- dtime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
  
  df$datetime <- as.Date(df$datetime, format = "%Y-%m-%d")
  daily.df <- aggregate(. ~ datetime, df, sum)
  tdaily <- data.frame(datetime = seq(as.POSIXct("2001-01-01"), as.POSIXct("2020-12-31"), by="day"))
  daily.df <- merge(tdaily, daily.df, by = "datetime", all = T)
  #daily.df[,2:ncol(daily.df)][daily.df[,2:ncol(daily.df)]<min_rf] <- 0
  
  year.df <- daily.df
  year.df$datetime <- strftime(year.df$datetime, format = "%Y", tz = "UTC")
  year.df <- aggregate(. ~ datetime, year.df, sum)
  year.df$datetime <- NULL
  return (year.df)
}

read_aggregate_timescale_rfactor <- function(product, type) {
  #type <- "raw"
  df <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_",type,".csv"),header = T, sep = ",", dec = ".")
  Ragg <- aggregate(R~Year + Station, df, sum)
  df.out <- data.frame(matrix(nrow = 20, ncol=length(unique(Ragg$Station))))
  stations <- unique(Ragg$Station)
  colnames(df.out) <- stations
  for (station in stations) {
    #print(paste0("station ", station))
    cc <- Ragg[Ragg$Station == station,]
    ccd <- data.frame(Year = 2001:2020)
    ccd <- merge(ccd, cc, by="Year", all.x=T)
    ccd[is.na(ccd$R),] <- 0
    df.out[,station] <- ccd$R
  }
  return (df.out)
}

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

obs.day <- read.csv(paste0("input/pr_obs_daily.csv"),header = T, sep = ",", dec = ".")
#obs.day[,2:ncol(obs.day)][obs.day[,2:ncol(obs.day)]<min_rf] <- 0
obs.day$datetime <- as.POSIXct(obs.day$datetime,format="%m/%d/%Y",tz="UTC")

obs.ann <- obs.day
obs.ann$datetime <- strftime(obs.ann$datetime, format = "%Y")
obs.ann <- aggregate(. ~ datetime, obs.ann, sum)
obs.ann$datetime <- NULL

cdf <- data.frame(matrix(nrow=0, ncol=12))
colnames(cdf) <- c("Variable","Product",stations)
mks <- c()
for (station in stations) {
  pann.obs.mk <- tfpwmk(obs.ann[,station])[c(2,4)]
  pann.obs.mk <- paste0(sprintf("%.1f",pann.obs.mk[1])," (",sprintf("%.3f",pann.obs.mk[2]),")")  
  mks <- c(mks, pann.obs.mk)
}
cdf[nrow(cdf)+1,] <- c("Rainfall","Observed",mks)

for (product in products) {
  #product <- products[3]
  print(paste0("Processing Rainfall: ",product))
  pann.raw <- write_aggregate_timescale(product, "0_hourly")
  pann.bc <- write_aggregate_timescale(product, "0_hourly_bc")
  
  mks1 <- mks2 <- c()
  for (station in stations) {
    #station <- stations[1]
    pann.raw.mk <- tfpwmk(pann.raw[,station])[c(2,4)]
    pann.bc.mk <- tfpwmk(pann.bc[,station])[c(2,4)]

    pann.raw.mk <- paste0(sprintf("%.1f",pann.raw.mk[1])," (",sprintf("%.3f",pann.raw.mk[2]),")")
    pann.bc.mk <- paste0(sprintf("%.1f",pann.bc.mk[1])," (",sprintf("%.3f",pann.bc.mk[2]),")")
    
    mks1 <- c(mks1, pann.raw.mk)
    mks2 <- c(mks2, pann.bc.mk)
  }
  cdf[nrow(cdf)+1,] <- c("Rainfall",paste0(product, " (raw)"),mks1)
  cdf[nrow(cdf)+1,] <- c("Rainfall",paste0(product, " (bc)"),mks2)
}

for (product in products) {
  #product <- products[5]
  print(paste0("Processing R-factor: ",product))
  rann.raw <- read_aggregate_timescale_rfactor(product, "raw")
  rann.bc <- read_aggregate_timescale_rfactor(product, "bc")
  
  mks1 <- mks2 <- c()
  for (station in stations) {
    #station <- stations[1]
    
    rann.raw.mk <- tfpwmk(rann.raw[,station])[c(2,4)]
    rann.bc.mk <- tfpwmk(rann.bc[,station])[c(2,4)]
    
    rann.raw.mk <- paste0(sprintf("%.1f",rann.raw.mk[1])," (",sprintf("%.3f",rann.raw.mk[2]),")")
    rann.bc.mk <- paste0(sprintf("%.1f",rann.bc.mk[1])," (",sprintf("%.3f",rann.bc.mk[2]),")")
    mks1 <- c(mks1, rann.raw.mk)
    mks2 <- c(mks2, rann.bc.mk)
  }
  cdf[nrow(cdf)+1,] <- c("R-factor",paste0(product, " (raw)"),mks1)
  cdf[nrow(cdf)+1,] <- c("R-factor",paste0(product, " (bc)"),mks2)
}

write.csv(cdf, file = paste0("tables/mk_trends_pann_rann.csv"), row.names = F)

print("Finished.")
