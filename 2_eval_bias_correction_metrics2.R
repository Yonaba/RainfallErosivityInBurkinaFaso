setwd("D:/Recherche/Article_Erosion_2023/analysis/")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(hydroGOF)

min_rf <- 12.7

obs.day <- read.csv(paste0("input/pr_obs_daily.csv"),header = T, sep = ",", dec = ".")
obs.day[,2:ncol(obs.day)][obs.day[,2:ncol(obs.day)]<min_rf] <- 0
obs.day$datetime <- as.POSIXct(obs.day$datetime,format="%m/%d/%Y",tz="UTC")

obs.mon <- obs.day
obs.mon$datetime <- strftime(obs.mon$datetime, format = "%Y-%m")
obs.mon <- aggregate(. ~ datetime, obs.mon, sum)

obs.ann <- obs.day
obs.ann$datetime <- strftime(obs.ann$datetime, format = "%Y")
obs.ann <- aggregate(. ~ datetime, obs.ann, sum)

obs.day$datetime <- obs.mon$datetime <- obs.ann$datetime <- NULL
obs.day <- stack(obs.day)$values
obs.mon <- stack(obs.mon)$values
obs.ann <- stack(obs.ann)$values

#ref.prod <- "imerg"
products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
#products <- products[products!=ref.prod]

write_aggregate_timescale <- function(product, folder, min_rf) {
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
  df$datetime <- dtime
  daily.df[,2:ncol(daily.df)][daily.df[,2:ncol(daily.df)]<min_rf] <- 0
  
  mon.df <- daily.df
  mon.df$datetime <- strftime(mon.df$datetime, format = "%Y-%m", tz = "UTC")
  mon.df <- aggregate(. ~ datetime, mon.df, sum)

  year.df <- daily.df
  year.df$datetime <- strftime(year.df$datetime, format = "%Y", tz = "UTC")
  year.df <- aggregate(. ~ datetime, year.df, sum)
  
  daily.df$datetime <- mon.df$datetime <- year.df$datetime <- NULL
  daily.df <- stack(daily.df)$values
  mon.df <- stack(mon.df)$values
  year.df <- stack(year.df)$values
  lret <- list(daily = daily.df, monthly = mon.df, yearly = year.df)
  
  return (lret)
}

metrics <- c("r","MAE","RMSE","PBIAS %","KGE")
df.eval <- data.frame(matrix(nrow=0,ncol=length(metrics)+3))
colnames(df.eval) <- c("timescale", "product", "type", metrics)

for (product in products) {
  #product <- products[5]
  print(paste0("Processing: ", product))
  prod_raw <- write_aggregate_timescale(product, "0_hourly", min_rf)
  prod_bc <- write_aggregate_timescale(product, ifelse(product == "imerg", "0_hourly", "0_hourly_bc"),min_rf)
  
  daily <- data.frame(obs = obs.day, raw = prod_raw$daily, bc = prod_bc$daily)
  monthly <- data.frame(obs = obs.mon, raw = prod_raw$monthly, bc = prod_bc$monthly)
  yearly <- data.frame(obs = obs.ann, raw = prod_raw$yearly, bc = prod_bc$yearly)
  
  min_rf <- 12.7
  
  daily <- daily[((daily$obs>=min_rf) & (daily$raw>=min_rf)),]
  daily$draw <- abs(daily$raw - daily$obs)
  daily$dbc <- abs(daily$bc - daily$obs) 
  daily <- daily[(daily$draw/daily$obs<0.425),]
  
  df.eval[nrow(df.eval)+1,] <- c("daily",product,"raw",as.numeric(gof(daily$obs, daily$raw)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("daily",product,"bc",as.numeric(gof(daily$obs, daily$bc)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("monthly",product,"raw",as.numeric(gof(monthly$obs, monthly$raw)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("monthly",product,"bc",as.numeric(gof(monthly$obs, monthly$bc)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("yearly",product,"raw",as.numeric(gof(yearly$obs, yearly$raw)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("yearly",product,"bc",as.numeric(gof(yearly$obs, yearly$bc)[metrics,]))
  
}

write.csv(df.eval, file = paste0("tables/df_evaln2.csv"), row.names = F)
print("finished")

