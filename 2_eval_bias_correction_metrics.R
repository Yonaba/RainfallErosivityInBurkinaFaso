setwd("D:/Recherche/Article_Erosion_2023/analysis/")
Sys.setenv(TZ = "UTC")

library(zoo)
library(lubridate)
library(hydroGOF)

products <- list.files(path="processing/0_hourly_bc")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

obs.day <- read.csv(paste0("input/pr_obs_daily.csv"),header = T, sep = ",", dec = ".")
obs.mon <- read.csv(paste0("input/pr_obs_monthly.csv"),header = T, sep = ",", dec = ".")
obs.ann <- read.csv(paste0("input/pr_obs_yearly.csv"),header = T, sep = ",", dec = ".")

obs.day$datetime <- obs.mon$datetime <- obs.ann$datetime <- NULL
obs.day <- stack(obs.day)$values
obs.mon <- stack(obs.mon)$values
obs.ann <- stack(obs.ann)$values

write_aggregate_timescale <- function(product, folder) {
  
  df <- read.csv(paste0("processing/",folder,"/",product,"_pr_bf_hourly.csv"),header = T, sep = ",", dec = ".")
  df$datetime <- dtime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
  
  df$datetime <- as.Date(df$datetime, format = "%Y-%m-%d")
  daily.df <- aggregate(. ~ datetime, df, sum)
  tdaily <- data.frame(datetime = seq(as.POSIXct("2001-01-01"), as.POSIXct("2020-12-31"), by="day"))
  daily.df <- merge(tdaily, daily.df, by = "datetime", all = T)
  df$datetime <- dtime
  
  df$datetime <- strftime(df$datetime, format = "%Y-%m")
  mon.df <- aggregate(. ~ datetime, df, sum)
  tmon <- seq(as.POSIXct("2001-01-01"), as.POSIXct("2020-12-01"), by="mon")
  tmon <- data.frame(datetime=strftime(tmon, format = "%Y-%m"))
  mon.df <- merge(tmon, mon.df, by = "datetime", all = T)  
  df$datetime <- dtime
  
  df$datetime <- strftime(df$datetime, format = "%Y")
  year.df <- aggregate(. ~ datetime, df, sum)
  tyear <- data.frame(datetime = as.character(seq(2001, 2020, 1)))
  year.df <- merge(tyear, year.df, by = "datetime", all = T)  
  df$datetime <- dtime
  
  daily.df$datetime <- mon.df$datetime <- year.df$datetime <- NULL
  daily.df <- stack(daily.df)$values
  mon.df <- stack(mon.df)$values
  year.df <- stack(year.df)$values
  lret <- list(daily = daily.df, monthly = mon.df, yearly = year.df)
  
  return (lret)
}

metrics <- c("MAE","RMSE","PBIAS %","R2","KGE")
df.eval <- data.frame(matrix(nrow=0,ncol=length(metrics)+3))
colnames(df.eval) <- c("timescale", "product", "type", metrics)

for (product in products) {
  #product <- products[1]
  print(paste0("Processing: ", product))
  prod_raw <- write_aggregate_timescale(product, "0_hourly")
  prod_bc <- write_aggregate_timescale(product, "0_hourly_bc")
  
  daily <- data.frame(obs = obs.day, raw = prod_raw$daily, bc = prod_bc$daily)
  monthly <- data.frame(obs = obs.mon, raw = prod_raw$monthly, bc = prod_bc$monthly)
  yearly <- data.frame(obs = obs.ann, raw = prod_raw$yearly, bc = prod_bc$yearly)
  
  daily <- daily[daily$obs>0,]
  
  df.eval[nrow(df.eval)+1,] <- c("daily",product,"raw",as.numeric(gof(daily$obs, daily$raw)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("daily",product,"bc",as.numeric(gof(daily$obs, daily$bc)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("monthly",product,"raw",as.numeric(gof(monthly$obs, monthly$raw)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("monthly",product,"bc",as.numeric(gof(monthly$obs, monthly$bc)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("yearly",product,"raw",as.numeric(gof(yearly$obs, yearly$raw)[metrics,]))
  df.eval[nrow(df.eval)+1,] <- c("yearly",product,"bc",as.numeric(gof(yearly$obs, yearly$bc)[metrics,]))
  
}

write.csv(df.eval, file = paste0("tables/df_eval.csv"), row.names = F)
print("finished")

