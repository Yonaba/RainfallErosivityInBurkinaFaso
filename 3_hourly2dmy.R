setwd("D:/Recherche/Article_Erosion_2023/analysis/")
Sys.setenv(TZ = "UTC")

library(zoo)
library(lubridate)

products <- list.files(path="processing/0_hourly_bc")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

write_aggregate_timescale <- function(product) {
  
  df <- read.csv(paste0("processing/0_hourly_bc/",product,"_pr_bf_hourly.csv"),header = T, sep = ",", dec = ".")
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
  
  write.csv(daily.df, file = paste0("processing/1_daily/",product,"_pr_bf_daily.csv"), row.names = F)
  write.csv(mon.df, file = paste0("processing/2_monthly/",product,"_pr_bf_monthly.csv"), row.names = F)
  write.csv(year.df, file = paste0("processing/3_yearly/",product,"_pr_bf_yearly.csv"), row.names = F)
}

for (product in products) {
  print(paste0("Processing: ", product))
  write_aggregate_timescale(product)
}
print("finished")

