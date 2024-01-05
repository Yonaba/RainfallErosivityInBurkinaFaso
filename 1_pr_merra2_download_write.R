setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(stringr)
library(lubridate)
library(nasapower)

cli_params <- c("PRECTOTCORR")

start_date <- "2001-01-01"
end_date <- "2020-12-31"
SLEEP_TIME_BETWEEN_REQUESTS <- 3

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")
rownames(stations) <- stations$Name

tseq <- as.character(seq(as.POSIXct("2001-01-01 00:00:00"), 
                         as.POSIXct("2020-12-31 23:00:00"), 
                         by="hour"))

df <- data.frame(matrix(nrow=length(tseq), ncol=nrow(stations)+1))
colnames(df) <- c("datetime",stations$Name)
df$datetime <- tseq
df$datetime <- ifelse(nchar(df$datetime) == 10,paste(df$datetime, "00:00:00"),df$datetime)

for (s in stations$Name) {
  #s <- "DORI"
  print(paste("Processing:", s))
  cli_df <- get_power(
    community = "ag",
    lonlat = c(stations[s,"Longitude"], stations[s,"Latitude"]),
    pars = cli_params,
    dates = c(start_date, end_date),
    temporal_api = "hourly")
  df[,s] <- cli_df$PRECTOTCORR
  df[df < 0] <- 0
  Sys.sleep(SLEEP_TIME_BETWEEN_REQUESTS)
}

write.csv(df, file = paste0("processing/0_hourly/merra2_pr_bf_hourly.csv"), row.names = F)
print("finished")

