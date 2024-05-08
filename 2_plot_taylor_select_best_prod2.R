setwd("D:/Recherche/Article_Erosion_2023/analysis/")
Sys.setenv(TZ = "UTC")

library(stringr)
library(gridExtra)
#library(zoo)
library(openair)
#library(lubridate)
#library(hydroGOF)
#library(ggplot2)
library(ggpubr)

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
#products <- products[products!=ref.prod]

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

ref <- list(
  daily = stack(obs.day)$values,
  monthly = stack(obs.mon)$values,
  yearly = stack(obs.ann)$values
)

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

df <- data.frame(matrix(nrow=0, ncol=4))
colnames(df) <- c("obs","mod","RP","type")

for (product in products) {
  #product <- products[1]
  print(paste0("Processing: ", product))
  prod_raw <- write_aggregate_timescale(product, "0_hourly", min_rf)
  daily <- data.frame(obs = ref$daily , raw = prod_raw$daily)
  #min_rf <- 12.7
  # if (product != "persiann") {
  #   daily <- daily[((daily$obs>=min_rf) & (daily$raw>=min_rf) & (daily$bc>=min_rf)),]
  # }
  
  daily$draw <- abs(daily$raw - daily$obs)
  #daily$dbc <- abs(daily$bc - daily$obs) 
  daily <- daily[(daily$draw/daily$obs<0.425),]
  daily$draw <- NULL
  
  dfd <- data.frame(obs = daily$obs,mod = daily$raw,RP = rep(paste0(product), nrow(daily)),
                     type = rep("Daily timescale", nrow(daily)))
  df <- rbind(df, dfd)
  
  monthly <- data.frame(obs = ref$monthly , raw = prod_raw$monthly)
  dfm <- data.frame(obs = monthly$obs,mod = monthly$raw,RP = rep(paste0(product), nrow(monthly)),
                     type = rep("Monthly timescale", nrow(monthly)))
  df <- rbind(df, dfm)
  
  yearly <- data.frame(obs = ref$yearly , raw = prod_raw$yearly)
  dfy <- data.frame(obs = yearly$obs,mod = yearly$raw,RP = rep(paste0(product), nrow(yearly)),
                     type = rep("Annual timescale", nrow(yearly)))
  df <- rbind(df, dfy)
  
}

cols <- c("#E6AB02","#FB8072","#E31A1C", "#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")
pnames <- c("CMORPH", "ERA5", "IMERG","MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")
names(pnames) <- c("cmorph", "era5","imerg", "merra2", "pdirnow", "persiann", "persiannccs")

ord <- products

df.daily <- df[df$type == "Daily timescale",]
df.daily$RP <- factor(df.daily$RP, levels = ord, ordered = T)

df.monthly <- df[df$type == "Monthly timescale",]
df.monthly$RP <- factor(df.monthly$RP, levels = ord, ordered = T)

df.yearly <- df[df$type == "Annual timescale",]
df.yearly$RP <- factor(df.yearly$RP, levels = ord, ordered = T)

res <- 400
factor <- res/72

w <- 600 * 1.25
h <- 300 * 1.25 

parset <- list(par.main.text = list(font = 2, just = "left", x = grid::unit(6, "mm")))

png("graphs/taylor_comp_daily.png", width = w * factor, height = h * factor, res = res,restoreConsole = TRUE)
TaylorDiagram(df.daily, obs = "obs", mod = "mod", group = "RP", 
              normalize = T, cols = cols, arrow.lwd = 3, 
              pch = c(rep(19,7)), text.obs = "observed",
              annotate = "RMSE",
              key.pos = "right", key.columns = 1, key.title = "Rainfall products (RPs)", cex=1.75,
              par.settings = parset, main = "a) Daily timescale", xlab="Standard deviation", ylab="Standard deviation")

dev.off()


png("graphs/taylor_cmp_monthly.png", width = w * factor, height = h * factor, res = res,restoreConsole = TRUE)
TaylorDiagram(df.monthly, obs = "obs", mod = "mod", group = "RP",
              normalize = T, cols = cols, arrow.lwd = 3, 
              pch = c(rep(19,7)), text.obs = "observed",
              annotate = "RMSE",
              key.pos = "right", key.columns = 1, key.title = "Rainfall products (RPs)", cex = 1.75,
              par.settings = parset, main = "b) Monthly timescale", xlab="Standard deviation", ylab="")

dev.off()

png("graphs/taylor_cmp_yearly.png", width = w * factor, height = h * factor, res = res,restoreConsole = TRUE)
TaylorDiagram(df.yearly, obs = "obs", mod = "mod", group = "RP",
              normalize = T, cols = cols, arrow.lwd = 3, 
              pch = c(rep(19,7)), text.obs = "observed",
              annotate = "RMSE",
              key.pos = "right", key.columns = 1, key.title = "Rainfall products (RPs)", cex = 1.75,
              par.settings = parset, main = "c) Annual timescale", xlab="Standard deviation", ylab="Standard deviation")

dev.off()

print("finished")

