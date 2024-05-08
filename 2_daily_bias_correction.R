setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(qmap)
library(MBC)
library(stringr)
library(lubridate)

THRESHOLD <- 0.1/24
TAU <- 0.85
SUBSAMPLE <- 30

ref.product <- "imerg"
products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
products <- products[products != ref.product]

ref.data <- read.csv(file = paste0("processing/0_hourly/",ref.product,"_pr_bf_hourly.csv"), header = T, sep=",")

for (sim.product in products) {
  #sim.product <- "era5"
  
  sim.data <- read.csv(file = paste0("processing/0_hourly/",sim.product,"_pr_bf_hourly.csv"), header = T, sep=",")
  sim.data[is.na(sim.data)] <- 0
  
  ref.data[,2:ncol(ref.data)] <- lapply(ref.data[,2:ncol(ref.data)],function(x) ifelse(x<THRESHOLD,0,x))
  sim.data[,2:ncol(sim.data)] <- lapply(sim.data[,2:ncol(sim.data)],function(x) ifelse(x<THRESHOLD,0,x))
  
  ref.data[,2:ncol(ref.data)] <- ref.data[,2:ncol(ref.data)]
  sim.data[,2:ncol(sim.data)] <- sim.data[,2:ncol(sim.data)]
  
  ref.data$datetime <- sim.data$datetime <- as.POSIXct(ref.data$datetime, format="%Y-%m-%d %H:%M:%S",tz="UTC")
  stations <- colnames(ref.data[,2:ncol(ref.data)])
  
  out.data <- data.frame(datetime = ref.data$datetime)
  
  for (station in stations) {
    #station <- "OUAGADOUGOU"
    simc.data <- data.frame(matrix(nrow = 0, ncol=2))
    colnames(simc.data) <- c("datetime", "sim")
    simc.data$datetime <- as.POSIXct(as.character(simc.data$datetime), format="%Y-%m-%d %H:%M:%S",tz="UTC")
    
    for (hh in 0:23) {
      #hh <- 23
      print(paste0("Processing: ", sim.product," - station: ",station," - hour: ", hh))
      hh_p <- ifelse(hh == 0, 23, hh-1)
      hh_n <- ifelse(hh == 23, 0, hh+1)
      rows <- which(hour(ref.data$datetime) == hh_p | hour(ref.data$datetime) == hh | hour(ref.data$datetime) == hh_n)
      rows <- which(hour(ref.data$datetime) == hh)
      s.data <- data.frame(datetime = ref.data[rows, "datetime"], 
                           ref = ref.data[rows, station],
                           sim = sim.data[rows, station])
      for (d in 1:366) {
        #d <- 366
        subrows <- which(yday(s.data$datetime)==d)
        s.data[subrows,"sim"] <- QDM(o.c = s.data[subrows,"ref"], 
                                     m.c = s.data[subrows,"sim"],
                                     m.p = s.data[subrows,"sim"], 
                                     ratio = T, trace = THRESHOLD, 
                                     n.tau = TAU * length(s.data[subrows,"ref"]),
                                     subsample = SUBSAMPLE)$mhat.c          

      }
      s.data <- s.data[which(hour(s.data$datetime) == hh),colnames(simc.data)]
      simc.data <- rbind(simc.data,s.data)
    }
    
    simc.data <- simc.data[order(simc.data$datetime),]
    simc.data$sim[simc.data$sim < THRESHOLD] <- 0
    out.data <- cbind(out.data, simc.data$sim)
  }
  
  colnames(out.data) <- c("datetime", stations)
  out.data$datetime <- as.character(out.data$datetime)
  out.data$datetime <- ifelse(nchar(out.data$datetime) == 10,paste(out.data$datetime, "00:00:00"), out.data$datetime)
  write.csv(out.data, file = paste0("processing/0_hourly_bc/",sim.product,"_pr_bf_hourly.csv"), row.names = F) 
}

write.csv(ref.data, file = paste0("processing/0_hourly_bc/",ref.product,"_pr_bf_hourly.csv"), row.names = F) 

print("finished.")
