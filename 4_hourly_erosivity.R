setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)

ref.product <- "imerg"
folder <- "0_hourly_bc"
products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
coef <- read.csv("processing/R_factor/coef_i30_i60_bf.csv",header = T, sep = ",", dec = ".")
rownames(coef) <- coef$station
stations <- coef$station

McGregor <- function(i) return(0.29*(1-0.72*exp(-0.082*i)))

#Shared from Nejc Bezak (https://doi.org/10.5194/esurf-10-851-2022)
CalcErosiveEvents <- function(Pcp, StartDate=as.POSIXct("1900-01-01", tz="UTC"), KinEnergy = McGregor,
                              Timestep=60, SeparationMin=1.27, InterEventDur = 6, PcpMin=12.7, Factor = 1, 
                              StationFactor=1) {

  Pcp[Pcp<0] <- as.integer(0) # Make sure no negative values in pcp vector
  Pcp[is.nan(Pcp)] <- as.integer(0) # if NaN
  Pcp[is.na(Pcp)] <- as.integer(0) # if NA
  IsWet <- (Pcp>0) # Vector describing if rain or no rain
  
  # Apply rolling sum which is used for the event separation - most time intensive part
  PcpRollSum <- rollsum(Pcp, k = 1 + InterEventDur*60/Timestep, align = "left", fill = 0) - Pcp # k = following 6 hrs = 1 + 6*60/Timestep
  #PcpRollI15 <- rollsum(Pcp, k = 30/Timestep, align = "center", fill = 0 ) # k = 15 mins = 15/Timestep
  # Extend wet periods between events if the separation criteria is not met
  IsWet <- ( IsWet | (PcpRollSum>=SeparationMin*Factor)  )
  
  # Now decompose the events and see which meet the erosivity criteria
  calcEventIMax <- function(a,b,Pcp,Dur=60) return( max(rollsum(x = Pcp[a:b], k = min(b-a+1,Dur/Timestep) )) ) # function to return max intensity [mm/hr] for given duration [mins]
  #calcEventI15Max <- function(a,b,PcpMax) return( max(PcpMax[a:b]) ) # function to return event I15 maximum
  calcEventVol <- function(a,b,Pcp) return(sum(Pcp[a:b])) # function to return volume of event
  
  Events <- rle(IsWet) # run length encoding function - compute the lengths and values of runs of equal values in a vector 
  Events <- data.frame(Length=Events[[1]],IsWet=Events[[2]]) # convert to data frame
  Events$endIndex <- cumsum(Events$Length) # end index from pcp vector for event
  Events$startIndex <- as.integer(Events$endIndex - Events$Length + 1) # likewise start index
  Events <- Events[Events$IsWet,] # we only care about wet events - remove dry events
  Events$Vol <- unlist(mapply(calcEventVol, Events$startIndex, Events$endIndex, MoreArgs=list(Pcp=Pcp)))
  #Events$I15 <- unlist(mapply(calcEventI15Max, Events$startIndex, Events$endIndex, MoreArgs=list(PcpMax=PcpRollI15))) 
  # Remove events that are under the criteria
  Events <- Events[ (Events$Vol>PcpMin ) , ]
  # Calculate also the I30 value for each event
  Events$I60 <- unlist(mapply(calcEventIMax, Events$startIndex, Events$endIndex, MoreArgs=list(Pcp=Pcp,Dur=60)))
  Events$I60 <- (Events$I60/Factor) / (60/60) # convert to intensity [mm/hr]
  Events$I30equiv <- (Events$I60 * StationFactor)
  # Function to calculate true event length, as there may be leading or trailing zeroes
  CalcEventDur <- function(a,b,Pcp) return( max(which(Pcp[a:b]!=0)) - min(which(Pcp[a:b]!=0)) + 1 )
  CalcEventDate <- function(a,b,Pcp) return( min(which(Pcp[a:b]!=0))+a-2  )

  CalcEventE <- function(a,b,Pcp) return( sum(KinEnergy((Pcp[a:b]/Factor)/(Timestep/60)) * (Pcp[a:b]/Factor) ) ) # pass pcp to Brown and Foster eqn
  Events$E <- unlist(mapply(CalcEventE, Events$startIndex, Events$endIndex, MoreArgs=list(Pcp=Pcp)))
  Events$R <- Events$E * Events$I30equiv
  Events$Date <- StartDate + (unlist(mapply(CalcEventDate, Events$startIndex, Events$endIndex, MoreArgs=list(Pcp=Pcp)))*Timestep*60)
  Events$Year <- year(Events$Date)
  Events$D_h <- unlist(mapply(CalcEventDur, Events$startIndex, Events$endIndex, MoreArgs=list(Pcp=Pcp))) * Timestep/60 
  Events$I_mmh <- (Events$Vol/Factor) / Events$D_h # Event pcp intensity [mm/hr]
  Events$P_mm <- (Events$Vol/Factor) # rename column
  # Remove unneeded columns
  Events[,c("IsWet","Length","Vol","startIndex","endIndex")] <- NULL
  # Reorder columns
  Events$Month <- month(Events$Date)
  Events <- Events[,c("Date", "Year","Month","P_mm", "D_h", "I_mmh","E","I60","I30equiv","R")]
  return (Events)
}

for (product in products) {
  #product <- "imerg"
  
  df <- read.csv(paste0("processing/",folder,"/",product,"_pr_bf_hourly.csv"),header = T, sep = ",", dec = ".")
  # df$datetime <- as.character(df$datetime)
  # df$datetime <- ifelse(nchar(df$datetime) == 10, paste(df$datetime, "00:00:00"), df$datetime)
  # df$datetime <- as.POSIXct(as.character(df$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
  # data <- df[,2:ncol(df)]
  #data[is.na(data)] <- 0
  # data[data<THRESHOLD] <- 0
  
  # dday <- date(df$datetime)
  # hh <- hour(df$datetime)
  # yy <- year(df$datetime)
  # df <- data.frame(datetime = df$datetime, year = yy, day = dday,hour = hh, data)
  
  #cnames <- c("station","year", "day", "duration", "total","i60","R")
  #alld <- data.frame(matrix(nrow = 0, ncol=length(cnames)))
  #colnames(alld) <- cnames
  cnames <- c("Station","Year","Date", "P_mm", "D_h", "I_mmh","E","I60","R")
  alld <- data.frame(matrix(nrow=0, ncol=length(cnames)))
  colnames(alld) <- cnames
  
  for (s in stations) {
    #s <- "OUAGADOUGOU"
    print(paste0("Processing: ",product,"/",s))
    sdf <- df[,s]
    sdate <- df[1,"datetime"]
    events <- CalcErosiveEvents(sdf, StartDate = as.POSIXct(sdate, tz="UTC"), KinEnergy = McGregor,
                                Timestep=60, SeparationMin=1.27, InterEventDur = 6, PcpMin=12.7,
                                StationFactor=ifelse(product!=ref.product,coef[s,"coef"],1))
    events <- data.frame(Station = rep(s, nrow(events)), events)
    alld <- rbind(alld, events)
    
    #imax <- aggregate(as.formula(paste0(s,"~year")),sdf,max)
    #imax$year <- as.numeric(imax$year)
    
    # ISRAINING  <- F
    # raind <- raintot <- eraintot <- i60 <- er <- E <- 0
    # for (i in 1:length(df$datetime)) {
    #   #print(paste(i,sdf[i,s]))
    #   cyear <- sdf[i,"year"]
    #   cimax <- imax[cyear-2001+1,s]
    #   if (sdf[i,s] >= THRESHOLD) {
    #     ir <- sdf[i,s]
    #     ISRAINING <- T
    #     raind <- raind + 1
    #     eraintot <- eraintot + sdf[i,s]
    #     i60 <- max(i60, sdf[i,s])
    #     er <- 0.29*(1-0.72*exp(-0.082*(sdf[i,s])))
    #     E <- E + (er * (sdf[i,s]))
    #   } else {
    #     if (ISRAINING) 
    #       alld[nrow(alld)+1,] <- c(s,
    #                                sdf[i,"year"], 
    #                                as.character(sdf[i,"day"]), 
    #                                raind, 
    #                                eraintot,
    #                                i60,
    #                                coef[s,"coef"] * E * i60)
    #     ISRAINING <- F
    #     raind <- raintot <- eraintot <- er <- i60 <- E <- 0
    #   }
    # }
  }
  
  write.csv(alld, file = paste0("processing/R_factor/",product,"_Rfactor_bf_bc.csv"), row.names = F)  
  
  # numcols <- c("year", "duration","total","i60","R")
  # alld[,numcols] <- sapply(alld[,numcols], as.numeric)
  # alld$Rusa <- alld$R/17.02
  # #alld$R <- (alld$R * i60)
  # aggregate(aggregate(R~year+station, alld, sum)$R, list(aggregate(R~year+station, alld, sum)$station),mean)
  # aggregate(aggregate(Rusa~year+station, alld, sum)$Rusa, list(aggregate(Rusa~year+station, alld, sum)$station),mean)
  # alld <- alld[alld$total>=E_THRESHOLD,]
  # write.csv(alld, file = paste0("processing/R_factor/",product,"_erosivity_bf.csv"), row.names = F)  
}

print("finished.")

