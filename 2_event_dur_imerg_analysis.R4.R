setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

McGregor <- function(i) return(0.29*(1-0.72*exp(-0.082*i)))

CalcErosiveEventsDur <- function(Pcp, StartDate=as.POSIXct("1900-01-01", tz="UTC"), KinEnergy = McGregor,
                              Timestep=30, SeparationMin=1.27, InterEventDur = 6, PcpMin=12.7, Factor = 1, 
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
  Events <- Events[,c("Year","Date", "P_mm", "D_h", "I_mmh","E","I60","I30equiv","R")]
  return (Events)
}

product <- "imerg"

sim <- read.csv(paste0("processing/0_30mn/",product,"_pr_bf_30mn.csv"),header = T, sep = ",", dec = ".")
sim$datetime <- as.POSIXct(as.character(sim$datetime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
cnames <- c("station", "dur")

stations <- colnames(sim)[-1]
dur.df <- data.frame(matrix(nrow=0, ncol=length(cnames)))
colnames(dur.df) <- cnames

for (s in stations) {
  #s <- "OUAGADOUGOU"
  print(paste0("Processing: ", s))
  df <- sim[,s]
  sdate <- sim$datetime[1]
  events <- CalcErosiveEventsDur(df, sdate,KinEnergy = McGregor, Timestep = 30,
                                 SeparationMin = 1.27, InterEventDur = 6, PcpMin = 12.7, 
                                 Factor=1, StationFactor = 1)
  dur <- data.frame(rep(s,length(events$D_h)),events$D_h)
  colnames(dur) <- cnames
  dur.df <- rbind(dur.df, dur)
}

ggplot(dur.df, aes(x=dur,fill=station)) +
  geom_density() +
  xlim(0,24) +
  facet_wrap(.~station)

obs.ann <- read.csv(paste0("input/pr_obs_yearly.csv"),header = T, sep = ",", dec = ".")
stations <- colnames(obs.mon)[-1]

lfiles.mon[[1]] <- obs.mon
lfiles.ann[[1]] <- obs.ann
i <- 1

for (product in products) {
  i <- i+1
  lfiles.mon[[i]] <- read.csv(paste0("processing/2_monthly/",product,"_pr_bf_monthly.csv"),
                          header = T, sep = ",", dec = ".")
  lfiles.ann[[i]] <- read.csv(paste0("processing/3_yearly/",product,"_pr_bf_yearly.csv"),
                              header = T, sep = ",", dec = ".")  
}
names(lfiles.mon) <- names(lfiles.ann) <- c("obs", products)

headers <- c("source", "x","value","station")
df.mon <- df.ann <- data.frame(matrix(nrow=0, ncol=length(headers)))
colnames(df.mon) <- colnames(df.ann) <- headers

for (source in names(lfiles.mon)) {
  #source <- "cmorph"
  
  dmon <- lfiles.mon[[source]]
  dann <- lfiles.ann[[source]]
  
  dmon$datetime <- NULL
  dmon$mon <- rep(1:12,20)
  dmon <- aggregate(.~mon, dmon, mean)
  dmon$mon <- dann$datetime <- NULL
  
  dmon <- stack(dmon)
  dann <- stack(dann)
  
  dmon <- data.frame(source = rep(source, nrow(dmon)),
                     mon = rep(1:12,length(stations)),
                     dmon)
  dann <- data.frame(source = rep(source, nrow(dann)),
                     year = rep(2001:2020, length(stations)),
                     dann)

  colnames(dann) <- colnames(dmon) <- headers
  df.mon <- rbind(df.mon, dmon)
  df.ann <- rbind(df.ann, dann)
}

df.mon$source <- as.factor(df.mon$source)
df.mon$source <- factor(df.mon$source, levels=c("obs", products))
df.ann$source <- as.factor(df.ann$source)
df.ann$source <- factor(df.ann$source, levels=c("obs", products))

cols <- c("black", "#E6AB02","#E31A1C","#FB8072","#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")

pl.mon <- ggplot(df.mon, aes(x=x, y = value, color = source)) +
  geom_line(aes(size = source)) +
  xlim(0,12) + ylim(0,400) +
  facet_wrap(.~station, ncol=5) +
  xlab ("\nMonth") + ylab("Rainfall [mm]\n") +
  scale_color_manual(values=cols) +
  scale_size_manual(values=c(1.2, rep(0.5,length(products)))) +
  scale_x_continuous(breaks=1:12, labels=month.abb) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("graphs/cmp_mon.png", plot = pl.mon, 
       width = 25, height = 12.5, units = "in", dpi = 400, scale = 0.5)

pl.ann <- ggplot(df.ann, aes(x=x, y = value, color = source)) +
  geom_line(aes(size = source)) +
  xlim(2001,2020) + ylim(200,2500) +
  facet_wrap(.~station, ncol=5) +
  xlab ("\nYears") + ylab("Annual rainfall [mm]\n") +
  scale_color_manual(values=cols) +
  scale_size_manual(values=c(1.2, rep(0.5,length(products)))) +
  #scale_x_continuous(breaks=2001:2020, labels=month.abb) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave("graphs/cmp_ann.png", plot = pl.ann, 
       width = 35, height = 13, units = "in", dpi = 400, scale = 0.45)

print("finished.")
