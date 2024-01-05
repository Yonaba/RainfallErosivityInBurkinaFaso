setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(randomcoloR)
library(ggplot2)
library(patchwork)
library(ggpubr)

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

ref.product <- "imerg"
products <- products[products != ref.product]

ref.data <- read.csv(file = paste0("processing/0_hourly/",ref.product,"_pr_bf_hourly.csv"), header = T, sep=",")
stations <- colnames(ref.data[,2:ncol(ref.data)])
pmonths <- list(c(12,1:2), 3:5, 6:8, 9:11)
pmonths.names <- c("DJF", "MAM", "JJA", "SON")

pl.list <- list()
cols <- c("black",distinctColorPalette(6))

pnames <- c("IMERG","CMORPH", "ERA5", "MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")

for (pmon in 1:length(pmonths)) {
  #pmon <- 1
  mon <- pmonths[[pmon]]
  
  raw.df <- bc.df <- data.frame(matrix(nrow=24, ncol=length(products)+1))
  colnames(raw.df) <- colnames(bc.df) <- c(ref.product, products)
  
  for (sim.product in products) {
    #sim.product <- "era5"
    print(paste0("Processing: ", pmonths.names[pmon]," - ", sim.product))
    ref.data <- read.csv(file = paste0("processing/0_hourly/",ref.product,"_pr_bf_hourly.csv"), header = T, sep=",")
    sim.data <- read.csv(file = paste0("processing/0_hourly/",sim.product,"_pr_bf_hourly.csv"), header = T, sep=",")
    simc.data <- read.csv(file = paste0("processing/0_hourly_bc/",sim.product,"_pr_bf_hourly.csv"), header = T, sep=",")
    sim.data[is.na(sim.data)] <- simc.data[is.na(sim.data)] <- 0
    ref.data$datetime <- sim.data$datetime <- simc.data$datetime <- as.POSIXct(ref.data$datetime, format="%Y-%m-%d %H:%M:%S",tz="UTC") 
    
    subrows <- which(month(ref.data$datetime) == mon[1] | 
                       month(ref.data$datetime) == mon[2] | 
                       month(ref.data$datetime) == mon[3])
    ref.data <- ref.data[subrows,]
    sim.data <- sim.data[subrows,]
    simc.data <- simc.data[subrows,]
    
    hh <- hour(ref.data$datetime)
    ref.data <- stack(ref.data)[,"values"]
    sim.data <- stack(sim.data)[,"values"]
    simc.data <- stack(simc.data)[,"values"]
    
    ref.data <- data.frame(hour = rep(hh,length(stations)),value = ref.data)
    sim.data <- data.frame(hour = rep(hh,length(stations)),value = sim.data)
    simc.data <- data.frame(hour = rep(hh,length(stations)),value = simc.data)
    
    # ref.data$value[ref.data$value == 0] <- NA
    # sim.data$value[sim.data$value == 0] <- NA 
    # simc.data$value[simc.data$value == 0] <- NA 
    
    cref <- aggregate(.~hour, data = ref.data, FUN = mean)
    csim <- aggregate(.~hour, data = sim.data, FUN = mean) 
    csimc <- aggregate(.~hour, data = simc.data, FUN = mean)
    
    raw.df[,ref.product] <- bc.df[,ref.product] <- cref$value
    raw.df[,sim.product] <- csim$value
    bc.df[,sim.product] <- csimc$value
  }
  
  raw.df <- data.frame(rep(c(24,1:23),ncol(raw.df)), stack(raw.df))
  bc.df <- data.frame(rep(c(24,1:23),ncol(bc.df)), stack(bc.df))
  colnames(raw.df) <- colnames(bc.df) <- c("hour", "rainfall", "reanalysis")
  
  raw.df <- raw.df[order(raw.df$reanalysis, raw.df$hour),]
  bc.df <- bc.df[order(bc.df$reanalysis, bc.df$hour),]
  # raw.df$reanalysis <- factor(raw.df$reanalysis, levels=c(levels(raw.df$reanalysis)[-1],"merra2"))
  # bc.df$reanalysis <- factor(bc.df$reanalysis, levels=c(levels(bc.df$reanalysis)[-1],"merra2"))
  # 
  # raw.df <- raw.df[order(raw.df$reanalysis),]
  # bc.df <- bc.df[order(bc.df$reanalysis),]
  
  #y_lim <- c(0, max(raw.df$rainfall, bc.df$rainfall))
  y_lim <- c(0,0.6)
  
  
  pl.raw <- ggplot(raw.df, aes(x = hour, y = rainfall, color = reanalysis, linetype = reanalysis)) +
    geom_line(aes(size = reanalysis)) + 
    ylim(y_lim) +
    xlab("") +
    ylab(ifelse(pmon == 1, "Rainfall [mm]","")) +
    scale_color_manual(values = cols, labels = as.character(pnames)) +
    scale_linetype_manual(values = c("solid",rep("twodash",6))) +
    scale_size_manual(values = c(1.2, rep(0.7,6))) +
    scale_x_continuous(limits=c(1,24), breaks=seq(2,24,2), labels = paste0(seq(2,24,2),":00")) +
    labs(title = paste0(letters[pmon],") Diurnal rainfall cycle (", pmonths.names[pmon], ", raw)")) +
    guides(linetype = F, size = F, color = guide_legend(title = "")) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1, size = 10),
          plot.title = element_text(size = 12)) +
    guides(colour = guide_legend(nrow = 1))

  pl.bc <- ggplot(bc.df, aes(x = hour, y = rainfall, color = reanalysis, linetype = reanalysis)) +
    geom_line(aes(size = reanalysis)) + 
    ylim(y_lim) +
    xlab("\nTime (Hours)") +
    ylab(ifelse(pmon == 1, "Rainfall [mm]","")) +
    scale_color_manual(values = cols, labels = as.character(pnames)) +
    scale_linetype_manual(values = c("solid",rep("twodash",6))) +
    scale_size_manual(values = c(1.2, rep(0.7,6))) +
    scale_x_continuous(limits=c(1,24), breaks=seq(2,24,2), labels = paste0(seq(2,24,2),":00")) +
    labs(title = paste0("(",letters[pmon+4],") Diurnal rainfall cycle (", pmonths.names[pmon], ", bc)")) +
    guides(linetype = F, size = F, color = guide_legend(title = "")) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust = 1, size = 10),
          plot.title = element_text(size = 12)) +
    guides(colour = guide_legend(nrow = 1))
  
  pl.bc
  pl.list[[pmon]] <- pl.raw
  pl.list[[pmon+4]] <- pl.bc
}

grob <- ggarrange(plotlist = pl.list, ncol=4, nrow=2, common.legend = TRUE, legend="bottom")
ggsave(filename = paste0("graphs/bias_correction_hourly.png"), 
       grob, width = 16, height = 8, dpi = 400,scale = 0.8)
  

