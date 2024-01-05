setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(patchwork)

#stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

products <- list.files(path="processing/0_hourly_bc")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

pobs <- read.csv(paste0("input/pr_obs_monthly.csv"),header = T, sep = ",", dec = ".")
pobs$Month <- rep(1:12,20)
pobs$datetime <- NULL
pobs <- aggregate(.~Month, pobs, sum)/20
pobs$Month <- NULL
pobs <- rowMeans(pobs)

type <- "bc"
Rmon <- data.frame(Month=1:12)
Rmon$Pobs <- pobs
Pmon <- data.frame(matrix(nrow=12, ncol=0))

for (product in products) {
  #product <- products[5]
  print(product)
  df <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_",type,".csv"),header = T, sep = ",", dec = ".")
  Ragg <- aggregate(P_mm~Month, df, sum)
  Ragg$P_mm <- Ragg$P_mm/10/20
  Rmon <- merge(Rmon, Ragg, by="Month", all.x=T)
  
  prdf <- read.csv(paste0("processing/2_monthly/",product,"_pr_bf_monthly.csv"),header = T, sep = ",", dec = ".")
  prdf$datetime <- NULL
  prdf$Month <- rep(1:12,20)
  prdf <- aggregate(.~Month, prdf,mean)
  prdf$Month <- NULL
  prdfmon <- rowMeans(prdf)
  Pmon <- cbind(Pmon, prdfmon)
}
colnames(Rmon) <- c("Months","Pobs",products)
colnames(Pmon) <- products

cols <- c("#E6AB02","#E31A1C","#FB8072","#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")
pnames <- c("CMORPH", "ERA5", "IMERG", "MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")
plotlist <- list()
for (k in 1:7) {
  plotlist[[k]] <- ggplot(Rmon, mapping = aes(x, y)) +
    geom_bar(data = data.frame(x = Rmon$Months, y = Pmon[,k]), width = 0.9, stat = 'identity', fill = "lightblue", color = "black") +
    geom_bar(data = data.frame(x = Rmon$Months, y = Rmon$Pobs), width = 0.6, stat = 'identity', fill = "bisque3") +
    geom_bar(data = data.frame(x = Rmon$Months, y = Rmon[,k+2]), width = 0.3, stat = 'identity', fill = cols[k],color = "black") +
    xlim(1,12) + ylim(0,500) +
    xlab("") + ylab(ifelse((k==1 || k==5),"Rainfall [mm]","")) +
    geom_text(x=0.5, y=500, label=paste0("(",letters[k],") ",pnames[k]), size = 4, hjust=0) +
    theme_bw() + scale_x_continuous(breaks=1:12,labels=month.abb) +
    theme(axis.text=element_text(colour="black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

grob <- wrap_plots(plotlist, ncol=4, bg)
grob
ggsave("graphs/erosive_rainfall.png", plot = grob, 
       width = 25, height = 12, units = "in", dpi = 350, scale = 0.6)
