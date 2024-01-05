setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(patchwork)

#stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

products <- list.files(path="processing/0_hourly_bc")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

type <- "bc"
Rmon <- data.frame(Month=1:12)

for (product in products) {
  #product <- products[3]
  print(product)
  df <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_",type,".csv"),header = T, sep = ",", dec = ".")
  Ragg <- aggregate(R~Month, df, sum)
  Ragg$R <- Ragg$R/10/20
  Rmon <- merge(Rmon, Ragg, by="Month", all.x=T)
}
colnames(Rmon) <- c("Months",products)
ann_rf <- colSums(Rmon, na.rm=T)[-1]
Rmon[is.na(Rmon)] <- 0

obsRmon <- read.csv(paste0("input/rfactor_month.csv"),header = T, sep = ",", dec = ".")[,8:19]
colnames(obsRmon) <- month.abb
obsRmon <- as.numeric(colMeans(obsRmon))

cols <- c("#E6AB02","#E31A1C","#FB8072","#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")
pnames <- c("CMORPH", "ERA5", "IMERG", "MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")
plotlist <- list()
for (k in 1:7) {
  plotlist[[k]] <- ggplot(Rmon, mapping = aes(x, y)) +
    geom_bar(data = data.frame(x = Rmon$Months, y = Rmon[,k+1]), width = 0.8, stat = 'identity', fill = cols[k]) +
    geom_bar(data = data.frame(x = Rmon$Months, y = obsRmon), width = 0.4, stat = 'identity', fill = "grey", color = "black") +
    xlim(1,12) + ylim(0,2300) +
    xlab("") + 
    ylab(ifelse((k==1 || k==5),expression("R-factor [MJ mm "*~ha^-1~h^-1~month^-1*"]"),"")) +
    annotate("text",x=1, y=2300, label=paste0("(",letters[k],") ",pnames[k]), size = 4.5, hjust=0, fontface = "bold") +
    annotate("text",x=1, y=2000, label=paste0("Annual R-factor: ",format(round(ann_rf[k],1),nsmall=1)),
             size = 4, hjust=0, col=cols[k], fontface="bold") +
    annotate("text",x=1, y=1800, label=paste0("Annual R-factor: ",sum(obsRmon)),size = 4, hjust=0, 
           col="darkgrey", fontface="bold") +  
    theme_bw() + scale_x_continuous(breaks=1:12,labels=month.abb) +
    theme(axis.text=element_text(colour="black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

grob <- wrap_plots(plotlist, ncol=4, bg)
grob
ggsave("graphs/monthly_Rfactor.png", plot = grob, 
       width = 25, height = 12, units = "in", dpi = 350, scale = 0.5)
