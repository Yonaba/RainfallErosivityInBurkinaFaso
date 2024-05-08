setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(patchwork)

#stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

products <- list.files(path="processing/0_hourly_bc")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

Rmon <- data.frame(Month=1:12)
cnames <- c()
for (product in products) {
  #product <- products[3]
  print(product)
  df.raw <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_raw.csv"),header = T, sep = ",", dec = ".")
  df.bc <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_bc.csv"),header = T, sep = ",", dec = ".")
  Ragg.raw <- aggregate(R~Month, df.raw, sum)
  Ragg.bc <- aggregate(R~Month, df.bc, sum)
  Ragg.raw$R <- Ragg.raw$R/10/20
  Ragg.bc$R <- Ragg.bc$R/10/20
  Rmon <- merge(Rmon, Ragg.raw, by="Month", all.x=T)
  Rmon <- merge(Rmon, Ragg.bc, by="Month", all.x=T)
  cnames <- append(cnames,c(paste0(product,".raw"),paste0(product,".bc")))
}

Rmon[is.na(Rmon)] <- 0
colnames(Rmon) <- c("Months",cnames)

lproducts <- products[products !="imerg"]
cnames <- c("Months","imerg.bc",c(paste0(lproducts,".raw"),paste0(lproducts,".bc")))
Rmon <- Rmon[,cnames]
ann_R <- colSums(Rmon, na.rm=T)[-1]

obsRmon <- read.csv(paste0("input/rfactor_month.csv"),header = T, sep = ",", dec = ".")[,8:19]
colnames(obsRmon) <- month.abb
obsRmon <- as.numeric(colMeans(obsRmon))
obsRmon <- (obsRmon * 0.837755362)
 
cols <- c("#E31A1C","#E6AB02","#FB8072","#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")
pnames <- c("IMERG","CMORPH", "ERA5","MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")
cols <- c(cols, tail(cols,6))
pnames <- c("IMERG",paste0(tail(pnames,6)," (raw)"), paste0(tail(pnames,6)," (bc)"))

plotlist <- list()
for (k in 1:length(cols)) {
  #k <- 1
  plotlist[[k]] <- 
    ggplot(Rmon, mapping = aes(x, y)) +
    geom_bar(data = data.frame(x = Rmon$Months, y = Rmon[,k+1]), width = 0.8, stat = 'identity', fill = cols[k]) +
    geom_bar(data = data.frame(x = Rmon$Months, y = obsRmon), width = 0.4, stat = 'identity', fill = "grey", color = "black") +
    xlim(1,12) + ylim(0,2300) +
    xlab("") + 
    ylab(ifelse((k==1 || k==2 || k == 5 || k == 8 || k == 11),
                expression("R-factor [MJ mm "*~ha^-1~h^-1~month^-1*"]"),"")) +
    annotate("text",x=1, y=2300, label=paste0("(",letters[k],") ",pnames[k]), size = 4.5, hjust=0, fontface = "bold") +
    annotate("text",x=1, y=2000, label=paste0("Annual R-factor: ",format(round(ann_rf[k],1),nsmall=1)),
             size = 4, hjust=0, col="black", fontface="bold.italic") +
    #annotate("text",x=1, y=1800, label=paste0("Annual R-factor: ",sum(obsRmon)),size = 4, hjust=0, col="darkgrey", fontface="bold") +  
    theme_bw() + scale_x_continuous(breaks=1:12,labels=month.abb) +
    theme(axis.text=element_text(colour="black", size = 12),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

empty_plot <- list(ggplot() + theme_void())
plotlist <- append(plotlist, empty_plot, after = 1)
plotlist <- append(plotlist, empty_plot, after = 1)

grob <- wrap_plots(plotlist, ncol=3)
grob

ggsave("graphs/monthly_Rfactor.png", plot = grob, 
       width = 20, height = 27, units = "in", dpi = 350, scale = 0.6)
