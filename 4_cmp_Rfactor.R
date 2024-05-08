setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(ggpubr)
library(patchwork)
library(hydroGOF)

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

products <- list.files(path="processing/0_hourly_bc")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

type <- "bc"

df.R.raw <- df.R.bc <- data.frame(Station = stations)
for (product in products) {
  #product <- products[3]
  print(product)
  df.raw <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_","raw",".csv"),header = T, sep = ",", dec = ".")
  df.bc <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_","bc",".csv"),header = T, sep = ",", dec = ".")
  Ragg.raw <- aggregate(R~Station, df.raw, sum)
  Ragg.bc <- aggregate(R~Station, df.bc, sum)
  Ragg.raw$R <- Ragg.raw$R/20
  Ragg.bc$R <- Ragg.bc$R/20
  df.R.raw <- merge(df.R.raw, Ragg.raw, by="Station",all.x=T)
  df.R.bc <- merge(df.R.bc, Ragg.bc, by="Station",all.x=T)
}
colnames(df.R.raw) <- colnames(df.R.bc) <- c("Station",products)

pann <- read.csv(paste0("input/pr_obs_yearly.csv"),header = T, sep = ",", dec = ".")
pann$datetime <- NULL
pann <- colMeans(pann)
pann <- data.frame(Station = names(pann), pann = as.numeric(pann))
pann$R_Roose <- (pann$pann/2) * 17.02

glored <- read.csv(paste0("input/bf_stations.csv"),header = T, sep = ",", dec = ".")
glored <- glored[,c("Name","Rfactor")]
colnames(glored)[1] <- "Station"
pann <- merge(pann, glored, by="Station", all.x=T)

df.R.raw <- data.frame(GloREDa = pann$Rfactor, df.R.raw[,products])
df.R.bc <- data.frame(GloREDa = pann$Rfactor, df.R.bc[,products])

metrics <- c("RMSE", "MAE", "PBIAS %")
mdf <- data.frame(matrix(nrow=0, ncol=9))
colnames(mdf) <- c("type", "product","meanGloREDa","sdGloREDa","meanRP","sdRP",metrics)
for (product in products) {
  #product <- products[1]
  gofs.raw <- gof(df.R.raw$GloREDa, df.R.raw[,product])
  gofs.bc <- gof(df.R.bc$GloREDa, df.R.bc[,product])
  gofs.raw <- gofs.raw[metrics,]
  gofs.bc <- gofs.bc[metrics,]
  mdf[nrow(mdf)+1,] <- c("raw",product,
                         mean(df.R.raw$GloREDa),sd(df.R.raw$GloREDa),
                         mean(df.R.raw[,product]),sd(df.R.raw[,product]),
                         gofs.raw)
  mdf[nrow(mdf)+1,] <- c("bc",product,
                         mean(df.R.bc$GloREDa),sd(df.R.bc$GloREDa),
                         mean(df.R.bc[,product]),sd(df.R.bc[,product]),                         
                         gofs.bc)
}

mdf <- mdf[order(mdf$type, decreasing = T),]
write.csv(mdf, file = paste0("tables/eval_perf_rps.csv"), row.names = F)

df.R.raw <- stack(df.R.raw)
df.R.bc <- stack(df.R.bc)

cols <- c("lightgrey","#E6AB02","#FB8072","#E31A1C", "#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")
pnames <- c("GloREDa","CMORPH", "ERA5", "IMERG","MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")

rawplot <- ggboxplot(df.R.raw, x = "ind", y="values", fill="ind", palette = cols) +
  #stat_compare_means(label.x = 1, label.y = 5000) +
  #ylim(0,8000) +
  #geom_segment(aes(x=8, y=7000, xend=8, yend=8000), arrow = arrow(length=unit(.5, 'cm')),color = tail(cols,1), lwd = 2) +
  xlab("") + ylab(expression("R-factor [MJ mm "*~ha^-1~h^-1~yr^-1*"]")) +
  scale_x_discrete(labels=pnames) + scale_y_continuous(trans="sqrt") +
  theme_bw() +  
  theme(legend.position = "none",
        axis.text=element_text(colour="black", size = 10)) +
  labs(title="(a) Annual R-factor (raw)")
#rawplot
bcplot <- ggboxplot(df.R.bc, x = "ind", y="values", fill="ind", palette = cols) +
  #stat_compare_means(label.x = 1, label.y = 5000) +
  #ylim(0,8000) +
  xlab("") + ylab(expression("R-factor [MJ mm "*~ha^-1~h^-1~yr^-1*"]")) +
  scale_x_discrete(labels=pnames) + scale_y_continuous(trans="sqrt") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text=element_text(colour="black", size = 10)) +
  labs(title="(b) Annual R-factor (bias corrected)")
#bcplot
grob <- ggarrange(rawplot, bcplot, ncol=1)
grob

ggsave("graphs/ann_Rfactor.png", plot = grob, 
       width = 23, height = 18, units = "in", dpi = 350, scale = 0.4)
