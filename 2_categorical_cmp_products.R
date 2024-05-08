setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(verification)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

THRESHOLDS <- c(12.7,15,17.5, 20, 22.5, 25)

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
obs <- read.csv(paste0("input/pr_obs_daily.csv"),header = T, sep = ",", dec = ".")

write_aggregate_timescale <- function(product, folder) {
  df <- read.csv(paste0("processing/",folder,"/",product,"_pr_bf_hourly.csv"),header = T, sep = ",", dec = ".")
  df$datetime <- dtime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
  
  df$datetime <- as.Date(df$datetime, format = "%Y-%m-%d")
  daily.df <- aggregate(. ~ datetime, df, sum)
  tdaily <- data.frame(datetime = seq(as.POSIXct("2001-01-01"), as.POSIXct("2020-12-31"), by="day"))
  daily.df <- merge(tdaily, daily.df, by = "datetime", all = T)
  return (daily.df)
}

stations <- colnames(obs)[-1]

obs$datetime <- NULL
obs[obs<min(THRESHOLDS)] <- 0
obs <- stack(obs)$values

sim <- simbc <- data.frame(matrix(nrow=length(obs), ncol=length(products)))
colnames(sim) <- colnames(simbc) <- products

for (product in products) {
  #product <- "merra2"
  print(paste0("Processing product: ",product))
  simdata <- write_aggregate_timescale(product, "0_hourly")
  simdatabc <- write_aggregate_timescale(product, "0_hourly_bc")
  
  simdata$datetime <- simdatabc$datetime <- NULL
  simdata[simdata<min(THRESHOLDS)] <- 0 
  simdatabc[simdatabc<min(THRESHOLDS)] <- 0 
  
  sim[,product] <- stack(simdata)$values
  simbc[,product] <- stack(simdatabc)$values
}
rm(simdata, simdatabc, product)

cnames <- c("POD", "FAR", "TS")
scores <- scoresbc <- data.frame(matrix(nrow=0, ncol=length(cnames)))
colnames(scores) <- colnames(scoresbc) <-cnames

for (threshold in THRESHOLDS) {
  #threshold <- 12.7
  print(paste0("Processing threshold: ",threshold," mm"))
  robs <- (obs > threshold)
  
  pscores <- pscoresbc <- data.frame(matrix(nrow=0, ncol=length(cnames)))
  colnames(pscores) <- colnames(pscoresbc) <- cnames  
  for (product in products) {
    #product <- "imerg"
    rsim <- (sim[,product] > threshold)
    rsimbc <- (simbc[,product] > threshold)
    ss <- table.stats(robs, rsim, fudge = 0.01, silent = FALSE) 
    ssbc <- table.stats(robs, rsimbc, fudge = 0.01, silent = FALSE) 
    vals <- c(ss$POD, ss$FAR, ss$TS)
    valsbc <- c(ssbc$POD, ssbc$FAR, ssbc$TS)
    pscores[nrow(pscores)+1,] <- vals
    pscoresbc[nrow(pscoresbc)+1,] <- valsbc
  }
  
  pscores <- data.frame(reanalysis = rep(paste(products," (raw)"),ncol(pscores)),stack(pscores))
  pscoresbc <- data.frame(reanalysis = rep(paste(products," (bc)"),ncol(pscoresbc)),stack(pscoresbc))
  
  pscores <- data.frame(threshold = rep(threshold, nrow(pscores)) ,pscores)
  pscoresbc <- data.frame(threshold = rep(threshold, nrow(pscoresbc)) ,pscoresbc)
  
  scores <- rbind(scores, pscores)
  scoresbc <- rbind(scoresbc, pscoresbc)
}

scores$ind <- factor(scores$ind, levels=c("POD", "FAR","TS"))
scoresbc$ind <- factor(scoresbc$ind, levels=c("POD", "FAR","TS"))

full_labels <- c("POD","FAR","TS")
labels_top <- paste(paste0("(",letters[1:3]),full_labels,sep=") ")
labels_bottom <- paste(paste0("(",letters[4:6]),full_labels,sep=") ")

names(labels_top) <- levels(scores$ind)
names(labels_bottom) <- levels(scoresbc$ind)

#ncols <- length(products)
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# #cols <- sample(col_vector, ncols)

cols <- c("#E6AB02","#FB8072","#E31A1C", "#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")
pnames <- c("CMORPH", "ERA5", "IMERG","MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")

pl_top <- ggplot(scores, aes(x=threshold, y=values, colour = reanalysis)) +
  geom_line(size=1) + geom_point(shape=16,size=2) +
  facet_wrap(. ~ ind, labeller = as_labeller(labels_top)) +
  xlab("") +
  ylab("Score\n") +
  scale_colour_manual(values = cols, labels = as.character(pnames)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(colour = "black",size=12),           
        strip.text.x = element_text(hjust = 0, size = 14, face = "bold"),
        axis.text=element_text(colour="black", size = 12),
        axis.title=element_text(colour="black", size = 12)) +
  guides(colour = guide_legend(nrow = 1))

pl_bottom <- ggplot(scoresbc, aes(x=threshold, y=values, colour = reanalysis)) +
  geom_line(size=1) + geom_point(shape=16,size=2) +
  facet_wrap(. ~ ind, labeller = as_labeller(labels_bottom)) +
  xlab("\nDaily rainfall thresholds [mm]") +
  ylab("Score\n") +
  scale_colour_manual(values = cols, labels = as.character(pnames)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(colour = "black",size=12),           
        strip.text.x = element_text(hjust = 0, size = 14, face = "bold"),
        axis.text=element_text(colour="black", size = 12),
        axis.title=element_text(colour="black", size = 12)) +
  guides(colour = guide_legend(nrow = 1))

grob <- ggarrange(pl_top, pl_bottom, ncol=1, nrow=2, common.legend = TRUE, legend="bottom")
grob

ggsave("graphs/skill_score_products.png", plot = grob, 
       width = 18, height = 12, units = "in", dpi = 400, scale = 0.7)
print("finished.")
