setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(verification)
library(ggplot2)
library(RColorBrewer)

THRESHOLDS <- c(0.1,5,10,15,20,25)

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
obs <- read.csv(paste0("input/pr_obs_daily.csv"),header = T, sep = ",", dec = ".")

stations <- colnames(obs)[-1]

obs$datetime <- NULL
obs <- stack(obs)$values

sim <- data.frame(matrix(nrow=length(obs), ncol=length(products)))
colnames(sim) <- products
for (product in products) {
  #product <- "merra2"
  simdata <- read.csv(paste0("processing/1_daily/",product,"_pr_bf_daily.csv"),header = T, sep = ",", dec = ".")
  simdata$datetime <- NULL
  sim[,product] <- stack(simdata)$values
}
rm(simdata, product)

cnames <- c("TS", "POD", "FAR", "HSS", "PC", "ORSS")
scores <- data.frame(matrix(nrow=0, ncol=length(cnames)))
colnames(scores) <- cnames

for (threshold in THRESHOLDS) {
  #threshold <- 0.1
  print(paste0("Processing threshold: ",threshold," mm"))
  robs <- (obs > threshold)
  
  pscores <- data.frame(matrix(nrow=0, ncol=length(cnames)))
  colnames(pscores) <- cnames  
  for (product in products) {
    #product <- "imerg"
    rsim <- (sim[,product] > threshold)
    ss <- table.stats(robs, rsim, fudge = 0.01, silent = FALSE)  
    vals <- c(ss$TS, ss$POD, ss$FAR, ss$HSS, ss$PC, ss$orss)
    pscores[nrow(pscores)+1,] <- vals
  }
  pscores <- data.frame(reanalysis = rep(products,ncol(pscores)),stack(pscores))
  pscores <- data.frame(threshold = rep(threshold, nrow(pscores)) ,pscores)
  scores <- rbind(scores, pscores)
}

scores$ind <- factor(scores$ind, levels=c("POD", "PC", "FAR","TS","HSS","ORSS"))
full_labels <- c("Probability of Detection (POD)",
                 "Percent Correct (PC)",
                 "False Alarm Ratio (FAR)",
                 "Threat Score (TS)",
                 "Heidke Skill Score (HSS)",
                 "Odds Ratio Skill Score (ORSS)")
ll <- paste0("(",letters[1:6])
labels <- paste(ll,full_labels,sep=") ")
names(labels) <- levels(scores$ind)


ncols <- length(products)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#cols <- sample(col_vector, ncols)
cols <- c("#E6AB02","#E31A1C","#FB8072","#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")

pnames <- c("CMORPH", "ERA5", "IMERG", "MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")

pl <- ggplot(scores, aes(x=threshold, y=values, colour = reanalysis)) +
  geom_line(size=0.5) + geom_point(shape=16,size=1) +
  facet_wrap(. ~ ind, labeller = as_labeller(labels)) +
  xlab("\nDaily rainfall threshold [mm]") +
  ylab("Score") +
  scale_colour_manual(values = cols, labels = as.character(pnames)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(hjust = 0),
        axis.text=element_text(colour="black")) + 
  guides(colour = guide_legend(nrow = 1))

pl

ggsave("graphs/skill_score_products.png", plot = pl, 
       width = 12, height = 8, units = "in", dpi = 400, scale = 0.7)
print("finished.")
