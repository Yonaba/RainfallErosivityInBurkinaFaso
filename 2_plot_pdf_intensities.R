setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(ggplot2)
library(ggpubr)
library(randomcoloR)

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

csvs <- lapply(products,function(product)  {
  return (read.csv(paste0("processing/0_hourly/",product,"_pr_bf_hourly.csv"),header = T, sep = ",", dec = "."))
})
names(csvs) <- products

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name
#cols <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
cols <- distinctColorPalette(length(products))

i <- 0
pl.list <- list()

for (s in stations) {
  i <- i + 1
  #s <- "OUAGADOUGOU"
  cdf <- data.frame(matrix(nrow=0, ncol=2))
  colnames(cdf) <- c("reanalysis","values")
  
  for (product in products) {
    #product <- "imerg"
    df <- csvs[[product]]
    is <- df[,s]
    is <- is[is>0]
    is <- data.frame(reanalysis = rep(product,length(is)), values = is)
    cdf <- rbind(cdf, is)
  }
  
  pl.list[[i]] <- ggplot(cdf, aes(x=values, colour=reanalysis)) + stat_ecdf(geom="step") +
    # geom_density(alpha=0.3) + 
    xlim(0,15) +
    xlab(ifelse(i>=6,expression("i"[60]~"[mm/h]"),"")) +
    ylab(ifelse(i==1 | i==6, "ecdf [-]","")) +
    labs(title=paste0(letters[i],") ",s)) +
    theme_bw()

}

grob <- ggarrange(plotlist = pl.list, ncol=5, nrow = 2, common.legend = T, legend = "bottom")
ggsave(filename = paste0("graphs/density_intensity.png"), 
       grob, width = 25, height = 10, dpi = 400,scale = 0.6)
