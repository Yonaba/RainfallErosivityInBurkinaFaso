setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(ggridges)
library(ggplot2)
library(ggpubr)

pnames <- c("CMORPH", "ERA5", "IMERG", "MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")
names(pnames) <- c("cmorph", "era5", "imerg","merra2", "pdirnow", "persiann", "persiannccs")

cols <- c("#E6AB02","#E31A1C","#FB8072","#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")

plot_int_dur <- function(type, plot_title_int, plot_title_dur, label_x) {
  #folder <- "0_hourly"
  products <- list.files(path="processing/0_hourly")
  products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
  int.df <- dur.df <- data.frame(matrix(nrow=0, ncol=2))
  
  for (product in products) {
    #product <- products[1]
    df <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_",type,".csv"),header = T, sep = ",", dec = ".")
    idf <- data.frame(prod = rep(product, length=nrow(df)), val = df$I_mmh)
    ddf <- data.frame(prod = rep(product, length=nrow(df)), val = df$D_h)
    int.df <- rbind(int.df, idf)
    dur.df <- rbind(int.df, ddf)
  }
  
  int.df$prod <- as.factor(int.df$prod)
  dur.df$prod <- as.factor(dur.df$prod)
  
  int.df$prod <- pnames[int.df$prod]
  dur.df$prod <- pnames[dur.df$prod]
  
  pl_int <- ggplot(int.df,aes(x = val, y = prod, fill = prod)) +
    geom_density_ridges(quantile_lines = T, quantile_fun = median, rel_min_height = 0.005) +
    xlim(0,10) +
    scale_fill_manual(values = cols, labels = as.character(pnames)) +
    theme_ridges() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab(ifelse(label_x,"\nRainfall intensity [mm/h]","")) +
    ylab("Assigned Probability (%)\n") + labs(title = plot_title_int)
  
  pl_dur <- ggplot(dur.df,aes(x = val, y = prod, fill = prod)) +
    geom_density_ridges(quantile_lines = T, quantile_fun = median, rel_min_height = 0.005) +
    xlim(0,7.5) +
    scale_fill_manual(values = cols, labels = as.character(pnames)) +    
    theme_ridges() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)
    ) +
    xlab(ifelse(label_x,"\nRainfall duration [h]","")) +
    ylab("") + labs(title = plot_title_dur)
  
  return (list(pl_int, pl_dur))
}

raw <- plot_int_dur("raw", "(a) Rainfall intensity (raw)", "(b) Rainfall duration (raw)", label_x = F)
bc <- plot_int_dur("bc","(c) Rainfall intensity (bc)", "(d) Rainfall duration (bc)", label_x = T)

plotlist <-  list(raw[[1]],raw[[2]],bc[[1]],bc[[2]])
grob <- ggarrange(plotlist = plotlist, nrow=2, ncol=2)
grob
ggsave("graphs/cmp_int_dur.png", plot = grob, bg = "white",
       width = 17, height = 17, units = "in", dpi = 350, scale = 0.5)

print("finished.")

