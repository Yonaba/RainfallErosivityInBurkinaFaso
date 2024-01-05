setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

products <- list.files(path="processing/2_monthly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_monthly.csv")[,2]

lfiles.day <- lfiles.mon <- lfiles.ann <- list()
obs.day <- read.csv(paste0("input/pr_obs_daily.csv"),header = T, sep = ",", dec = ".")
obs.mon <- read.csv(paste0("input/pr_obs_monthly.csv"),header = T, sep = ",", dec = ".")
obs.ann <- read.csv(paste0("input/pr_obs_yearly.csv"),header = T, sep = ",", dec = ".")
stations <- colnames(obs.mon)[-1]

lfiles.day[[1]] <- obs.day
lfiles.mon[[1]] <- obs.mon
lfiles.ann[[1]] <- obs.ann
i <- 1

for (product in products) {
  i <- i+1
  lfiles.day[[i]] <- read.csv(paste0("processing/1_daily/",product,"_pr_bf_daily.csv"),
                              header = T, sep = ",", dec = ".")  
  lfiles.mon[[i]] <- read.csv(paste0("processing/2_monthly/",product,"_pr_bf_monthly.csv"),
                          header = T, sep = ",", dec = ".")
  lfiles.ann[[i]] <- read.csv(paste0("processing/3_yearly/",product,"_pr_bf_yearly.csv"),
                              header = T, sep = ",", dec = ".")  
}
names(lfiles.day)  <- names(lfiles.mon) <- names(lfiles.ann) <- c("obs", products)

headers <- c("source", "x","value","station")
df.mon <- df.ann <- data.frame(matrix(nrow=0, ncol=length(headers)))
colnames(df.mon) <- colnames(df.ann) <- headers

df.day <- data.frame(matrix(nrow=0, ncol=length(headers)-1))
colnames(df.day) <- c("source","value","station")

for (source in names(lfiles.mon)) {
  #source <- "cmorph"
  
  dday <- lfiles.day[[source]]
  dmon <- lfiles.mon[[source]]
  dann <- lfiles.ann[[source]]
  
  dmon$datetime <- NULL
  dmon$mon <- rep(1:12,20)
  dmon <- aggregate(.~mon, dmon, mean)
  dday$datetime <- dmon$mon <- dann$datetime <- NULL
  
  dday <- stack(dday)
  dmon <- stack(dmon)
  dann <- stack(dann)
  
  dday <- data.frame(source = rep(source, nrow(dday)),
                     dday)
  dmon <- data.frame(source = rep(source, nrow(dmon)),
                     mon = rep(1:12,length(stations)),
                     dmon)
  dann <- data.frame(source = rep(source, nrow(dann)),
                     year = rep(2001:2020, length(stations)),
                     dann)

  colnames(dann) <- colnames(dmon) <- headers
  colnames(dday) <- colnames(df.day)
  df.day <- rbind(df.day, dday)
  df.mon <- rbind(df.mon, dmon)
  df.ann <- rbind(df.ann, dann)
}

df.day$source <- as.factor(df.day$source)
df.day$source <- factor(df.day$source, levels=c("obs", products))
df.mon$source <- as.factor(df.mon$source)
df.mon$source <- factor(df.mon$source, levels=c("obs", products))
df.ann$source <- as.factor(df.ann$source)
df.ann$source <- factor(df.ann$source, levels=c("obs", products))

dobs <- df.day[df.day$source=="obs",]$value
df.day0 <- data.frame(matrix(nrow=0, ncol=3))
colnames(df.day0) <- c("obs", "mod","source")

for (product in products) {
  #product <- products[[1]]
  dprod <- df.day[df.day$source==product,]$value
  tmp <- data.frame(obs=dobs, mod = dprod, source = rep(product,length(dprod)))
  df.day0 <- rbind(df.day0, tmp)
}
df.day0 <- df.day0[df.day0$obs>0,]

cols <- c("black", "#E6AB02","#E31A1C","#FB8072","#8DA0CB","#A6CEE3","#8DD3C7","#FCCDE5")
pnames <- c("CMORPH", "ERA5", "IMERG", "MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")
names(pnames) <- c("cmorph", "era5", "imerg","merra2", "pdirnow", "persiann", "persiannccs")

df.day0$source <- as.character(pnames[df.day0$source])

pl.day <- ggscatter(df.day0, x ="obs", y = "mod", facet.by = "source", add = "reg.line", 
                    conf.int = T, conf.int.level = 0.9,
                    add.params = list(color = "blue",size=0.5, fill = "lightgray"),
                    shape = 1,size = 0.5, cor.method = "spearman",
                    cor.coef = T,
                    cor.coeff.args = list(method = "spearman", label.y = 200, label.x = 150, label.sep = "\n"),
                    xlab = "Synoptic gauge observations [mm]", 
                    ylab = "Rainfall product data [mm]\n") + 
  xlim(0,270) + ylim(0,270) + theme_bw() +
  theme(axis.text=element_text(colour="black"))

ggsave("graphs/cmp_day.png", plot = pl.day, 
       width = 20, height = 20, units = "in", dpi = 400, scale = 0.4)

pnames <- c("OBS",pnames)
#names(pnames) <- c("obs","cmorph", "era5", "imerg","merra2", "pdirnow", "persiann", "persiannccs")

pl.mon <- ggplot(df.mon, aes(x=x, y = value, color = source)) +
  geom_line() +
  xlim(0,12) + ylim(0,600) +
  facet_wrap(.~station, ncol=5) + 
  xlab("") + ylab("Rainfall [mm]\n") +
  scale_color_manual(values=cols, labels = as.character(pnames)) +
  scale_size_manual(values=c(1.2, rep(0.5,length(products)))) +
  scale_x_continuous(breaks=1:12, labels=month.abb) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text=element_text(colour="black")) +
  guides(colour = guide_legend(nrow = 1))

pl.mon
ggsave("graphs/cmp_mon.png", plot = pl.mon, 
       width = 25, height = 12.5, units = "in", dpi = 400, scale = 0.5)

pl.ann <- ggplot(df.ann, aes(x=x, y = value, color = source)) +
  geom_line() +
  xlim(2001,2020) + ylim(200,2500) +
  facet_wrap(.~station, ncol=5) +
  xlab ("") + ylab("Annual rainfall [mm]\n") +
  scale_color_manual(values=cols, labels = as.character(pnames)) +
  scale_size_manual(values=c(1.2, rep(0.5,length(products)))) +
  #scale_x_continuous(breaks=2001:2020, labels=month.abb) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text=element_text(colour="black")) +
  guides(colour = guide_legend(nrow = 1))

pl.ann
ggsave("graphs/cmp_ann.png", plot = pl.ann, 
       width = 35, height = 14, units = "in", dpi = 400, scale = 0.4)

print("finished.")
