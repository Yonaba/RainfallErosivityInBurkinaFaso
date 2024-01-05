setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(stringr)
library(lubridate)
library(ggplot2)
library(ggpubr)

product <- "imerg"

df30 <- read.csv(paste0("processing/0_30mn/",product,"_pr_bf_30mn.csv"),header = T, sep = ",", dec = ".")
df60 <- read.csv(paste0("processing/0_hourly/",product,"_pr_bf_hourly.csv"),header = T, sep = ",", dec = ".")
df30$datetime <- as.POSIXct(as.character(df30$datetime),format="%Y-%m-%d %H:%M",tz="UTC")
df60$datetime <- as.POSIXct(as.character(df60$datetime),format="%Y-%m-%d %H:%M",tz="UTC")

df30$datetime <- date(df30$datetime)
df60$datetime <- date(df60$datetime)
df30 <- aggregate(.~datetime, df30, max)
df60 <- aggregate(.~datetime, df60, max)

stations <- colnames(df30)[-1]

plist <- list()
i <- 0
dcoef <- data.frame(matrix(nrow=0, ncol=2))
colnames(dcoef) <- c("station", "coef")
for (s in stations) {
  i <- i + 1
  #s <- "OUAGADOUGOU"
  df30s <- df30[,c("datetime",s)]
  df60s <- df60[,c("datetime",s)]
  df <- merge(df30s, df60s, by = "datetime", all.x = T)
  df[df == 0] <- NA
  colnames(df) <- c("datetime","i30", "i60")
  df <- df[!is.na(df$i30) & !is.na(df$i60),]
  
  lmeq <- lm(i30~i60 + 0, data=df)
  dcoef[i,] <- c(s,as.numeric(lmeq$coefficients[1]))
  
  # Hack ggpubr:::.stat_lm: 
  # https://stackoverflow.com/questions/66177005/im-using-stat-regline-equation-with-ggscatter-is-there-a-way-to-specify-the-si
  # trace(ggpubr:::.stat_lm, edit = TRUE)
  pl <- ggscatter(
    df, x = "i60", y = "i30", size = 0.5, add = "reg.line",
    title = paste0(letters[i],") ",s),
    add.params = list(linetype = "dashed", color = "blue"), 
    conf.int = T, conf.int.level = 0.9, cor.method = "pearson",legend = "bottom"
  ) + theme_bw() +
    theme(axis.text=element_text(colour="black")) +
    xlab(ifelse(i>5,expression("i"[60]~"[mm/h]"),"")) +
    ylab(ifelse(i==1 | i==6,expression("i"[30]~"[mm/h]"),"")) +
    xlim(0,100) + ylim(0,160) +
    stat_regline_equation(formula=y~x-1, label.y = 150, fullrange = T) +
    stat_cor(method = "pearson", aes(label = paste(..rr.label.., ..p.label.., sep="~`,`~")), label.y = 135)
  
  plist[[i]] <- pl
}

grob <- ggarrange(plotlist = plist, ncol=5, nrow = 2)
ggsave(filename = paste0("graphs/i30_i60_imerg.png"), 
       grob, width = 25, height = 10, dpi = 400,scale = 0.6)

write.csv(dcoef, file = paste0("processing/R_factor/coef_i30_i60_bf.csv"), row.names = F)  
print("finished")
