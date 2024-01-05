setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]
stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

product <- "merra2"
dfm <- read.csv(paste0("processing/2_monthly/",product,"_pr_bf_monthly.csv"),header = T, sep = ",", dec = ".")
dtime <- seq(as.POSIXct("2001-01-01"),as.POSIXct("2020-12-31"),by="month", tz="UTC")

dfm <- data.frame(dyear = as.numeric(format(dtime,"%Y")),dfm[,2:ncol(dfm)])
dfpi <- data.frame(dyear = dfm$dyear, dfm[,2:ncol(dfm)]^2)
dfpi <- aggregate(.~dyear, dfpi, sum)
dfm <- aggregate(.~dyear, dfm, sum)
mfi <- data.frame(dyear = dfm$dyear, dfpi[,2:ncol(dfpi)]/dfm[,2:ncol(dfm)])
mfi[,2:ncol(mfi)] <- 27.8 * mfi[,2:ncol(mfi)] - 189.2
write.csv(mfi, file = paste0("processing/R_factor/",product,"_MFI_annual.csv"), row.names = F)  

mfi.ave <- colMeans(mfi[,2:ncol(mfi)])

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
  
  pl <- ggscatter(
    df, x = "i60", y = "i30", size = 1, add = "reg.line",
    title = s,
    add.params = list(linetype = "dashed", color = "blue"), 
    conf.int = T,cor.method = "pearson",legend = "bottom"
  ) + 
    xlab(expression("i"[60]~" (mm/h)")) +
    ylab(expression("i"[30]~" (mm/h)")) +
    xlim(0,100) + ylim(0,160) +
    stat_regline_equation(formula=y~x-1, label.y = 150) +
    stat_cor(method = "pearson", aes(label = paste(..rr.label..)), label.y = 140)
  
  plist[[i]] <- pl
}

grob <- ggarrange(plotlist = plist, ncol=3, nrow = 4)
ggsave(filename = paste0("graphs/i30_i60_imerg.png"), 
       grob, width = 16, height = 16, dpi = 400)

write.csv(dcoef, file = paste0("processing/R_factor/coef_i30_i60_bf.csv"), row.names = F)  
print("finished")
