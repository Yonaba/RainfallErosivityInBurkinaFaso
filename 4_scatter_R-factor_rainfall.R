setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(patchwork)
library(ggpubr)

#stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

product <- "imerg"
type <- "bc"
df <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_",type,".csv"),header = T, sep = ",", dec = ".")
#df <- df[-4937,]

daily.df <- data.frame(ev = df$P_mm, rf = df$R)

monthly.df <- aggregate(R~Month+Year, df, sum)
monthly.df$R <- monthly.df$R/10
monthly.df <- data.frame(datetime = paste0(monthly.df$Year,"-",sprintf("%02d",monthly.df$Month)),
                         rf = monthly.df$R)

mrf <- read.csv(paste0("processing/2_monthly/",product,"_pr_bf_monthly.csv"),header = T, sep = ",", dec = ".")
mrf <- data.frame(datetime = mrf$datetime, pr = rowMeans(mrf[,-1]))
monthly.df <- merge(monthly.df, mrf, by="datetime",all.x=T)

ann.df <- aggregate(R~Year, df, sum)
ann.df$R <- ann.df$R/10
colnames(ann.df)[1] <- "datetime"
annrf <- read.csv(paste0("processing/3_yearly/",product,"_pr_bf_yearly.csv"),header = T, sep = ",", dec = ".")
annrf <- data.frame(datetime = annrf$datetime, pr = rowMeans(annrf[,-1]))
ann.df <- merge(ann.df, annrf, by="datetime",all.x=T)


fm0 <- lm(log(rf) ~ log(ev), daily.df)
reg_coef <- list(a = round(exp(coef(fm0)[1]),4), b = round(coef(fm0)[2],4))
r2 <- round(as.numeric(cor.test(daily.df$rf, reg_coef$a*daily.df$ev^reg_coef$b)$estimate), 2)

pl_daily <- ggplot(daily.df,aes(x = ev, y = rf)) +
  geom_point() + 
  stat_smooth(method = 'nls', formula = 'y~a*x^b', 
              method.args = list(start = reg_coef), se=F) +
  annotate(geom="text",x = 15, y = 6000, hjust=0, fontface="italic",
           label = bquote("y ="~.(reg_coef$a)~"x"^.(reg_coef$b)*","~~~"R"^2*" = "*.(r2))) +
  xlab("\nDaily rainfall [mm]") + ylab(expression("R-factor [MJ mm "*~ha^-1~h^-1~day^-1*"]")) +
  labs(title = "(a) Daily event timescale") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"))

pl_daily

fm0 <- lm(log(rf) ~ log(pr), monthly.df)
reg_coef <- list(a = round(exp(coef(fm0)[1]),4), b = round(coef(fm0)[2],4))
r2 <- round(as.numeric(cor.test(monthly.df$rf, reg_coef$a*monthly.df$pr^reg_coef$b)$estimate), 2)

pl_monthly <- ggplot(monthly.df,aes(x = pr, y = rf)) +
  geom_point() + 
  stat_smooth(method = 'nls', formula = 'y~a*x^b', 
              method.args = list(start = reg_coef), se=T)
  annotate(geom="text",x = 15, y = 2000, hjust=0, fontface="italic",
           label = bquote("y ="~.(reg_coef$a)~"x"^.(reg_coef$b)*","~~~"R"^2*" = "*.(r2))) +
  xlab("\nMonthly rainfall [mm]") + ylab(expression("R-factor [MJ mm "*~ha^-1~h^-1~month^-1*"]")) +
  labs(title = "(b) Monthly timescale") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"))

pl_monthly


fm0 <- lm(log(R) ~ log(pr), ann.df)
reg_coef <- list(a = round(exp(coef(fm0)[1]),4), b = round(coef(fm0)[2],4))
r2 <- round(as.numeric(cor.test(ann.df$R, reg_coef$a*ann.df$pr^reg_coef$b)$estimate), 2)

pl_annual <- ggplot(ann.df,aes(x = pr, y = R)) +
  geom_point() + 
  stat_smooth(method = 'nls', formula = 'y~a*x^b',
              method.args = list(start = reg_coef), se=F) +
  annotate(geom="text",x = 700, y = 4500, hjust=0, fontface="italic",
           label = bquote("y ="~.(reg_coef$a)~"x"^.(reg_coef$b)*","~~~"R"^2*" = "*.(r2))) +
  xlab("\nAnnual rainfall [mm]") + ylab(expression("R-factor [MJ mm "*~ha^-1~h^-1~yr^-1*"]")) +
  labs(title = "(c) Annual timescale") +
  theme_bw() +
  theme(axis.text=element_text(colour="black"))

pl_annual

grob <- pl_daily + pl_monthly + pl_annual +  plot_layout(ncol = 3)
grob
ggsave("graphs/Rfactor_Rainfall.png", plot = grob, 
       width = 27.5, height = 10, units = "in", dpi = 350, scale = 0.5)
