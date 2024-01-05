setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(zoo)
library(stringr)
library(lubridate)
library(modifiedmk)
library(patchwork)
library(ggpubr)

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")$Name

product <- "imerg"
type <- "bc"

Pann <- read.csv(paste0("processing/3_yearly/",product,"_pr_bf_yearly.csv"),header = T, sep = ",", dec = ".")
colnames(Pann)[1] <- "Year"

Rdf <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_",type,".csv"),header = T, sep = ",", dec = ".")
Rdf <- aggregate(R~Station + Year, Rdf, sum)

raincol <- "cornflowerblue"
rfcol <- "darkred"

lt <- 0
plotlist <- list()
for (station in stations){
  lt <- lt + 1
  #station <- stations[8]
  df <- data.frame(year = 2001:2020, Pann = Pann[,station], Rann =  Rdf[Rdf$Station==station,"R"])
  Pannmk <- sprintf("%.3f",tfpwmk(df$Pann)[c(2,4)])
  Rannmk <- sprintf("%.3f",tfpwmk(df$Rann)[c(2,4)])
  
  plotlist[[lt]] <- ggplot(df, aes(x=year, y= Pann)) +
    geom_bar(stat = "identity", width = 0.8, fill=raincol) +
    geom_line(data = df, aes(x=year, y = Rann/7), inherit.aes=F, lwd = 1,col=rfcol) +
    geom_point(data = df, aes(x=year, y = Rann/7), inherit.aes=F, size = 2, col=rfcol) + 
    geom_smooth(data = df, aes(x=year, y = Rann/7), inherit.aes=F, method = "lm", se = F, linetype="dashed", col="black") +
    annotate("text",x=2000, y=1800, hjust=0, 
             label = paste0("M-K p: ",Pannmk[2],", Sen's slope: ",Pannmk[1]), col=raincol, fontface="bold") +
    annotate("text",x=2000, y=1700, hjust=0, 
             label = paste0("M-K p: ",Rannmk[2],", Sen's slope: ",Rannmk[1]), col=rfcol, fontface="bold") +    
    xlab("") +
    scale_y_continuous(
      name=ifelse(((lt==1) || (lt ==6)),"Annual Rainfall [mm]",""), limits = c(0,1800),
      breaks = seq(0,1800,600), labels = seq(0,1800,600),
      sec.axis = sec_axis(~.*7,name=ifelse(((lt==5) || (lt == 10)),expression(bold("R-factor [MJ mm "*~ha^-1~h^-1~yr^-1*"]")),""))
      ) +
    labs(title = paste0(letters[lt],") ",station)) +
    theme_bw() +
    theme(
      axis.text=element_text(colour="black"),
      axis.text.y = element_text(color = raincol,face="bold"),
      axis.title.y.left = element_text(color = raincol, face="bold"),
      axis.line.y.left = element_line(color = raincol),
      axis.ticks.y.left = element_line(color = raincol),
      axis.title.y.right = element_text(angle = 90, color = rfcol),
      axis.text.y.right = element_text(color = rfcol,face="bold"),
      axis.line.y.right = element_line(color = rfcol),
      axis.ticks.y.right = element_line(color = rfcol)
    )
      
}

grob <- wrap_plots(plotlist = plotlist, ncol = 5)
grob

ggsave("graphs/Trends_Pann_Rann.png", plot = grob, 
       width = 30, height = 12, units = "in", dpi = 350, scale = 0.7)

