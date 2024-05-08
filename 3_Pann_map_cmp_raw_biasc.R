setwd("D:/Recherche/Article_Erosion_2023/analysis")
Sys.setenv(TZ = "UTC")

library(tidyverse)
library(sf)
library(ggplot2)
library(gstat)
library(stars)
library(ggpubr)
library(MetBrewer)
library(patchwork)
library(ggsn)
library(ggthemes)
library(paletteer)

write_aggregate_timescale <- function(product, folder) {
  #product<- "persiann"
  #folder <- "0_hourly"
  #fend <- ifelse(folder=="0_hourly_bc","_pr_bf_hourly12.7.csv","_pr_bf_hourly.csv")
  fend <- "_pr_bf_hourly.csv"
  df <- read.csv(paste0("processing/",folder,"/",product,fend),header = T, sep = ",", dec = ".")
  df$datetime <- dtime <- as.POSIXct(df$datetime,format="%Y-%m-%d %H:%M:%S",tz="UTC")
  
  df$datetime <- as.Date(df$datetime, format = "%Y-%m-%d")
  daily.df <- aggregate(. ~ datetime, df, sum)
  tdaily <- data.frame(datetime = seq(as.POSIXct("2001-01-01"), as.POSIXct("2020-12-31"), by="day"))
  daily.df <- merge(tdaily, daily.df, by = "datetime", all = T)
  #daily.df[,2:ncol(daily.df)][daily.df[,2:ncol(daily.df)]<min_rf] <- 0
  
  year.df <- daily.df
  year.df$datetime <- strftime(year.df$datetime, format = "%Y", tz = "UTC")
  year.df <- aggregate(. ~ datetime, year.df, sum)
  
  return (year.df)
}

write_append_pann <- function(stations, products, folder, suffix, ref) {
  cnames <- colnames(stations)
  lproducts <- c(ref,products[products!=ref])
  if (folder == "0_hourly_bc") lproducts <- lproducts[-1]
  for (product in lproducts) {
    #product <- "merra2"
    print(paste0("Reading ",product))
    ffolder <- ifelse(product == ref,"0_hourly_bc",folder)    
    pann <- write_aggregate_timescale(product, ffolder)
    pann$datetime <- NULL
    pann <- colMeans(pann)
    stations <- cbind(stations, pann)
  }
  
  ccnames <- c(ref, paste0(products[products!=ref], suffix))
  if (folder == "0_hourly_bc") ccnames <- ccnames[-1]  
  cnames <- c(cnames, ccnames)
  colnames(stations) <- cnames
  return (stations)
}

ref.product <- "imerg"
obs.day <- read.csv(paste0("input/pr_obs_daily.csv"),header = T, sep = ",", dec = ".")
#obs.day[,2:ncol(obs.day)][obs.day[,2:ncol(obs.day)]<min_rf] <- 0
obs.day$datetime <- as.POSIXct(obs.day$datetime,format="%m/%d/%Y",tz="UTC")

obs.ann <- obs.day
obs.ann$datetime <- strftime(obs.ann$datetime, format = "%Y")
obs.ann <- aggregate(. ~ datetime, obs.ann, sum)
obs.ann$datetime <- NULL

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")
stations$Rfactor <- stations$Ann_rf <- NULL
stations$ann <- colMeans(obs.ann)

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

stations <- write_append_pann(stations, products, "0_hourly", "_raw", ref.product)
stations <- write_append_pann(stations, products, "0_hourly_bc", "_bc", ref.product)

bfshp <- read_sf("maps/bf.shp")

crs <- st_crs("EPSG:4326")
st_as_sf(stations, crs = "EPSG:4326", coords = c("Longitude", "Latitude")) |>
  st_transform(crs) -> sp.df

st_bbox(bfshp) |> st_as_stars(dx = 0.05) |> st_crop(bfshp) -> grd

interp <- sapply(tail(colnames(stations),14),function(x) NULL)

for (i in names(interp)) {
  print(paste0("IDW ",i))
  interp[[i]] <- idw(as.formula(paste0(i,"~1")), sp.df, grd, idp = 2)
}

lnames <- names(interp)
lnames <- append(lnames,"void",after=2)
interp2 <- sapply(lnames,function(x) NULL)

for (i in names(interp2)) {
  if (is.null(interp[[i]])) {
    interp2[[i]] <- ggplot() + theme_void()
  } else  {
    interp2[[i]] <- interp[[i]]
  }
}

pp <- c("CMORPH", "ERA5","MERRA-2", "PDIR-Now", "PERSIANN", "PERSIANN-CCS")
labels <- c("Observed","IMERG",paste0(pp," (raw)"),paste0(pp," (bc)"))
scalelims <- seq(0,2500,500)
n <- cl <- 0

pl <-list()
for (i in names(interp2)) {
  n <- n + 1
  if (i != "void") {
    cl <- cl + 1
    print(paste0("cl ",cl))
    pl[[n]] <- ggplot() +
      geom_stars(data = interp2[[i]], aes(fill=var1.pred, x=x,y=y)) + 
      xlab("Longitude (°)") +
      ylab("Latitude (°)") +
      geom_sf(data = st_cast(bfshp, "MULTILINESTRING")) +
      geom_sf(data = sp.df, aes(),size = 2.5) +
      #scale_shape_manual(values=c(1, 16)) +
      geom_sf_text(data=sp.df, aes(label=stations$Name), nudge_y=0.25, size = 2.5) +
      scale_fill_gradientn(colors=met.brewer("Homer1"), na.value=NA,
                           breaks = scalelims, labels=scalelims,
                           trans = "sqrt",
                           limits = range(scalelims)) +
      labs(title = paste0("(",letters[cl],") ",labels[cl]),
           fill = expression("Annual rainfall [mm "*yr^-1*"]      ")) + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold"),
            axis.text = element_text(color = "black"),
            legend.position = "bottom") +
      north(data = sp.df, scale=0.2, symbol = 12, anchor = c(x=2.75,y=15)) +
      scalebar(data = sp.df, anchor = c(x=0.5,y=10),
               model = "WGS84", dist = 150, dist_unit = "km", transform = T,
               location = "bottomright",height = 0.05, 
               border.size = 0.2, st.size = 4, st.dist = 0.075)    
  } else {
    pl[[n]] <- interp2[[n]]
  }
  
}

grob <- wrap_plots(pl, ncol=3, byrow=T) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.key.width=unit(1, "in"))

grob

ggsave("graphs/cmp_ann_map.png", plot = grob, 
       width = 23, height = 30, dpi = 400, scale = 0.6)
