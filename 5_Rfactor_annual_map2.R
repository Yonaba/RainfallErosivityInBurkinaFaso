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

add_Rfactor <- function(products, stations, ref) {
  colnames(stations)[2] <- "Station"  
  df.R.raw <- df.R.bc <- data.frame(Station = stations$Station)
  lproducts <- c(ref,products[products!=ref])
  for (product in lproducts) {
    #product <- products[3]
    print(product)
    df.raw <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_","raw",".csv"),header = T, sep = ",", dec = ".")
    df.bc <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_","bc",".csv"),header = T, sep = ",", dec = ".")
    Ragg.raw <- aggregate(R~Station, df.raw, sum)
    Ragg.bc <- aggregate(R~Station, df.bc, sum)
    Ragg.raw$R <- Ragg.raw$R/20
    Ragg.bc$R <- Ragg.bc$R/20
    df.R.raw <- merge(df.R.raw, Ragg.raw, by="Station",all.x=T)
    df.R.bc <- merge(df.R.bc, Ragg.bc, by="Station",all.x=T)
  }
  colnames(df.R.raw) <- c("Station",paste0(lproducts,".raw"))
  colnames(df.R.bc) <- c("Station",paste0(lproducts,".bc"))
  df.R.raw$Station <- df.R.bc$Station <- NULL
  stations <- cbind(stations, df.R.raw, df.R.bc)
  stations[,7] <- stations[,14]
  stations[,14] <- NULL
  colnames(stations)[7] <- ref
  return (stations)
}

ref.product <- "imerg"
stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")
stations$Ann_rf <- NULL
colnames(stations)[6] <- "GloREDa"

products <- list.files(path="processing/0_hourly")
products <- str_match(products,"^([a-z0-9]*)_pr_bf_hourly.csv")[,2]

stations <- add_Rfactor(products, stations, ref.product)

bfshp <- read_sf("maps/bf.shp")

crs <- st_crs("EPSG:4326")
st_as_sf(stations, crs = "EPSG:4326", coords = c("Longitude", "Latitude")) |>
  st_transform(crs) -> sp.df

st_bbox(bfshp) |> st_as_stars(dx = 0.05) |> st_crop(bfshp) -> grd


plist <- tail(colnames(stations),14)
interp <- sapply(plist,function(x) NULL)

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
labels <- c("GloREDa","IMERG",paste0(pp," (raw)"),paste0(pp," (bc)"))
scalelims <- seq(0,20000,2000)
n <- cl <- 0

pl <-list()
for (i in names(interp2)) {
  n <- n + 1
  if (i != "void") {
    cl <- cl + 1
    print(paste0("cl ",cl))
    pl[[n]] <- ggplot() +
    #ggplot() +
      geom_stars(data = interp2[[i]], aes(fill=var1.pred, x=x,y=y)) + 
      xlab("Longitude (°)") +
      ylab("Latitude (°)") +
      geom_sf(data = st_cast(bfshp, "MULTILINESTRING")) +
      geom_sf(data = sp.df, aes(),size = 2.5) +
      #scale_shape_manual(values=c(1, 16)) +
      geom_sf_text(data=sp.df, aes(label=stations$Station), nudge_y=0.25, size = 2.5) +
      scale_fill_gradientn(colors=met.brewer("Peru2"), na.value=NA,
                           trans = "sqrt",
                           breaks = scalelims, labels=scalelims,
                           limits = range(scalelims)) +
      labs(title = paste0("(",letters[cl],") ",labels[cl]),
           fill = expression("R-factor [MJ mm "*~ha^-1~h^-1~yr^-1*"]        ")) + 
      theme_bw() +
      theme(plot.title = element_text(face = "bold"),
            axis.text = element_text(color = "black"),
            legend.position = "bottom",
            legend.text = element_text(angle=90)) +
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

#grob

ggsave("graphs/ann_Rfactor_cmp_raw_biasc.png", plot = grob, 
       width = 22, height = 30, dpi = 400, scale = 0.6)
