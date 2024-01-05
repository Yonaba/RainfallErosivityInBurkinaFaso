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

stations <- read.csv("input/bf_stations.csv",header = T, sep = ",", dec = ".")

product <- "imerg"
df.bc <- read.csv(paste0("processing/R_factor/",product,"_Rfactor_bf_","bc",".csv"),header = T, sep = ",", dec = ".")
Ragg.bc <- aggregate(R~Station, df.bc, sum)
Ragg.bc$R <- Ragg.bc$R/20
colnames(Ragg.bc) <- c("Name","Rimerg")
stations <- merge(stations, Ragg.bc, by="Name", all.x=T)

bfshp <- read_sf("maps/bf.shp")

crs <- st_crs("EPSG:4326")
st_as_sf(stations, crs = "EPSG:4326", coords = c("Longitude", "Latitude")) |>
  st_transform(crs) -> sp.df

st_bbox(bfshp) |> st_as_stars(dx = 0.05) |> st_crop(bfshp) -> grd
interp.idw.Rfactor <- idw(Rfactor~1, sp.df, grd, idp = 2)
interp.idw.Rimerg <- idw(Rimerg~1, sp.df, grd, idp = 2)

scalelims <- seq(1500,5000,500)
glored <- ggplot() +
  geom_stars(data = interp.idw.Rfactor, aes(fill=var1.pred, x=x,y=y)) + 
  xlab("Longitude (°)") +
  ylab("Latitude (°)") +
  geom_sf(data = st_cast(bfshp, "MULTILINESTRING")) +
  geom_sf(data = sp.df, aes(),size = 2) +
  #scale_shape_manual(values=c(1, 16)) +
  geom_sf_text(data=sp.df, aes(label=stations$Name), nudge_y=0.25, size = 2.2) +
  scale_fill_gradientn(colors=met.brewer("OKeeffe2"), na.value=NA,
                       breaks = scalelims, labels=scalelims,
                       limits = range(scalelims)) +
  labs(title = "(a) GloREDa R-factor",
       fill = expression("R-factor [MJ mm "*ha^-1~h^-1~yr^-1*"]      ")) + 
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom") +
  north(data = sp.df, scale=0.2, symbol = 12, anchor = c(x=2.75,y=15)) +
  scalebar(data = sp.df, anchor = c(x=2,y=10),
           model = "WGS84",  dist_unit = "km", transform = TRUE,
           location = "bottomright",
           height = 0.05, dist = 150, border.size = 0.4, st.size = 4, st.dist = 0.075)

imerg <- ggplot() +
  geom_stars(data = interp.idw.Rimerg, aes(fill=var1.pred, x=x,y=y)) + 
  xlab("Longitude (°)") +
  ylab("") +
  geom_sf(data = st_cast(bfshp, "MULTILINESTRING")) +
  geom_sf(data = sp.df, aes(),size = 2) +
  #scale_shape_manual(values=c(1, 16)) +
  geom_sf_text(data=sp.df, aes(label=stations$Name), nudge_y=0.25, size = 2.2) +
  scale_fill_gradientn(colors=met.brewer("OKeeffe2"), na.value=NA,
                       breaks = scalelims, labels=scalelims,
                       limits = range(scalelims)) +
  labs(title = "(b) IMERG R-factor",
       fill = expression("R-factor [MJ mm "*ha^-1~h^-1~yr^-1*"]      ")) + 
  theme_bw() +
  theme(plot.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom") +
  north(data = sp.df, scale=0.2, symbol = 12, anchor = c(x=2.75,y=15)) +
  scalebar(data = sp.df, anchor = c(x=2,y=10),
           model = "WGS84",  dist_unit = "km", transform = TRUE,
           location = "bottomright",
           height = 0.05, dist = 150, border.size = 0.4, st.size = 4, st.dist = 0.075)

grob <- glored + imerg + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", legend.key.width=unit(0.1,"npc"))

ggsave("graphs/Rfactor_map.png", plot = grob, 
       width = 25, height = 14, units = "in", dpi = 350, scale = 0.5)
