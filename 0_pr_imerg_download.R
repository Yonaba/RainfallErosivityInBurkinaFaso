setwd("D:/imerg/")
Sys.setenv(TZ = "UTC")

library(ncdf4)

fpath <- "3B-HHR.MS.MRG.3IMERG.20010101-S003000-E005959.0030.V06B.HDF5.SUB.NC4"
nc <- nc_open(fpath)
nc_close(nc)

nlen <- length(files)

for (i in 1:nlen) {
  i <- 1
  download.file(files[i], destfile=paste0("data/imerg/",i,".nc4"), method = "wget", quiet = T)
  #print(paste0("Year: ", yr))
  
  for (j in tseq) {
    #j <- tseq[25]
    cdate <- as.Date(j)
    yr <- year(cdate)
    mon <- sprintf("%02d", month(cdate))
    day <- sprintf("%02d",day(cdate))
    nc_url <- paste0(base_url,yr,"/",mon,"/",day,"/")
    
    for (h in 0:23) {
          
      #h <- 5
      hh <- sprintf("%02d",h)
      cur_nc <- paste0(base_nc, yr,mon,day,hh,".nc")
      full_url <- paste0(nc_url, cur_nc)
      if (!(file.exists(paste0("data/cmorph/",yr))))
        dir.create(paste0("data/cmorph/",yr))
      if (!(file.exists(paste0("data/cmorph/",yr,"/",cur_nc)))) {
        print(paste0("Day: ", cdate, " - hour : ", h))  
        download.file(full_url, paste0("data/cmorph/",yr,"/",cur_nc), method = "curl", quiet = T)
      }
    }
    
    #Sys.sleep(3)
  }
}

# library(ncdf4)
# to180 <- function(x) {bx <- (x > 180); x[bx] <- x[bx] - 360; x}
# stations <- read.csv("data/bf_stations.csv",header = T, sep = ",", dec = ".")
# rownames(stations) <- stations$Name

# 
# nc <- nc_open("data/cmorph/CMORPH_V1.0_ADJ_0.25deg-HLY_2001010100.nc")
# mvar <- "cmorph"
# var <- ncvar_get(nc,"cmorph")
# 
# lon <- nc$dim$lon$vals
# lat <- nc$dim$lat$vals
# lon <- to180(lon)
# nc_close(nc)
# 
# df <- data.frame(matrix(nrow=0, ncol=nrow(stations)))
# colnames(df) <- stations$Name
# 
# vals <- c()
# for (s in stations$Name) {
#   #s <- "OUAGADOUGOU"
#   mlon <- stations[s,"Longitude"]
#   mlat <- stations[s,"Latitude"]
#   
#   lon_idx <- which.min(abs(lon-mlon))
#   lat_idx <- which.min(abs(lat-mlat))
#   
#   vals <- c(vals, var[lon_idx, lat_idx])
# }
# df[nrow(df)+1,] <- vals
