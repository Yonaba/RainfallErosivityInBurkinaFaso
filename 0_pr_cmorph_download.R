setwd("D:/Recherche/Article_Erosion_2023/analysis/")
Sys.setenv(TZ = "UTC")

library(lubridate)

base_url <- "https://www.ncei.noaa.gov/data/cmorph-high-resolution-global-precipitation-estimates/access/hourly/0.25deg/"
base_nc <- "CMORPH_V1.0_ADJ_0.25deg-HLY_"

for (yr in 2000:2020) {
  #print(paste0("Year: ", yr))
  tseq <- seq(as.Date(paste0(yr, "-01-01")), as.Date(paste0(yr, "-12-31")), by = "day")
  
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
    
  }
}

