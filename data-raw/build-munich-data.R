library(sf)
zones_names = read.table("./data-raw/Munich/Munich.plc",header = FALSE,sep = ";")
zones_names=zones_names$V1
Migr <- read.table("./data-raw/Munich/MUNICH.PMX",header = FALSE)
colnames(Migr)=zones_names
row.names(Migr)=zones_names
Migr = as.matrix(Migr)
pts <- read.table("./data-raw/Munich/Munich.pts",header = FALSE) 
coords = st_as_sf(pts,coords = c("V1","V2"))
coords$name = zones_names
munich = list(OD=Migr,coords=coords)
usethis::use_data(munich,overwrite = TRUE)
