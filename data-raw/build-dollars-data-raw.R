library(sf)

zones_names = readLines("./data-raw/Dollar Movement/FRB Names.plc",n=12)
zones_names = gsub("\t","", zones_names)

zones_center = read.table("./data-raw/Dollar Movement/FRB_Bnds/FRBcenters.pts",header = FALSE,sep = " ")
coords = st_as_sf(zones_center[,c(2,4)],coords = c("V2","V4"))
coords$name = zones_names
library(purrr)
lines = readLines("./data-raw/Dollar Movement/FRB_Bnds/FRBdys.Ply")
isplit = c(which(lines == -9),length(lines))
poly = list()
for (i in 2:length(isplit)){
  pol_coords=lines[(isplit[i-1]+1):(isplit[i]-1)] %>% as.list() %>% 
    map(function(coords){substr(coords,2,nchar(coords)-1)}) %>% 
    map(function(coords){strsplit(coords,"  ")[[1]]}) %>%
    map(as.numeric) 
  poly[[i-1]]=st_polygon(list(do.call(rbind,pol_coords)))
}
zones = st_sf(geom=st_as_sfc(poly))
zones$name = zones_names

  
Migr <- readLines("./data-raw/Dollar Movement/Dollar.pmx",n = 12)
Migr = gsub("[\t, ]+"," ",Migr) 
Migr = read.table(text=Migr,sep=" ",header = FALSE)
Migr = Migr[,-1]

colnames(Migr)=zones_names
row.names(Migr)=zones_names
Migr = as.matrix(Migr)

dollars=list(OD=Migr,coords=zones_center,polygones=zones)
usethis::use_data(dollars,overwrite = TRUE)


