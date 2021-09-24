library(sf)
library(rmapshaper)

# Data Import
dir.create("data/tmp")
url <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip"
file <- download.file(url, "data/tmp/us_states.zip")
unzip("data/tmp/us_states.zip", exdir = "data/tmp")
us <- st_read("data/tmp/cb_2018_us_state_20m.shp")
us <- us[,c("STUSPS","NAME","geometry")]
colnames(us) <- c("id","name","geometry")
us <- us[!us$id %in% c("AK", "HI", "PR"),]
unlink("data/tmp/", recursive = TRUE)

# Generalization
us <- ms_simplify(us, method = "vis", keep = 0.08, snap = TRUE, keep_shapes = TRUE)
saveRDS(us, file = "data/geometries/us.rds")
