states_names = read.table("./data-raw/migrations/State Names.txt",header = FALSE,sep = ",")
states_names=states_names$V1
Migr7580 <- read.table("./data-raw/migrations/7580Migr.TBL",header = FALSE)
colnames(Migr7580)=states_names
row.names(Migr7580)=states_names
Migr7580 = as.matrix(Migr7580)
Migr6570 <- read.table("./data-raw/migrations/6570Migr.TBL",header = FALSE)
colnames(Migr6570)=states_names
row.names(Migr6570)=states_names


us = readRDS("./data/geometries/us.rds")
library(dplyr)
library(sf)

setdiff(us$name,states_names)
#wdc = us%>% filter(id=="DC")
usmigr = us%>% filter(id!="DC") %>% st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
balance = data.frame(delta7580=colSums(Migr7580)-rowSums(Migr7580),delta6570=colSums(Migr6570)-rowSums(Migr6570),name=states_names,stringsAsFactors = FALSE)
usmigrations = usmigr %>% left_join(balance)
usethis::use_data(usmigrations,overwrite = TRUE)
