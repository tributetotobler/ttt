dir.create("data/tmp")


url = "https://www.insee.fr/fr/statistiques/fichier/4171543/RP2016_migcom_csv.zip"

file <- download.file(url, "data/tmp/migfr.zip")
unzip("data/tmp/migfr.zip", exdir = "data/tmp")

library(dplyr)
library(readr)
library(CARTElette)

migcom=read_delim("./data/tmp/FD_MIGCOM_2016.csv",delim = ";")


# arrondissement 
ARM=rbind(tibble(ARM=migcom %>% filter(COMMUNE==75056) %>% pull(ARM) %>% unique(),COM="75056"),
          tibble(ARM=migcom %>% filter(COMMUNE==13055) %>% pull(ARM) %>% unique(),COM="13055"),
          tibble(ARM=migcom %>% filter(COMMUNE==69123) %>% pull(ARM) %>% unique(),COM="69123"))



migcomnoarr = migcom %>% left_join(ARM,by=c("DCRAN"="ARM")) %>% mutate(PCOMMUNE=if_else(is.na(COM),DCRAN,COM))


migin = migcomnoarr %>% group_by(COMMUNE,CS1,SEXE) %>% summarise(nin=sum(IPONDI))
migout = migcomnoarr %>% group_by(PCOMMUNE,CS1,SEXE) %>% summarise(nout=sum(IPONDI)) 

migcomdelta = migin %>% full_join(migout,by=c("COMMUNE"="PCOMMUNE","CS1"="CS1","SEXE"="SEXE")) %>% ungroup()%>% mutate(nin = if_else(is.na(nin),0,nin),nout = if_else(is.na(nout),0,nout),delta=nin-nout)

migcomdelta %>% arrange(desc(abs(delta)))
saveRDS(migcomdelta,"data-raw/migr_fr_delta2016.rds")

migcomdelta=readRDS("data-raw/migr_fr_delta.rds")
migcomdeltaglob = migcomdelta %>% group_by(COMMUNE) %>% summarise(delta=sum(delta))
saveRDS(migcomdelta,"data-raw/migr_fr_delta_glob2016.rds")

migcs   = migcomdelta %>% group_by(COMMUNE,CS1) %>% summarise(delta=sum(delta)) %>% pivot_wider(names_from = CS1,values_from = delta, names_prefix = "CS",values_fill = list(delta=0))
migsexe = migcomdelta %>% group_by(COMMUNE,SEXE) %>% summarise(delta=sum(delta)) %>% pivot_wider(names_from = SEXE,values_from = delta, names_prefix = "SEXE",values_fill = list(delta=0))
mig_details = migcs %>% left_join(migsexe)

library(CARTElette)
CO_sf <- charger_carte(COG=2018,nivsupra="COM")
reg_out = c("01","02","03","04","06","94")
CO_sf.delta = CO_sf %>% left_join(migcomdeltaglob,by=c("INSEE_COM"="COMMUNE")) %>% mutate(delta=if_else(is.na(delta),0,delta)) %>% filter(!INSEE_REG %in% reg_out)
saveRDS(CO_sf.delta,"data-raw/migr_fr_delta_glob_geom2016.rds")

CO_sf.delta.details =  CO_sf %>% left_join(mig_details,by=c("INSEE_COM"="COMMUNE")) %>% filter(!INSEE_REG %in% reg_out)
saveRDS(CO_sf.delta.details,"data-raw/migr_fr_delta_details_geom2016.rds")


library(ttt)
library(sf)
library(dplyr)
data=readRDS("./data-raw/migr_fr_delta_details_geom.rds")

data = data %>% mutate(TOTAL = SEXE1+SEXE2) 
var_names = names(data)[c(12:21,23)]
data = data %>% mutate_at(var_names,function(v){if_else(is.na(v),0,v)})

pot = list()
for ( v in var_names){
  pot[[v]]=poisson.potential(data,varname = v,cellsize=2500,nb_it=300*100)
}
saveRDS(pot,"./data-raw/migr_fr_delta_details_potentials.rds")
pot = readRDS("./data-raw/migr_fr_delta_details_potentials.rds")

params = list(resolution=2500, lifespan=30,scalefact = 12,linewidth = 1, nbparticules = 3000)
res=list()
for (v in var_names){
  res[[v]] = pot[[v]] %>% st_drop_geometry() %>% select(dx,dy)
}

  

xy = pot[[1]] %>% st_drop_geometry() %>% select(x,y)


data_export = list(data=res,params=params,xy=xy)

jsonlite::write_json(data_export,"./data-raw/pot_migr_fr2016jac.json",dataframe="columns")

fr_contour = fr %>% st_geometry() %>% st_union() %>% st_simplify(dTolerance = 250)


