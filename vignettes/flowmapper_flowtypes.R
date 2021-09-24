# FLOWMAPPER FROM FLOW TYPES
# TTT - 16/07/2020, FB
#---------------------------

rm(list=ls())

install.packages("devtools")

library("devtools")
remotes::install_git(url = "https://gitlab.huma-num.fr/nlambert/flowmapper")

library(dplyr)
library(sf)
library(rlang)
library(cartograflow)


#--------------------------
# DATA
#--------------------------

crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

subregions <- st_read(system.file("subregions.gpkg", package="flowmapper")) %>% st_transform(crs)

migr <- read.csv(system.file("migrantstocks2019.csv", package="flowmapper"))


#--------------------------
# From Tabflow to Matflow (also for closing and square a matrix)
#--------------------------

#From list to matrix
mat_migr<-cartograflow::flowtabmat(migr, matlist = "M")

#From matrix to list  
tab_migr<-cartograflow::flowtabmat(mat_migr, matlist = "L")
colnames(tab_migr)<-c("i", "j", "fij")

summary(tab_migr)  # closed and square matrix

summary(migr)     # non closed and non square matrix : "rectangular" format

str(migr)

# Variable typing 
migr$i<-as.character(migr$i)
migr$j<-as.character(migr$j)
migr$fij<-as.numeric(migr$fij)

#----------------------------
# I - Compute Gross and Net flows
#----------------------------

# O - Square the matrix first (if necessary for "rectangular formats")
#----------------------------

liste<-subregions %>% select(id)

liste<-as.data.frame(liste$id)

# matrix version (if necessary)
# mat_migr_2<-flowcarre(tab=mat_migr,
#                       liste=liste,
#                       origin = "i", dest="j",valflow="fij",
#                       format="M",
#                       diagonale = TRUE,
#                       empty.sq = FALSE
# )

# List version      
migr_carre<-cartograflow::flowcarre(tab=migr,
                                    liste=liste,
                                    origin = "i", dest="j",valflow="fij",
                                    format="L",
                                    diagonale = TRUE,
                                    empty.sq = FALSE
)

colnames(migr_carre)<-c("i", "j", "fij")

# Variable typing 
migr_carre$i<-as.character(migr_carre$i)
migr_carre$j<-as.character(migr_carre$j)
migr_carre$fij<-as.numeric(migr_carre$fij)


# I.1. Compute bilateral gross flows
#----------------------------

#>>> launch ttt_flowmapper_types.R


## All symetric / gross matrix
#----------------------------

migr_gross<-ttt_flowmapper_types(migr, 
                            origin="i",destination="j",fij="fij",
                            format="L",
                            type="gross")

test1<-cartograflow::flowtabmat(migr_gross,matlist = "M")

## Part of the symetric / gross matrix (with triangular low up reduction)
#----------------------------

migr_gross_up<-ttt_flowmapper_types(migr, 
                                 origin="i",destination="j",fij="fij",
                                 format="L",
                                 type="gross",
                                 lowup="up")

#test11<-cartograflow::flowtabmat(migr_gross_up,matlist = "M")

migr_gross<-migr_gross_up
colnames(migr_gross) <- c("i","j", "fij")
migr_gross$i<-as.character(migr_gross$i)
migr_gross$j<-as.character(migr_gross$j)
migr_gross$fij<-as.numeric(migr_gross$fij)


# I.1. Compute bilateral net flows
#----------------------------

#>>> launch ttt_flowmapper_types.R

## All antisymetric / net matrix
#----------------------------

migr_net<-ttt_flowmapper_types(migr, 
                                 origin="i",destination="j",fij="fij",
                                 format="L",
                                 type="net")

#test2<-cartograflow::flowtabmat(migr_net,matlist = "M")


## Part of the antisymetric / net matrix (keeping only positive net flows)
#----------------------------

migr_net_pos<-ttt_flowmapper_types(migr, 
                               origin="i",destination="j",fij="fij",
                               format="L",
                               type="net",
                               net="positive")

#test22<-cartograflow::flowtabmat(migr_net_pos,matlist = "M")


#----------------------------
# II - Compute intrazonal and place oriented flows
#----------------------------

# intrazonal interactions
#-----------------------------
intra <- migr_gross[migr_gross$i == migr_gross$j,c("i","fij")]
colnames(intra) <- c("id","nb")
str(intra)

# flow volume by places
#-----------------------------

#>>> launch flowplaces.R

voli <- cartograflow::flowplaces(migr_gross, origin ="i",destination="j",fij="fij",format = "L", x = "voli")
colnames(voli) <- c("id","vol")
voli<-as.data.frame(voli)

str(voli)


# net flow  (or balance) by places
#-----------------------------

#>>> launch flowplaces.R

bali <- cartograflow::flowplaces(migr_gross, origin ="i",destination="j",fij="fij",format = "L", x = "bali")
colnames(bali) <- c("id","bal")
bali<-as.data.frame(bali)

str(bali)


#----------------------------
# III - thresholding flows
#----------------------------

# Example of gross flow for plotting with rectangles

summary(migr_gross$fij)

threshold <- 703  # mean value as Tobler's suggest


# flow reduction
migr_gross_map <- migr_gross[migr_gross$fij >= threshold,]


#----------------------------
# IV- FLOWMAPPER
#----------------------------


# >>> launch ttt_flowmapper.R
# >>> launch helpers.R


# Flowmapping general test
# ----------------------------

c <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr_gross_map,
  dfid = c("i","j"),
  dfvar = "fij"
)



# Plot C - All bilateral flows
#--------------------------
summary(migr) #for plotting with rectangles

threshold <- 625  # mean value as Tobler's suggest

migr_mean <- migr[migr$fij >= threshold,]

dev.off()

plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)

c <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr,
  dfid = c("i","j"),
  dfvar = "fij",
  size = "thickness",
  type = "arrows",
  decreasing = FALSE,
  add = TRUE,
  lwd = 1,
  col = "#00FF0090",
  border = "#4a0c25",
  k = NULL
)


# Plot C2 : Gross flows - thickness
#-------------------------------------


colnames(migr_gross_map)<-c("i", "j", "fij")

dev.off()

plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)

c2 <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr_gross_map,
  dfid = c("i","j"),
  dfvar = "fij",
  size = "thickness",
  type = "rect",
  decreasing = FALSE,
  add = TRUE,
  lwd = 1,
  col = "#00FF0090",
  border = "#4a0c25",
  k = NULL,
  df2 = intra,
  df2id = "id",
  df2var = "nb"
)


# Plot C3 : Gross flows - area
#-----------------------------------

colnames(migr_gross_map)<-c("i", "j", "fij")

dev.off()

plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)

c3 <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr_gross_map,
  dfid = c("i","j"),
  dfvar = "fij",
  size = "area",
  type = "rect",
  decreasing = FALSE,
  add = TRUE,
  lwd = 1,
  col = "#00FF0090",
  border = "#4a0c25",
  k = NULL,
  df2 = voli,
  df2id = "id",
  df2var = "vol"
)


# Plot C4 - Net (positive) flows - thickness
#--------------------------

colnames(migr_net)<-c("i", "j", "fij")

mean<-mean(migr_net$fij)

migr_net_mean <- migr_net[migr_net$fij >= mean,]

dev.off()

plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)

c4 <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr_net_mean,
  dfid = c("i","j"),
  dfvar = "fij",
  size = "thickness",
  type = "arrows",
  decreasing = FALSE,
  add = TRUE,
  lwd = 1,
  col = "#00FF0090",
  border = "#4a0c25",
  k = NULL
)

dev.off()

plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)

c5 <- ttt_flowmapper(
  x = subregions,
  xid = "id",
  df = migr_net_mean,
  dfid = c("i","j"),
  dfvar = "fij",
  size = "area",
  type = "arrows",
  decreasing = FALSE,
  add = TRUE,
  lwd = 1,
  col = "#00FF0090",
  border = "#4a0c25",
  k = NULL
)


## -----------------------------------------------------------------------------
crs <- "+proj=aeqd +lat_0=90 +lon_0=50 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "
subregions2 <- st_transform(subregions, crs)

plot(st_geometry(subregions2), col = "#CCCCCC", border = "white", lwd = 0.5)

c <- ttt_flowmapper(
  x = subregions2,
  xid = "id",
  df = migr,
  dfid = c("i","j"),
  dfvar = "fij",
  add = TRUE
)

plot(st_geometry(subregions2), col = "#CCCCCC", border = "white", lwd = 0.5)

c5 <- ttt_flowmapper(
  x = subregions2,
  xid = "id",
  df = migr_net_mean,
  dfid = c("i","j"),
  dfvar = "fij",
  add = TRUE
)


## -----------------------------------------------------------------------------
dev.off()

crs <- "+proj=ortho +lat_0=42.5333333333 +lon_0=-72.53333333339999 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

flows <- smoothr::densify(c[[3]], n = 30) %>% st_transform(crs)
plot(st_geometry(subregions) %>% st_transform(crs), col ="#CCCCCC", border = "white")
plot(st_geometry(flows), col ="#00FF0090", add = TRUE)
plot(st_centroid(st_geometry(c[[2]])) %>% st_transform(crs), add = TRUE, pch = 20, cex = 1.3, col ="black")

