% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttt_flowmapper.R
\name{ttt_flowmapper}
\alias{ttt_flowmapper}
\title{Flowmapper}
\usage{
ttt_flowmapper(
  x,
  xid = "id",
  df,
  dfid = c("i", "j"),
  dfvar = "fij",
  size = "thickness",
  type = "arrows",
  decreasing = FALSE,
  add = FALSE,
  lwd = 1,
  col = "#FF000099",
  border = "#4a0c25",
  k = NULL,
  df2 = NULL,
  df2id = "id",
  df2var,
  k2 = NULL,
  col2 = "white",
  border2 = "black",
  lwd2 = 2,
  plot = TRUE
)
}
\arguments{
\item{x}{an sf object (polygon or multipolygons).}

\item{xid}{name of the identifier variable in x, default is "id".}

\item{df}{name of the numeric field in df to plot flows.}

\item{dfid}{name of the two identifier variables in df, default is c("i","j").}

\item{dfvar}{name of the numeric variable to plot flows, default is "fij".}

\item{size}{way of calculating the size of the flows, "thickness" ou "area, default is "thickness".}

\item{type}{type of flows, "rect" (for rectangles) or "arrows" (for arrows), default is "arrows".}

\item{decreasing}{display order, default is FALSE.}

\item{add}{whether to add the layer to an existing plot (TRUE) or not (FALSE), default is TRUE}

\item{lwd}{flows borders width, default is 1.}

\item{col}{color of flows, default is "#00FF0090"}

\item{border}{color of flows borders, default is "#4a0c25"}

\item{k}{value to increase or decrease the size of the flows (in units of the map), default is NULL. To be improved...}

\item{df2}{dataframe containg data to plot proportional circles between flows, default is NULL.}

\item{df2id}{name of the numeric field in df2, default is "id"}

\item{df2var}{name of the numeric variable to plot circles.}

\item{k2}{value to increase or decrease the size of the circles (in units of the map), default is NULL. To be improved...}

\item{col2}{color of circles, default is "white"}

\item{border2}{color of circles borders, default is "black".}

\item{lwd2}{circle borders width, default is 2.}

\item{plot}{display or not the map, default = TRUE}
}
\value{

}
\description{
Build flowmaps
}
\examples{
library(sf)

# Import
crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

subregions <- st_read(system.file("subregions.gpkg", package="flowmapper")) \%>\% st_transform(crs)
migr <- read.csv(system.file("migrantstocks2019.csv", package="flowmapper"))

#  Selection, filtrage, traitement en amont

threshold <- 1500
migr <- migr[migr$fij >= threshold,]

# Example 1

c <- ttt_flowmapper(
 x = subregions,
 xid = "id",
 df = migr,
 dfid = c("i","j"),
 dfvar = "fij"
)

# Example 2

intra <- migr[migr$i == migr$j,c("i","fij")]
colnames(intra) <- c("id","nb")

plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)
c <- ttt_flowmapper(
 x = subregions,
 xid = "id",
 df = migr,
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

# Example 3

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

# Example 4

crs <- "+proj=ortho +lat_0=42.5333333333 +lon_0=-72.53333333339999 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
flows <- smoothr::densify(c[[3]], n = 30) \%>\% st_transform(crs)
plot(st_geometry(subregions) \%>\% st_transform(crs), col ="#CCCCCC", border = "white")
plot(st_geometry(flows), col ="#FF000099", add = TRUE)
plot(st_centroid(st_geometry(c[[2]])) \%>\% st_transform(crs), add = TRUE, pch = 20, cex = 1.3, col ="black")
}
