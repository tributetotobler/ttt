% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ttt_mean_od_vectors.R
\name{mean_od_vectors}
\alias{mean_od_vectors}
\title{Compute the mean vectors of an OD matrix}
\usage{
mean_od_vectors(pts, OD)
}
\arguments{
\item{pts}{\code{sf} data.frame with point geometries}

\item{OD}{an OD matrix with as many rows as \code{pts}}
}
\value{
An \code{sf} data.frame similar to \code{pts} with \code{dx} and \code{dy} columns that store the mean vectors.
}
\description{
Compute the mean vectors of an OD matrix
}
\examples{
# data(munich)
# sf=mean_od_vectors(munich$coords,minich$OD)
# ggplot(sf)+geom_segment(aes(x=x,y=y,xend=x+dx*100,yend=y+dy*100),arrow = arrow(angle=15,length = unit(0.3,"cm"),type = "closed"))+theme_void()
}
