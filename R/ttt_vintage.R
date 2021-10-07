#' Create a vintage frame !
#'
#' @param x An \code{\link{sf}} object 
#' @return a plot
#' @examples
#' data(dollars)
#' vintage_frame(dollars$polygones)
#' @export 
vintage_frame = function(x){
  bb = sf::st_bbox(x)
  d  = (bb[3]-bb[1])/50
  bb = bb+c(-d,-d,d,d)
  frame=sf::st_as_sfc(bb,crs=st_crs(x))
  f=sf::st_segmentize(frame,d)
  f = sf::st_cast(f,"POINT")
  plot(f,pch=3)
}


#' Create a +- map !
#' 
#' @param xsf An \code{\link{sf}} object with poylgones or points
#' @param varname The name of the column that store the balances to plot
#' @return a plot 
#' @examples
#' data(dollars)
#' vintage_frame(dollars$polygones)
#' dollars$polygones$delta = rowSums(dollars$OD)-colSums(dollars$OD)
#' plus_minus_map(dollars$polygones,"delta")
#' @export 
plus_minus_map =function(xsf,varname){
  pts=sf::st_centroid(st_geometry(xsf))
  vals = xsf[[varname]]
  
  rpos = sqrt(vals[vals>0])
  rpos = rpos/max(rpos)*10
  
  rneg = sqrt(-vals[vals<0])
  rneg= rneg/max(rneg)*10

  plot(pts[vals>0],pch="+:",cex=rpos,add=TRUE)
  plot(pts[vals<0],pch="-:",cex=rneg,add=TRUE)
}

