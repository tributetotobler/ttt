# create a vintage frame !
#' @param x An \code{\link{sf}} object 
#' @return 
#' @examples
#' data(dollars)
#' vintageframe(dollars$polygones)
#' @export 
vintageframe = function(x){
  bb = sf::st_bbox(x)
  d  = (bb[3]-bb[1])/50
  bb = bb+c(-d,-d,d,d)
  frame=sf::st_as_sfc(bb,crs=st_crs(x))
  f=sf::st_segmentize(frame,d)
  f = sf::st_cast(f,"POINT")
  plot(f,pch=3)
}


# create a +- map !
#' @param xsf An \code{\link{sf}} object with poylgones
#' @param varname The name 
#' @value 
#' @examples
#' data(dollars)
#' vintageframe(dollars$polygones)
#' @export 
plusmoins =function(xsf,varname){
  pts=sf::st_centroid(st_geometry(xsf))
  vals = xsf[[varname]]

  
  rpos = sqrt(vals[vals>0])
  rpos = rpos/max(rpos)*10
  
  rneg = sqrt(-vals[vals<0])
  rneg= rneg/max(rneg)*10

  plot(pts[vals>0],pch="+:",cex=rpos,add=TRUE)
  plot(pts[vals<0],pch="-:",cex=rneg,add=TRUE)
}

