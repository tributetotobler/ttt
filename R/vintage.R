# create a vintage frame !
#' @export 
vintageframe = function(x){
  bb = st_bbox(x)
  d  = (bb[3]-bb[1])/50
  bb = bb+c(-d,-d,d,d)
  frame=st_as_sfc(bb,crs=st_crs(x))
  f=st_segmentize(frame,d)
  f = st_cast(f,"POINT")
  plot(f,pch=3)
}


# create a +- map !
#' @export 
plusmoins =function(xsf,varname){
  pts=st_centroid(st_geometry(xsf))
  vals = xsf[[varname]]

  
  rpos = sqrt(vals[vals>0])
  rpos = rpos/max(rpos)*10
  
  rneg = sqrt(-vals[vals<0])
  rneg= rneg/max(rneg)*10

  plot(pts[vals>0],pch="+:",cex=rpos,add=TRUE)
  plot(pts[vals<0],pch="-:",cex=rneg,add=TRUE)
}

