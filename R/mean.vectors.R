# vectors 
#' \code{poisson.potential} returns an sf
#' @examples
#' # data(munich)
#' # sf=mean.vectors(munich$coords,minich$OD)
#' # ggplot(sf)+geom_segment(aes(x=x,y=y,xend=x+dx*100,yend=y+dy*100),arrow = arrow(angle=15,length = unit(0.3,"cm"),type = "closed"))+theme_void()
#' @export

mean.vectors = function(pts,OD){
  dists = sf::st_distance(pts,pts)
  n = nrow(pts)
  ODds = (OD-t(OD))/(OD+t(OD))/dists
  diag(ODds) = 0
  weights = 
  Xd = matrix(rep(st_coordinates(pts)[,1],each=n),n,n)
  Yd = matrix(rep(st_coordinates(pts)[,2],each=n),n,n)
  Xo = matrix(rep(st_coordinates(pts)[,1]),n,n)
  Yo = matrix(rep(st_coordinates(pts)[,2]),n,n)
  dx = 1/(n-1)*rowSums(ODds*(Xd-Xo))
  dy = 1/(n-1)*rowSums(ODds*(Yd-Yo))
  pts$dx=dx
  pts$dy=dy
  pts$x = st_coordinates(pts)[,1]
  pts$y = st_coordinates(pts)[,2]
  pts
}