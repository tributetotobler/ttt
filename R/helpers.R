# Links
getlinks <- function(x, df, xid, dfid, dfvar){
   dots <- sf::st_centroid(x = sf::st_geometry(x),of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))
  x2 <- data.frame(id = x[[xid]],
                   sf::st_coordinates(sf::st_centroid(x = sf::st_geometry(x),
                                                      of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))))
  df <- df[, c(dfid,dfvar)]
  colnames(df) <- c("i","j","fij")
  df <- df[!df$i == df$j,]

  link <- merge(df, x2, by.x = dfid[2], by.y = "id", all.x = TRUE)
  link <- merge(link, x2, by.x = dfid[1], by.y = "id", all.x = TRUE)
  names(link)[4:7] <- c("xj", "yj", "xi", "yi")
  link <- link[link$i != link$j,]
  link$ang <- atan2(link$yj - link$yi, link$xj - link$xi) * 180 / pi
  link$dist <- sqrt ((link$xj - link$xi) ^ 2 + (link$yj - link$yi) ^ 2)
  stringo <- paste0("LINESTRING(", link$xi, " ", link$yi, ", ",link$xj, " ", link$yj, ")")
  link <- sf::st_sf(link, geometry = sf::st_as_sfc(stringo, crs = sf::st_crs(x)))
  link <- link[,c(1:3,8:10)]
  ctr <- st_coordinates(st_centroid(st_geometry(link)))
  link$ctrx <- ctr[,1]
  link$ctry <- ctr[,2]
  link <- link[,c("i","j","fij","ang","dist","ctrx","ctry","geometry")]
  return(link)
}

# Rotate

rotate <- function(x){
  ctr = st_as_sf(x[,c("ctrx","ctry")] %>% st_drop_geometry(), coords = c("ctrx", "ctry"), crs = st_crs(x))
  rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  n <- nrow(x)
  for (i in 1:n){
    st_geometry(x[i,]) <- ( st_geometry(x)[i] - st_geometry(ctr)[i]) * rot(x$ang[i] * pi/180) + st_geometry(ctr)[i]
  }
  return (x)
}
# Circles

getcircles <- function(x, xid, df, dfid, dfvar, k){

    if (is.null(k)){
    m <- max(df[,dfvar])
    bb <- st_bbox(x)
    k <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)/ (m * 700)
  }

  dots <- sf::st_centroid(x = sf::st_geometry(x),of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))
  dots <- st_sf(x[,xid] %>% st_drop_geometry(), dots)
  dots <- merge(dots, df, by.x = xid, by.y = dfid)
  dots[,"r"] <- dots[,dfvar] %>% st_drop_geometry() * k
  circles <- st_buffer(dots, dots$nb * k)
  circles <- circles[,c(xid,dfvar,"r","geometry")]
  return(circles)
}

# Dots

getdots <- function(x, xid, k = NULL){
  if (is.null(k)){
    bb <- st_bbox(x)
    k <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)/300
  }

  dots <- sf::st_centroid(x = sf::st_geometry(x),of_largest_polygon = max(sf::st_is(sf::st_as_sf(x), "MULTIPOLYGON")))
  dots <- st_sf(x[,xid] %>% st_drop_geometry(), dots)
  dots[,"r"] <- k
  circles <- st_buffer(dots, k)
  return(circles)
}

# Link to Flows

linktoflows <- function(link, size, k, dfvar, type, decreasing){

  crs <- st_crs(link)
  link$shift <- 0
  link$ij <- paste0(link$i,"_",link$j)
  link$ji <- paste0(link$j,"_",link$i)
  nb <- nrow(link)
  for(i in 1:nb) {
    if(link$ij[i] %in% link$ji) {link$shift[i] <- 1}
  }

  bb <- st_bbox(link)
  hmax <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin)/25
  delta <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin) / 300
  delta2 <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin) / 750

  if(!"delta_i" %in% names(link)){
    link$delta_i <- 0
    link$delta_j <- 0
    delta <- 0
  }


  if (size == "area"){
    if(is.null(k)){ k <- hmax / max(link[[dfvar]] / link$dist)}
    link$height <- link[[dfvar]] / link$dist * k
    link$area <- link$height * link$dist

  } else {
    if(is.null(k)){ k <- hmax / max(link[[dfvar]])}
    link$height <- link[[dfvar]] * k
  }

  link <- rotate(link)
  l <- st_geometry(link)
  d <- st_coordinates(l)
  n <- nrow(d)
  r <- data.frame(d[seq(1, n, 2),] ,d[seq(2, n, 2),] )
  rownames(r) <- r$L1
  r <- r[,c("X","Y","X.1","Y.1")]
  colnames(r) <- c("x1","y1","x2","y2")
  r$height <- link$height

  if (type == "arrows"){
  r$x1 <- r$x1 + link$delta_j + delta
  r$x2 <- r$x2 - link$delta_i - delta
  r$y1 <- r$y1 - (r[["height"]]/2 + delta2) * link$shift
  r$y2 <- r$y2 - (r[["height"]]/2 + delta2) * link$shift
  r$p1x <- r$x1
  r$p1y <- r$y1 - r[["height"]]/2
  r$p2x <- r$x2 - r[["height"]]/2
  r$p2y <- r$y2 - r[["height"]]/2
  r$p3x <- r$x2
  r$p3y <- r$y2
  r$p4x <- r$p2x
  r$p4y <- r$y2 + r[["height"]]/2
  r$p5x <- r$x1
  r$p5y <- r$y1 + r[["height"]]/2


  st_geometry(link) <- st_as_sfc(paste0("POLYGON((",r$p1x," ",r$p1y,", ",r$p2x," ",r$p2y,", ",r$p3x," ",r$p3y,", ",r$p4x," ",r$p4y,", ",r$p5x," ",r$p5y,", ",r$p1x," ",r$p1y,"))"))
  }


  if (type == "rect"){
    r$p1x <- r$x1 + link$delta_j + delta
    r$p2x <- r$x2 - link$delta_i - delta
    r$p1y <- r$y1 - (r[["height"]]/2 + delta2) * link$shift
    r$p2y <- r$y2 - (r[["height"]]/2 + delta2) * link$shift
    r$p3x <- r$p2x
    r$p3y <- r$p2y + r[["height"]]/2
    r$p4x <- r$p1x
    r$p4y <- r$p1y + r[["height"]]/2
    r$p1y <- r$p1y - r[["height"]]/2
    r$p2y <- r$p2y - r[["height"]]/2
    st_geometry(link) <- st_as_sfc(paste0("POLYGON((",r$p1x," ",r$p1y,", ",r$p2x," ",r$p2y,", ",r$p3x," ",r$p3y,", ",r$p4x," ",r$p4y,", ",r$p1x," ",r$p1y,"))"))
  }

  link$ang <- - link$ang
  link <- rotate(link)
  link <- link[order(link$height, decreasing = decreasing),]
  st_crs(link) <- crs
  return(link)
}
