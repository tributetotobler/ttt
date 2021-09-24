#' @title Flowmapper Legend
#' @description Build flowmaps legend
#' @name ttt_flowmapperlegend 
#' @param x flow generetd with the function plotflows(). list.
#' @param title title of the legend (flows), default is "Title".
#' @param title2 title of the legend (circles), default is NULL.
#' @export
#' @examples
#' library(sf)
#' crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#' subregions <- st_read(system.file("subregions.gpkg", package="flowmapper")) %>% st_transform(crs)
#' migr <- read.csv(system.file("migrantstocks2019.csv", package="flowmapper"))
#' threshold <- 1500
#' migr <- migr[migr$fij >= threshold,]
#' intra <- migr[migr$i == migr$j,c("i","fij")]
#' colnames(intra) <- c("id","nb")
#'
#' plot(st_geometry(subregions), col = "#CCCCCC", border = "white", lwd = 0.5)
#' flows <- ttt_flowmapper(
#'        x = subregions,
#'        xid = "id",
#'        df = migr,
#'         dfid = c("i","j"),
#'         dfvar = "fij",
#'        size = "area",
#'         type = "arrows",
#'         decreasing = FALSE,
#'         add = TRUE,
#'        lwd = 1,
#'        col = "#00FF0090",
#'         border = "#4a0c25",
#'        df2 = intra,
#'       df2id = "id",
#'      df2var = "nb"
#' )
#'
#' ttt_flowmapperlegend(x = flows, title = "inter", title2 = "intra")

ttt_flowmapperlegend <- function(x, pos = NULL, title = "Title 1", title2 = NULL) {
# Vars

col = "white"
border = "black"
lwd = 1
values.cex = 0.6
values.round = 0
lty = 3
nb.circles = 4
title.cex = 0.8
title.font = 2


# 1 - circles

        # Radii & Values
        v <- x[[2]]
        st_geometry(v) <- NULL
        v <- v[,2]
        r <- sqrt(as.numeric(st_area(x[[2]]))/pi)
        radii <- seq(from = max(r), to = min(r), length.out = nb.circles)
        sle <- radii * radii * pi
        values <- sle * max(v) / sle[1]

        # Positions

        delta <- (par()$usr[2] - par()$usr[1]) / 50
        if(length(pos) != 2){
                pos <- c(par()$usr[1] + radii[1] + delta,par()$usr[3] + delta)
        }

        # Circles

        if (!is.null(title2)){

        for(i in 1:nb.circles){
                # circles
                posx <- pos[1]
                posy <- pos[2] + radii[i]
                p <- st_sfc(st_point(c(posx,posy)))
                circle <- st_buffer(st_as_sf(p), dist = radii[i])
                plot(circle, col = col, border = border, lwd=lwd, add=T)
                # lines
                segments(posx, posy + radii[i], posx + radii[1] + radii[1]/10, col = border, lwd=lwd, lty = lty)
                # texts
                text(x = posx + radii[1] + radii[1]/5, y = posy + radii[i],
                     labels = formatC(round(values[i],values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)
        }

        # Title
        text(x = posx - radii[1] ,y = posy + radii[1]*2 + radii[1]/3, title2,
             adj = c(0,0), cex = title.cex, font = title.font)
}

# flows (thickness)

        if(is.null(x[[3]]$area)){


        hmax <- max(x[[3]]$height)
        hmin <- min(x[[3]]$height)
        bb <- st_bbox(x[[3]])
        width <- (bb$xmax - bb$xmin) / 15
        deltax <- (bb$xmax - bb$xmin) / 10
        rectmax <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",width," ",0,", ",width," ",hmax,", ",0," ",hmax,", ",0," ",0,"))")) + pos + c(max(radii) + deltax,0)
        rectmin <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",width," ",0,", ",width," ",hmin,", ",0," ",hmin,", ",0," ",0,"))")) + pos + c(max(radii) + deltax,0)
        segments(pos[1] + max(radii) + width + deltax, pos[2] + hmin, pos[1] + max(radii) + width + deltax + width/4, pos[2] + hmin, col = border, lwd=lwd, lty = lty)
        segments(pos[1] + max(radii) + width + deltax, pos[2] + hmax, pos[1] + max(radii) + width + deltax + width/4, pos[2] + hmax, col = border, lwd=lwd, lty = lty)
        plot(rectmax, add= TRUE)
        plot(rectmin, add= TRUE)
        vals <- flows[[2]][,2] %>% st_drop_geometry()
        text(x = pos[1] + max(radii) + width + deltax + width/4 + width/8 , y = pos[2] + hmin,
             labels = formatC(round(min(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)

        text(x = pos[1] + max(radii) + width + deltax + width/4 + width/8, y = pos[2] + hmax,
             labels = formatC(round(max(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)

        text(x = pos[1] + max(radii) + width/2 + deltax, y = pos[2] + (hmax - hmin)/2,
             labels = "(thickness)", adj = c(0.5,0), cex = 0.6)

        # Title
        text(x = pos[1] + max(radii) + deltax ,y = pos[2] + hmax  + radii[1]/3, title,
             adj = c(0,0), cex = title.cex, font = title.font)

        }

# flows (area)


        if(!is.null(x[[3]]$area)){

                hmax <- sqrt(max(x[[3]]$area))
                hmin <- sqrt(min(x[[3]]$area))

                bb <- st_bbox(x[[3]])
                width <- (bb$xmax - bb$xmin) / 15
                deltax <- (bb$xmax - bb$xmin) / 10
                rectmax <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",hmax," ",0,", ",hmax," ",hmax,", ",0," ",hmax,", ",0," ",0,"))")) + pos + c(max(radii) + deltax,0)
                rectmin <- st_as_sfc(paste0("POLYGON((",0," ",0,", ",hmin," ",0,", ",hmin," ",hmin,", ",0," ",hmin,", ",0," ",0,"))")) + pos + c(max(radii) + deltax + hmax - hmin,0)
                segments(pos[1] + max(radii) + hmax + deltax, pos[2] + hmin, pos[1] + max(radii) + hmax + deltax + width/4, pos[2] + hmin, col = border, lwd=lwd, lty = lty)
                segments(pos[1] + max(radii) + hmax + deltax, pos[2] + hmax, pos[1] + max(radii) + hmax + deltax + width/4, pos[2] + hmax, col = border, lwd=lwd, lty = lty)
                plot(rectmax, add= TRUE)
                plot(rectmin, add= TRUE)
                vals <- flows[[2]][,2] %>% st_drop_geometry()
                text(x = pos[1] + max(radii) + hmax + deltax + width/4 + width/8 , y = pos[2] + hmin,
                     labels = formatC(round(min(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)

                text(x = pos[1] + max(radii) + hmax + deltax + width/4 + width/8, y = pos[2] + hmax,
                     labels = formatC(round(max(vals),values.round), big.mark = " ", format = "fg", digits = values.round), adj = c(0,0.5), cex = values.cex)

                text(x = pos[1] + max(radii) + hmax/2 + deltax, y = pos[2] + hmax/2,
                     labels = "(area)", adj = c(0.5,0), cex = 0.6)

                # Title
                text(x = pos[1] + max(radii) + deltax ,y = pos[2] + hmax  + radii[1]/3, title,
                     adj = c(0,0), cex = title.cex, font = title.font)



        }
}
