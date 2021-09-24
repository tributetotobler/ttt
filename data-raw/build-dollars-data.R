library(sf)
library(stringr)
library(xml2)
library(purrr)
library(ggplot2)
library(dplyr)
library(readr)
library(Matrix)
svg = read_xml("./data-raw/dollars.svg")
paths = svg %>% xml_find_all("//svg:path")
polygons = paths %>% map(function(path){
  path_str = xml_attr(path,"d")
  list_coords = path_str %>% str_split(" ") %>% unlist() %>%
    keep(function(x){! x %in% c("m","h","v","z","l","L","M","Z")}) %>%
    map(function(x){str_split(x,",") %>% unlist()}) %>%
    map(as.numeric)

  coords = do.call(rbind,list_coords)
  coords_cum = cbind(cumsum(coords[,1]),-cumsum(coords[,2]))
  coords_closed = rbind(coords_cum,coords_cum[1,])
  st_polygon(list(coords_closed))
})


outline = st_as_sfc(polygons[paths %>% map(function(x){xml_attr(x,"id")}) %>% unlist() =="outline"])
polygons = polygons[paths %>% map(function(x){xml_attr(x,"id")}) %>% unlist() !="outline"]
plot(st_as_sfc(polygons))
fed_polygons = st_sf(geom=st_as_sfc(polygons))

circles = svg %>% xml_find_all("//svg:circle")
points = circles %>% map(function(circle){
  pos_str = c(xml_attr(circle,"cx"),xml_attr(circle,"cy"))
  coords = as.numeric(pos_str)
  coords[2]=-coords[2]
  st_point(coords)
})
plot(st_as_sfc(points),add=TRUE,col = "red")

cities_svg =st_sf(geom=st_as_sfc(points))
cities_svg$id = circles %>% map(function(ci){xml_attr(ci,"id")}) %>% unlist()

geojoin = st_within(cities_svg,fed_polygons) %>% unlist()
fed_polygons$id = cities_svg$id[geojoin]

ggplot()+geom_sf(data=st_as_sfc(polygons),fill="#ffffff")+geom_sf(data=cities_svg)+geom_sf_text(data=cities_svg,aes(label=id),nudge_y = -5,nudge_x = 10)+theme_void()

# Recalage ?
cities_loc = read_csv("./data-raw/dollars-cities.csv")
xy_svg = cities_svg %>% mutate(x_svg = st_coordinates(geom)[,1],y_svg = st_coordinates(geom)[,2]) %>% st_drop_geometry()
cities_bireg = cities_loc %>% left_join(xy_svg,by="id")
tra = lm2(x + y ~ x_svg + y_svg,cities_bireg,"affine")
cities_bireg[,c("x_rec","y_rec")]=predict(tra,cities_bireg[,c("x_svg","y_svg")])
plot(cities_bireg$x,cities_bireg$y,type='p')
points(cities_bireg$x_rec,cities_bireg$y_rec,col='red')

fed_polygons = fed_polygons %>% left_join(cities_loc,by="id")

st_geometry(fed_polygons) = fed_polygons$geom %>% map(function(pol){
  old_coords = data.frame(x_svg=st_coordinates(pol)[,1],y_svg=st_coordinates(pol)[,2])
  new_coords = predict(tra,old_coords)
  st_polygon(list(new_coords))
  }) %>% st_as_sfc()

plot(fed_polygons)

old_coords = data.frame(x_svg=st_coordinates(outline)[,1],y_svg=st_coordinates(outline)[,2])
new_coords = predict(tra,old_coords)
outline=st_polygon(list(new_coords))

