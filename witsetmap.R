# Witset Map

library(sf)
library(mapview)
library(leaflet)
library(ggplot2)
library(dplyr)

SK.CU.river <- st_read(dsn = "./River_Type_Sockeye_Salmon_CU_Shape/SER_CU_BOUNDARY_En.shp")

SK.CU.river.Skeena <- SK.CU.river %>% 
  filter(CU_NAME %in% "Skeena River")

pts.df <- st_read("Witset.kml")

pts <- as.data.frame(st_coordinates(pts.df))
pts$pt <- pts.df$Name
str(pts)
pts <- st_as_sf(x= pts, coords = c("X", "Y"), crs= 4326)

SK.CU.lake <- st_read(dsn = "./Lake_Type_Sockeye_Salmon_CU_Shape/SEL_CU_BOUNDARY_En.shp")

SK.CU.lake.Bulkley <- SK.CU.lake %>% 
  filter(CU_NAME %in% c("Atna","Morice","Maxan","Bulkley"))

major.watersheds <- st_read(dsn= "./BC_MAJOR_WATERSHEDS/MJR_WTRSHD_polygon.shp")

major.watersheds.skeena <- major.watersheds %>% 
  filter(MJR_WTRSHM %in% "Skeena River")
major.watersheds.bulkley <- major.watersheds %>% 
  filter(MJR_WTRSHM %in% "Bulkley River")

mapview(list(major.watersheds.skeena, major.watersheds.bulkley),
        col.regions = c("darkgreen", "blue"))


mapview(list(major.watersheds.bulkley,SK.CU.lake.Bulkley, pts),
        col.regions = c("green", "blue", "pink"),
        legend = list(F,F,T),
        addStaticLabels(label = pts$name,
                        noHide = TRUE,
                        direction = 'top',
                        textOnly = TRUE,
                        textsize = "20px"))

leaflet(pts) %>% 
  addTiles() %>% 
  addLabelOnlyMarkers(label= ~pt)

points <- tribble(~name, ~lat, ~lon,
                  'Point A',     -38.119151, 145.401893,
                  'Point B',     -38.127870, 145.685598)

points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)

leaflet(points_sf) %>%
  addTiles() %>%
  addLabelOnlyMarkers(label =  ~name, 
                      labelOptions = labelOptions(noHide = T,
                                                  direction = 'top',
                                                  textOnly = T))
