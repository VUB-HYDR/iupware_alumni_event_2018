
### GIS in R ###
library(sf)
library(raster)

## Load in a raster file
nete_dem = raster("./data/r25_nete.tif")
plot(nete_dem)

## Transform a raster file & ggplot
library(magrittr)
library(ggplot2)

lng_lat = st_crs(4326)
nete_dem = projectRaster(from = nete_dem, crs=lng_lat$proj4string )

nete_dem.df = rasterToPoints(nete_dem) %>% as.data.frame()
ggplot(nete_dem.df, aes(x, y, fill = r25_nete)) + geom_raster() + coord_equal()+
  scale_fill_gradientn(colors= rev(rainbow(10)))


### Load in a shapefile
# previously
nete_sub = rgdal::readOGR("./data/nete_subbasins/nete_subbasins.shp")
str(nete_sub)

# with sf
nete_sub = st_read("./data/nete_subbasins/nete_subbasins.shp")
str(nete_sub)
head(nete_sub)
plot(nete_sub)

### Transform a shapefile
nete_sub = st_transform(nete_sub, crs = st_crs(4326))


### Make a shapefile (wells)
wells_coords = read.csv("./data/wells_loc.csv")
wells = read.csv("./data/wells_new.csv")
wells = full_join(wells, wells_coords, by = 'id')

wells_shp = st_as_sf(wells, coords = c("long", "lat"), crs=lng_lat)
plot(wells_shp['q'])

st_write(wells_shp, "./data/wells.shp", delete_dsn = T)

### Plot
# geometry
plot(st_geometry(wells_shp))

# mutliple layers
plot(nete_sub['nete_catch'])
plot(st_geometry(wells_shp), add=T)

#### Manipulating a shapefile
library(dplyr)

wells_shp %>% filter(stratigraphy == 'Pliocene') %>% plot()

# extract coordinates
st_coordinates(wells_shp) %>% head()

### Clipping a shapefile
# select all wells in the Grote Nete catchment

grote_nete = nete_sub %>% filter(nete_catch == 'Grote Nete')
plot(st_geometry(grote_nete))

# subset sf wells based on sf grote nete
wells_grote = wells_shp[grote_nete, op = st_intersects]
plot(st_geometry(wells_grote))

### Variograms of T
library(gstat)

# set up variogram
vagm = variogram(log(tran)~1, data = na.omit(wells), locations = ~long+lat)
head(vagm)
plot(vagm)

# create variogram model
vagm_model = vgm(psill = 8, model = 'Exp', range = 0.04, nugget = 5.9)

# fit the model
vagm_fit = fit.variogram(vagm, model = vagm_model)
plot(vagm, vagm_fit)


### Interactive plotting
library(leaflet)

leaflet() %>% addTiles() %>% addMarkers(lng = 4.398320, lat = 50.822449, popup = 'HYDR')
  

# make color palette
palf = colorFactor("RdYlBu", nete_sub$nete_catch)

leaflet() %>% addProviderTiles('Esri.WorldImagery') %>%
  addPolygons(data = nete_sub, group = 'Subbasins', 
              fillColor = ~palf(nete_catch),
              color = 'black', fillOpacity = 0.3,
              highlightOptions = highlightOptions(color = 'white', bringToFront=T)) %>%
  addCircles(data = wells_shp, radius=~q, group = 'Wells', stroke=F, label = ~as.character(q),
             labelOptions = labelOptions( textsize='30px')) %>%
  addLegend(pal = palf, values = nete_sub$nete_catch) %>%
  addLayersControl(overlayGroups = c('Wells', 'Subbasins'))




# # Make target map
# data("meuse.grid")
# ggplot(meuse.grid, aes(x, y)) + geom_point() + coord_equal()
# 
# 
# # Krige
# # computational time increases exponentially with gridsize
# kriged = krige(log(zinc)~1, data = na.omit(meuse), locations = ~x+y, 
#                newdata = meuse.grid, model = vagm_fit)
# head(kriged)
# 
# ggplot() + geom_raster(data = kriged, aes(x, y, fill=var1.pred)) + 
#   geom_point(data = meuse, aes(x, y)) + 
#   coord_equal() + 
#   scale_fill_gradientn(colors = rev(rainbow(10)))

