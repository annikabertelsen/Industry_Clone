
#Finding the central point of our lat longs
centroids_sf <- final_data %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid
centroids_sf

#POINT (37.86929 -98.19072)


#Subset by wind type
map_ProjectType_Solar <- final_data[which(final_data$`Generation Type` == 'Solar'),] 

map_Solar <- ggmap(get_googlemap(center =c(lon = -98.19072, lat = 37.86929), 
                                zoom = 5,
                                maptype = 'terrain',
                                color = 'color')) + 
  geom_point(data = map_ProjectType_Solar, aes(x = Generator_Longitude, y = Generator_Latitude), color = "red", size =.5)

print(map_Solar)


################################################################################################################################################################################
#Leaflet
####################################################################
library(leaflet)
library(sp)
library(geosphere)


#Cleaning:
final_data <- final_data %>% 
  mutate(bus_id=as.factor(bus_id))

#Calculate distances between points

#The shortest distance between two points (i.e., the ’great-circle-distance’ or ’as the crow flies’), 
#according to the ’haversine method’. This method assumes a spherical earth, ignoring ellipsoidal effects.
final_data <- final_data %>%
  mutate(dist = distHaversine(cbind(Generator_Longitude, Generator_Latitude), cbind(BUS_Longitude, BUS_Latitude)))
#Output in meters; convert to miles
final_data<- final_data %>% 
  mutate(dist = dist/1609.344)
#Now dist column is in miles



# %>% create with control shift m


#Locations Generator and BUS
map <- leaflet() %>% 
  addTiles() %>% 
  clearBounds() %>% 
  addCircleMarkers(
    data = final_data, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation Interconnection Number`, color='blue', stroke = F, fillOpacity = 1, radius = 4, group = 'Generators') %>% 
  addCircleMarkers(
    data = final_data, lng = ~BUS_Longitude, lat = ~BUS_Latitude, popup = ~bus_id, color = 'green', stroke = F, fillOpacity = 1, radius = 4, group = 'Substations') %>%
  addLayersControl(
    overlayGroups = c('Generators', 'Substations'))  
  #addPolylines()
map







#ADD LAYERS
  #https://www.rdocumentation.org/packages/leaflet/versions/2.1.1/topics/addLayersControl 


#Map Styles
  #http://leaflet-extras.github.io/leaflet-providers/preview/index.html








