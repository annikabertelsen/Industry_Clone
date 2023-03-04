


library(leaflet)
library(geosphere)
library(sp)
library(rgdal)

NO_GEOMETRY <- read.csv("final_Data_No_Geometry.csv")

final_data <- NO_GEOMETRY
# %>% create with control shift m

final_data <- final_data %>%
  mutate(dist = distHaversine(cbind(Generator_Longitude, Generator_Latitude), cbind(BUS_Longitude, BUS_Latitude)))
#Convert to miles
final_data<- final_data %>% 
  mutate(dist =dist/1609.344)
#Round to two decimal points
final_data <- final_data %>% 
  mutate(across(dist,round,2))
#cleaning
final_data <- final_data %>% 
  mutate(bus_id=as.factor(bus_id))


####################################################################################################
#Map with Generators
####################
map_generators <- leaflet() %>% 
  addTiles() %>% 
  clearBounds() %>% 
  addCircleMarkers(data = final_data, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation.Interconnection.Number`, color='darkblue', stroke = F, fillOpacity = 1, radius = 6, group = 'Generators') %>% 
  addLegend("bottomright", colors= "darkblue", labels="Generators", title="Points")
map_generators




####################################################################################################
#Map with Substations
####################
map_substations <- leaflet() %>% 
  addTiles() %>% 
  clearBounds() %>% 
  addCircleMarkers(data = final_data, lng = ~BUS_Longitude, lat = ~BUS_Latitude, popup = ~bus_id, color = 'green', stroke = F, fillOpacity = 1, radius = 6, group = 'Substations') %>% 
  addLegend("bottomright", colors= "green", labels="Substations", title="Points")
map_substations



####################################################################################################
#Map with connections
######################
map <- leaflet() %>% 
  addTiles() %>% 
  clearBounds() %>% 
  addCircleMarkers(data = final_data, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation.Interconnection.Number`, color='darkblue', stroke = F, fillOpacity = 1, radius = 6, group = 'Generators') %>% 
  addCircleMarkers(data = final_data, lng = ~BUS_Longitude, lat = ~BUS_Latitude, popup = ~bus_id, color = 'green', stroke = F, fillOpacity = 1, radius = 6, group = 'Substations') %>%
  addLayersControl(overlayGroups = c('Generators', 'Substations'))

for(i in 1:nrow(final_data)){
  map <- addPolylines(map, lat = as.numeric(final_data[i, c("Generator_Latitude", "BUS_Latitude")]), 
                       lng = as.numeric(final_data[i, c("Generator_Longitude", "BUS_Longitude")]),
                       color = 'red', group = 'Connecting Lines',
                      popup = as.character(final_data[i, 'dist']))
}
map %>% addLayersControl(overlayGroups = c('Generators', 'Substations', 'Connecting Lines'))


####################################################################################################
#Map with Energy Types
#######################

# create sub tables for each type of generations
solar_gen <- final_data %>% 
  filter(`Generation.Type` == "Solar" ) #138

battery_gen <- final_data %>% 
  filter(`Generation.Type` == "Battery/Storage" ) #79

hybrid_gen <- final_data %>% 
  filter(`Generation.Type` == "Hybrid") #40

thermal_gen <- final_data %>% 
  filter(`Generation.Type` == "Thermal") #13

wind_gen <- final_data %>% 
  filter(`Generation.Type` == "Wind") #46

map_En_Type <- leaflet() %>% 
  addTiles() %>% 
  clearBounds() %>% 
  addCircleMarkers(data = solar_gen, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation.Interconnection.Number`, color='orange', stroke = F, fillOpacity = 1, radius = 6, group = 'Solar') %>% 
  
  addCircleMarkers(data = battery_gen, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation.Interconnection.Number`, color='black', stroke = F, fillOpacity = 1, radius = 6, group = 'Battery') %>%
  
  addCircleMarkers(data = hybrid_gen, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation.Interconnection.Number`, color='green', stroke = F, fillOpacity = 1, radius = 6, group = 'Hybrid') %>% 
  
  addCircleMarkers(data = thermal_gen, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation.Interconnection.Number`, color='red', stroke = F, fillOpacity = 1, radius = 6, group = 'Thermal') %>% 
  
  addCircleMarkers(data = wind_gen, lng = ~Generator_Longitude, lat = ~Generator_Latitude, popup = ~`Generation.Interconnection.Number`, color='blue', stroke = F, fillOpacity = 1, radius = 6, group = 'Wind') %>% 
  
  addLayersControl(overlayGroups = c('Solar', 'Battery','Hybrid','Thermal','Wind'))

map_En_Type


#