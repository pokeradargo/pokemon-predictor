install.packages("elasticsearchr")

library(elasticsearchr)

## Insert
data <- data.frame(list("test","hola"))

es <- elastic("http://ci.adsmurai.net:9200", "dataoutput", "json") %index% data



install.packages("elastic")
library(elastic)

## search
es = connect(es_host="ci.adsmurai.net", es_port=9200)
resp = Search(index="datainput", type = "json", size= 1000, q = "(urbanization:\"urban\" AND terrain_type:\"Cropland/Natural vegetation mosaic\" AND close_to_water:\"Yes\" AND poke_stop_distance:\"pokestopIn100m\" AND gym_distance:\"gymIn100m\" AND continent:\"America\" AND appeared_time_of_day:\"night\" AND temperature:\"Warm\" AND pressure:\"Normal\" AND wind_speed:\"Calm\" AND weather_icon:\"clear-night\")")$hits$hits
summary(resp)

resp[[1]]$`_source`$urbanization