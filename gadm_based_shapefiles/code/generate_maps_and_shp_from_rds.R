library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

# Set the directory to master folder
setwd("..")

# Run for each gadm-based geography
files <- c("gadm1_nuts2", "gadm1_nuts3_counties")

for(file_name in files){
  dat_in <- readRDS(str_interp("rds_format/${file_name}.Rds"))

  st_write(dat_in, str_interp("shp_format/${file_name}.shp"))
  
  dat_simple <- st_simplify(dat_in, dTolerance = .02)
  
  labels <- sprintf(
    "<strong>Key: </strong>%s<br/>
      <strong>Name: </strong>%s<br/>
      <strong>County: </strong>%s<br/>",
    dat_simple$key,
    dat_simple$name,
    dat_simple$country) %>%
    lapply(htmltools::HTML)
  
  m <- leaflet() %>%
    addProviderTiles(
      "Esri.WorldStreetMap",
      options = leafletOptions()
    ) %>%
    addPolygons(
      data=dat_simple,
      weight=1,
      opacity=0.5,
      fillOpacity=0.3,
      color="Blue",
      fillColor = "Blue",
      group = "shapes1",
      label = labels,
      highlight = highlightOptions(
        weight = 4,
        color = "black"
      )
    )
  
  saveWidget(m, str_interp("map_of_${file_name}.html"))
  
}