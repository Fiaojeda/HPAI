#####================================== CACHE DATA HANDLING =====================================
#To avoid errors, this piece of code removes previously loaded map when switching between categories.

observeEvent(input$species, { #triggers whenever the 'species' input changes
  # Clear all maps when species changes - this is now much faster with the optimized system
  leafletProxy("map_humans") %>% clearShapes() %>% clearControls()
  leafletProxy("map_poultry_farms") %>% clearShapes() %>% clearControls()
  leafletProxy("map_poultry_birds") %>% clearShapes() %>% clearControls()
  leafletProxy("map_cattle") %>% clearShapes() %>% clearControls()
  leafletProxy("map_cats") %>% clearShapes() %>% clearControls()
  leafletProxy("map_wildbirds") %>% clearShapes() %>% clearControls()
  leafletProxy("map_mammals") %>% clearShapes() %>% clearControls()
  
  # Clear county maps
  leafletProxy("map_poultry_farms_county") %>% clearShapes() %>% clearControls()
  leafletProxy("map_poultry_birds_county") %>% clearShapes() %>% clearControls()
  leafletProxy("map_cats_county") %>% clearShapes() %>% clearControls()
  leafletProxy("map_wildbirds_county") %>% clearShapes() %>% clearControls()
  leafletProxy("map_mammals_county") %>% clearShapes() %>% clearControls()
})