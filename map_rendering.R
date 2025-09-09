#####================================== MAP RENDERING ========================================
#Maps are generated using the leaflet package in R. The leaflet package in R is used to create interactive 
#web maps with customizable features such as markers, layers, and popups, often used in Shiny apps for 
#spatial data visualization (it's pretty cool!)

# Reactive values to track zoom state
zoom_state <- reactiveValues(
  selected_state = NULL,
  is_zoomed = FALSE
)

# Pre-load and cache county boundaries once
us_counties_cached <- reactive({
  counties(cb = TRUE, year = 2022) %>%
    st_transform(crs = 4326)
})

# Pre-process state name conversion function
state_abbr_to_name <- function(state) {
  if(state %in% state.abb) {
    return(state.name[match(state, state.abb)])
  }
  return(state)
}

# Function to get state bounds for zooming
get_state_bounds <- function(state_name) {
  state_data <- us_states[us_states$name == state_name, ]
  if (nrow(state_data) > 0) {
    bbox <- st_bbox(state_data)
    return(list(
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
      lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
    ))
  }
  return(NULL)
}

# Function to handle state click and zoom
handle_state_click <- function(clicked_state) {
  if (is.null(clicked_state)) {
    # Reset to full US view
    zoom_state$selected_state <- NULL
    zoom_state$is_zoomed <- FALSE
  } else {
    # Zoom to specific state
    zoom_state$selected_state <- clicked_state
    zoom_state$is_zoomed <- TRUE
  }
}

# Cache processed data for each species
processed_data_cache <- reactiveValues(
  humans = NULL,
  poultry_farms = NULL,
  poultry_birds = NULL,
  cattle = NULL,
  cats = NULL,
  wildbirds = NULL,
  mammals = NULL
)

# Initialize cache when app starts
observe({
  # Pre-process humans data
  processed_data_cache$humans <- hpai_data_humans_map %>%
    mutate(State = sapply(State, state_abbr_to_name))
  
  # Pre-process poultry data
  processed_data_cache$poultry_farms <- hpai_data_poultry_map %>%
    mutate(State = sapply(State, state_abbr_to_name))
  
  processed_data_cache$poultry_birds <- hpai_data_poultry_map %>%
    mutate(State = sapply(State, state_abbr_to_name))
  
  # Pre-process cattle data
  processed_data_cache$cattle <- hpai_data_cattle_map %>%
    mutate(State = sapply(State, state_abbr_to_name))
  
  # Pre-process cats data
  processed_data_cache$cats <- hpai_data_mammals_cats_map %>%
    mutate(State = sapply(State, state_abbr_to_name))
  
  # Pre-process wildbirds data
  processed_data_cache$wildbirds <- hpai_data_wildbirds_map %>%
    mutate(State = sapply(State, state_abbr_to_name))
  
  # Pre-process mammals data
  processed_data_cache$mammals <- hpai_data_mammals_mammals_map %>%
    mutate(State = sapply(State, state_abbr_to_name))
})

# New approach: Always render all maps but control visibility
# This prevents map re-initialization and improves performance

# Main map container with all maps
output$map_container <- renderUI({
  div(
    style = "width: 100%; margin: 20px; padding: 20px;",
    
    # Humans map
    div(id = "map_humans_container",
        leafletOutput("map_humans", height="500px")
    ),
    
    # Poultry farms maps
    div(id = "map_poultry_farms_container", style = "display: none;",
        leafletOutput("map_poultry_farms", height="500px"),
        br(),
        div(
          style = "background-color: #f8f9fa; border: 1px solid #34495e; padding: 8px 12px; 
                   border-radius: 5px; font-size: clamp(10px, 4vw, 16px); color: #34495e; font-family: Arial; 
                   margin: 10px auto; font-weight: normal; text-align: center; 
                   max-width: 1000px; display: block;",
          strong("County-Level View: "), 
          "Detailed view showing HPAI cases at the county level. Hover over counties to see case information."
        ),
        div(
          style = "width: 100%; height: 400px; background-color: #f9f9f9;",
          leafletOutput("map_poultry_farms_county", height="400px")
        )
    ),
    
    # Poultry birds maps
    div(id = "map_poultry_birds_container", style = "display: none;",
        leafletOutput("map_poultry_birds", height="500px"),
        br(),
        div(
          style = "background-color: #f8f9fa; border: 1px solid #34495e; padding: 8px 12px; 
                   border-radius: 5px; font-size: clamp(10px, 4vw, 16px); color: #34495e; font-family: Arial; 
                   margin: 10px auto; font-weight: normal; text-align: center; 
                   max-width: 1000px; display: block;",
          strong("County-Level View: "), 
          "Detailed view showing HPAI cases at the county level. Hover over counties to see case information."
        ),
        div(
          style = "width: 100%; height: 400px; background-color: #f9f9f9;",
          leafletOutput("map_poultry_birds_county", height="400px")
        )
    ),
    
    # Cattle map
    div(id = "map_cattle_container", style = "display: none;",
        leafletOutput("map_cattle", height="500px")
    ),
    
    # Cats maps
    div(id = "map_cats_container", style = "display: none;",
        leafletOutput("map_cats", height="500px"),
        br(),
        div(
          style = "background-color: #f8f9fa; border: 1px solid #34495e; padding: 8px 12px; 
                   border-radius: 5px; font-size: clamp(10px, 4vw, 16px); color: #34495e; font-family: Arial; 
                   margin: 10px auto; font-weight: normal; text-align: center; 
                   max-width: 1000px; display: block;",
          strong("County-Level View: "), 
          "Detailed view showing HPAI cases at the county level. Hover over counties to see case information."
        ),
        div(
          style = "width: 100%; height: 400px; background-color: #f9f9f9;",
          leafletOutput("map_cats_county", height="400px")
        )
    ),
    
    # Wild mammals maps
    div(id = "map_mammals_container", style = "display: none;",
        leafletOutput("map_mammals", height="500px"),
        br(),
        div(
          style = "background-color: #f8f9fa; border: 1px solid #34495e; padding: 8px 12px; 
                   border-radius: 5px; font-size: clamp(10px, 4vw, 16px); color: #34495e; font-family: Arial; 
                   margin: 10px auto; font-weight: normal; text-align: center; 
                   max-width: 1000px; display: block;",
          strong("County-Level View: "), 
          "Detailed view showing HPAI cases at the county level. Hover over counties to see case information."
        ),
        div(
          style = "width: 100%; height: 400px; background-color: #f9f9f9;",
          leafletOutput("map_mammals_county", height="400px")
        )
    ),
    
    # Wild birds maps
    div(id = "map_wildbirds_container", style = "display: none;",
        leafletOutput("map_wildbirds", height="500px"),
        br(),
        div(
          style = "background-color: #f8f9fa; border: 1px solid #34495e; padding: 8px 12px; 
                   border-radius: 5px; font-size: clamp(10px, 4vw, 16px); color: #34495e; font-family: Arial; 
                   margin: 10px auto; font-weight: normal; text-align: center; 
                   max-width: 1000px; display: block;",
          strong("County-Level View: "), 
          "Detailed view showing HPAI cases at the county level. Hover over counties to see case information."
        ),
        div(
          style = "width: 100%; height: 400px; background-color: #f9f9f9;",
          leafletOutput("map_wildbirds_county", height="400px")
        )
    )
  )
})

# Observer to control map visibility based on species selection
observe({
  req(input$species)
  
  # Hide all map containers first
  hide("map_humans_container")
  hide("map_poultry_farms_container")
  hide("map_poultry_birds_container")
  hide("map_cattle_container")
  hide("map_cats_container")
  hide("map_mammals_container")
  hide("map_wildbirds_container")
  
  # Show only the selected map container
  if(input$species == "Humans") {
    show("map_humans_container")
  } else if(input$species == "Poultry (farms)") {
    show("map_poultry_farms_container")
  } else if(input$species == "Poultry (birds)") {
    show("map_poultry_birds_container")
  } else if(input$species == "Dairy cattle (farms)") {
    show("map_cattle_container")
  } else if(input$species == "Domestic cats") {
    show("map_cats_container")
  } else if(input$species == "Wild mammals") {
    show("map_mammals_container")
  } else if(input$species == "Wild birds") {
    show("map_wildbirds_container")
  }
})

output$selected_date<-renderUI({ #the text is updated dynamically to indicate what date are we looking at:
  
  req(input$date)
  HTML(paste0(
    "<div style='display: flex; justify-content: center; width: 100%;'>",
    "<div style='background-color: #f8f9fa; border: 1px solid #34495e; padding: 8px 12px; 
               border-radius: 5px; font-size: clamp(8px, 4vw, 18px); color: #34495e; font-family: Arial; 
               font-weight: normal; text-align: center; max-width: 325px;'>",
    "<strong>Selected date:</strong> ", format(input$date, "%B %d, %Y"),
    "</div>",
    "</div>"
  ))
})

### Map rendering - OPTIMIZED VERSION
# Initialize all maps once and reuse them
output$map_humans<-renderLeaflet({
  leaflet(us_states) %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_poultry_farms<-renderLeaflet({
  leaflet(us_states) %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_poultry_birds<-renderLeaflet({
  leaflet(us_states) %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_cattle<-renderLeaflet({
  leaflet(us_states) %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_cats<-renderLeaflet({
  leaflet(us_states) %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_wildbirds<-renderLeaflet({
  leaflet(us_states) %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_mammals<-renderLeaflet({
  leaflet(us_states) %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

# County-level maps - initialize once
output$map_poultry_farms_county<-renderLeaflet({
  leaflet() %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_poultry_birds_county<-renderLeaflet({
  leaflet() %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_cats_county<-renderLeaflet({
  leaflet() %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_wildbirds_county<-renderLeaflet({
  leaflet() %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

output$map_mammals_county<-renderLeaflet({
  leaflet() %>%
    setView(lng=-98.5795, lat=39.8283, zoom=4) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs",
      title = "Center Map",
      onClick = JS("function(btn, map) { map.setView([39.8283, -98.5795], 4); }")
    ))
})

# Optimized map update function
update_map <- function(map_id, data, date_col, value_col = "TotalCases", is_county = FALSE) {
  req(data, input$date)
  
  # Filter data by date
  filtered_data <- data %>%
    filter(.data[[date_col]] <= input$date)
  
  if (is_county) {
    # County-level processing
    filtered_data <- filtered_data %>%
      group_by(State, County) %>%
      summarise(TotalCases = n(), .groups = "drop") %>%
      ungroup()
    
    # Get cached county boundaries
    us_counties <- us_counties_cached()
    
    # Convert state abbreviations
    filtered_data <- filtered_data %>%
      mutate(State = sapply(State, state_abbr_to_name))
    
    # Merge with county data
    merged_data <- us_counties %>%
      left_join(filtered_data, by = c("STATE_NAME" = "State", "NAME" = "County")) %>%
      mutate(TotalCases = ifelse(is.na(TotalCases), 0, TotalCases))
    
    # Create color palette
    color_domain <- range(merged_data$TotalCases, na.rm = TRUE, finite = TRUE)
    if(length(unique(color_domain)) < 2) {
      color_domain <- c(0, max(color_domain) + 1)
    }
    
    pal <- colorNumeric(palette = "YlOrRd", domain = color_domain, na.color = "transparent")
    
    # Update map
    leafletProxy(map_id, data = merged_data) %>%
      clearShapes() %>%
      clearControls() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~ifelse(TotalCases == 0, "transparent", pal(TotalCases)),
        weight = 0.5,
        opacity = 0.6,
        color = "#999999",
        dashArray = "",
        fillOpacity = ~ifelse(TotalCases == 0, 0, 0.7),
        highlight = highlightOptions(
          weight = 1.5,
          color = "#333333",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~ifelse(TotalCases == 0, 
                       paste(NAME, ", ", STATE_NAME, ": No cases"), 
                       paste(NAME, ", ", STATE_NAME, ": ", TotalCases, " cases")),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addPolylines(
        data = us_states,
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0
      ) %>%
      {if(any(merged_data$TotalCases > 0)) {
        addLegend(., pal = pal, values = merged_data$TotalCases,
                 opacity = 0.7, title = "Reported cases", position = "bottomright",
                 labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)))
      } else .}
    
  } else {
    # State-level processing
    if (value_col == "FlockSize") {
      filtered_data <- filtered_data %>%
        group_by(State) %>%
        summarise(TotalCases = sum(FlockSize, na.rm = TRUE), .groups = "drop") %>%
        ungroup()
    } else if (value_col == "NumberOfCases") {
      filtered_data <- filtered_data %>%
        group_by(State) %>%
        summarise(TotalCases = sum(NumberOfCases, na.rm = TRUE), .groups = "drop") %>%
        ungroup()
    } else {
      filtered_data <- filtered_data %>%
        group_by(State) %>%
        summarise(TotalCases = n(), .groups = "drop") %>%
        ungroup()
    }
    
    if(nrow(filtered_data) == 0) {
      filtered_data <- tibble(State = unique(us_states$name), TotalCases = 0)
    }
    
    merged_data <- us_states %>%
      left_join(filtered_data, by = c("name" = "State")) %>%
      mutate(TotalCases = ifelse(is.na(TotalCases), 0, TotalCases))
    
    color_domain <- range(merged_data$TotalCases, na.rm = TRUE, finite = TRUE)
    if(length(unique(color_domain)) < 2) {
      color_domain <- c(0, max(color_domain) + 1)
    }
    
    pal <- colorNumeric(palette = "YlOrRd", domain = color_domain, na.color = "transparent")
    
    # Update map
    leafletProxy(map_id, data = merged_data) %>%
      clearShapes() %>%
      clearControls() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~ifelse(TotalCases == 0, "transparent", pal(TotalCases)),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "",
        fillOpacity = ~ifelse(TotalCases == 0, 0, 0.7),
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~ifelse(TotalCases == 0, 
                       paste(name, ": No cases"), 
                       paste(name, ": ", TotalCases, " cases")),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold"),
          textsize = "15px",
          direction = "auto"
        ),
        options = pathOptions(clickable = TRUE),
        layerId = ~name
      ) %>%
      {if(any(merged_data$TotalCases > 0)) {
        addLegend(., pal = pal, values = merged_data$TotalCases,
                 opacity = 0.7, title = "Reported cases", position = "bottomright",
                 labFormat = labelFormat(transform = function(x) sort(x, decreasing = FALSE)))
      } else .}
  }
}

# Initial map rendering when app starts
observe({
  req(input$species, processed_data_cache$humans, processed_data_cache$poultry_farms, 
      processed_data_cache$poultry_birds, processed_data_cache$cattle, 
      processed_data_cache$cats, processed_data_cache$wildbirds, processed_data_cache$mammals)
  
  # Wait a moment for UI to be fully rendered, then trigger initial map update
  invalidateLater(1000)  # Wait 1 second
  
  # Force initial render for the selected species
  if(input$species == "Humans") {
    update_map("map_humans", processed_data_cache$humans, "Date_confirmed", "NumberOfCases")
  } else if(input$species == "Dairy cattle (farms)") {
    update_map("map_cattle", processed_data_cache$cattle, "Date")
  }
})

# Optimized observers - only update when species and date change
observe({
  req(input$species == "Humans", processed_data_cache$humans)
  if(!is.null(input$date)) {
    update_map("map_humans", processed_data_cache$humans, "Date_confirmed", "NumberOfCases")
  }
})

observe({
  req(input$species == "Poultry (farms)", processed_data_cache$poultry_farms)
  if(!is.null(input$date)) {
    update_map("map_poultry_farms", processed_data_cache$poultry_farms, "Outbreak Date")
  }
})

observe({
  req(input$species == "Poultry (farms)", input$date, processed_data_cache$poultry_farms)
  update_map("map_poultry_farms_county", processed_data_cache$poultry_farms, "Outbreak Date", is_county = TRUE)
})

observe({
  req(input$species == "Poultry (birds)", input$date, processed_data_cache$poultry_birds)
  update_map("map_poultry_birds", processed_data_cache$poultry_birds, "Outbreak Date", "FlockSize")
})

observe({
  req(input$species == "Poultry (birds)", input$date, processed_data_cache$poultry_birds)
  update_map("map_poultry_birds_county", processed_data_cache$poultry_birds, "Outbreak Date", is_county = TRUE)
})

observe({
  req(input$species == "Dairy cattle (farms)", processed_data_cache$cattle)
  if(!is.null(input$date)) {
    update_map("map_cattle", processed_data_cache$cattle, "Date")
  }
})

observe({
  req(input$species == "Domestic cats", input$date, processed_data_cache$cats)
  update_map("map_cats", processed_data_cache$cats, "Date Collected")
})

observe({
  req(input$species == "Domestic cats", input$date, processed_data_cache$cats)
  update_map("map_cats_county", processed_data_cache$cats, "Date Collected", is_county = TRUE)
})

observe({
  req(input$species == "Wild birds", input$date, processed_data_cache$wildbirds)
  update_map("map_wildbirds", processed_data_cache$wildbirds, "Collection Date")
})

observe({
  req(input$species == "Wild birds", input$date, processed_data_cache$wildbirds)
  update_map("map_wildbirds_county", processed_data_cache$wildbirds, "Collection Date", is_county = TRUE)
})

observe({
  req(input$species == "Wild mammals", input$date, processed_data_cache$mammals)
  update_map("map_mammals", processed_data_cache$mammals, "Date Collected")
})

observe({
  req(input$species == "Wild mammals", input$date, processed_data_cache$mammals)
  update_map("map_mammals_county", processed_data_cache$mammals, "Date Collected", is_county = TRUE)
})

# TODO: Add click-to-zoom functionality in a future update
# For now, maps should load normally without zoom functionality