#####================================== CUMULATIVE CASES PLOTS =====================================
#This code generates cumulative case plots for the different categories

# Define theme function (fallback if hrbrthemes not available)
if (exists("theme_function")) {
  # Use the theme function defined in app.R
  theme_function <- theme_function
} else {
  theme_function <- function() {
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey90", size = 0.5),
      panel.grid.minor = element_line(color = "grey95", size = 0.25),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11)
    )
  }
}

# Function to apply consistent plot styling
apply_plot_styling <- function(p) {
  p + theme(
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey90", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(fill = "white", color = NA)
  )
}

# Function to apply consistent plotly layout
apply_plotly_layout <- function(plotly_obj, y_title = "Cumulative reported cases") {
  plotly_obj %>% layout(
    autosize = TRUE,
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    font = list(size = 18, color = "black"),
    xaxis = list(
      tickfont = list(size = 18, family = "Arial", color = "black"),
      gridcolor = "lightgrey",
      zerolinecolor = "lightgrey"
    ),
    yaxis = list(
      tickfont = list(size = 18, family = "Arial", color = "black"),
      gridcolor = "lightgrey",
      zerolinecolor = "lightgrey",
      title = list(
        text = y_title,
        font = list(size = 20, family = "Arial", color = "black", weight = "bold"),
        standoff = 20)
    ),
    title = list(font = list(size = 20, color = "black")),
    legend = list(
      font = list(size = 15, family = "Arial", color = "black"),
      bgcolor = "white",
      bordercolor = "lightgrey"
    ),
    margin = list(l = 60, r = 60, t = 10, b = 60)
  )
}

# Plot type selector UI
output$plot_type_selector <- renderUI({
  req(input$species)
  
  if (input$species %in% c("Humans", "Poultry (farms)", "Poultry (birds)", 
                           "Dairy cattle (farms)", "Domestic cats", 
                           "Wild birds", "Wild mammals")) {
    
    div(
      style = "margin-bottom: 20px; text-align: center;",
      radioButtons(
        "plot_type",
        "Select View:",
        choices = c(
          "Cumulative Cases" = "cumulative",
          "Case Trends by State" = "daily_trends"
        ),
        selected = "cumulative",
        inline = TRUE
      )
    )
  } else {
    div() # Empty div for species without plots
  }
})

# Timeline slider UI
output$timeline_slider_ui <- renderUI({
  req(input$species)
  
  if (input$species %in% c("Humans", "Poultry (farms)", "Poultry (birds)", 
                           "Dairy cattle (farms)", "Domestic cats", 
                           "Wild birds", "Wild mammals")) {
    
    # Get date range based on species
    date_range <- get_date_range_for_species(input$species)
    
    # Ensure dates are valid
    if (is.na(date_range$min_date) || is.na(date_range$max_date)) {
      return(div(style = "color: red;", "Error: Could not determine date range for this species"))
    }
    
    div(
      style = "width: 100%;",
      sliderInput(
        "timeline_range",
        NULL,
        min = as.Date(date_range$min_date),
        max = as.Date(date_range$max_date),
        value = c(as.Date(date_range$min_date), as.Date(date_range$max_date)),
        timeFormat = "%b %d, '%y",
        step = 1,
        width = "100%"
      )
    )
  } else {
    div() # Empty div for species without plots
  }
})

# Helper function to generate color palette for states
generate_color_palette <- function(states, total_color = "#000000") {
  # Safety check: ensure states is not NULL or empty
  if (is.null(states) || length(states) == 0) {
    warning("generate_color_palette: states is NULL or empty")
    return(c("Total" = total_color))
  }
  
  # Remove "Total" from states for color assignment
  non_total_states <- states[states != "Total"]
  
  # Safety check: if no non-Total states, return just Total
  if (length(non_total_states) == 0) {
    return(c("Total" = total_color))
  }
  
  # Extended color palette with 50+ distinct colors
  color_palette <- c(
    "#1B9E77", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
    "#0b8a8c", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#e85409",
    "#9e2bb3", "#A65628", "#F781BF", "#00c3ff", "#FF6B6B", "#4ECDC4", 
    "#45B7D1", "#96CEB4", "#FFEAA7", "#DDA0DD", "#98D8C8", "#F7DC6F",
    "#BB8FCE", "#85C1E9", "#F8C471", "#82E0AA", "#F1948A", "#85C1E9",
    "#D7BDE2", "#A9CCE3", "#FAD7A0", "#ABEBC6", "#F5B7B1", "#AED6F1",
    "#F9E79F", "#D5A6BD", "#A3E4D7", "#F8C471", "#D2B4DE", "#A9DFBF",
    "#FADBD8", "#D1F2EB", "#FCF3CF", "#D7BDE2", "#A9CCE3", "#FAD7A0",
    "#ABEBC6", "#F5B7B1", "#AED6F1", "#F9E79F", "#D5A6BD", "#A3E4D7"
  )
  
  # Ensure we have enough colors
  if (length(non_total_states) > length(color_palette)) {
    # Repeat colors if we need more
    color_palette <- rep(color_palette, ceiling(length(non_total_states) / length(color_palette)))
  }
  
  # Create named vector for non-Total states
  state_colors <- setNames(
    color_palette[1:length(non_total_states)],
    sort(non_total_states)
  )
  
  # Add Total color
  if ("Total" %in% states) {
    state_colors <- c(state_colors, "Total" = total_color)
  }
  
  return(state_colors)
}

# Helper function to get date range for each species
get_date_range_for_species <- function(species) {
  print(paste("Getting date range for species:", species))
  tryCatch({
    if (species == "Humans") {
      # Parse dates properly for humans data
      dates <- mdy(hpai_data_humans$Date_confirmed)
      min_date <- min(dates, na.rm = TRUE)
      max_date <- max(dates, na.rm = TRUE)
    } else if (species %in% c("Poultry (farms)", "Poultry (birds)")) {
      # Parse dates properly for poultry data
      dates <- mdy(hpai_data_poultry$`Outbreak Date`)
      min_date <- min(dates, na.rm = TRUE)
      max_date <- max(dates, na.rm = TRUE)
    } else if (species == "Dairy cattle (farms)") {
      # Parse dates properly for cattle data - use the already parsed dates
      dates <- hpai_data_cattle_cum$Date
      min_date <- min(dates, na.rm = TRUE)
      max_date <- max(dates, na.rm = TRUE)
    } else if (species == "Domestic cats") {
      # Parse dates properly for cats data - use the already parsed dates
      dates <- hpai_data_mammals_cats_map$`Date Detected`
      min_date <- min(dates, na.rm = TRUE)
      max_date <- max(dates, na.rm = TRUE)
    } else if (species == "Wild birds") {
      # Parse dates properly for wild birds data - use the already parsed dates
      dates <- hpai_data_wildbirds_map$`Collection Date`
      min_date <- min(dates, na.rm = TRUE)
      max_date <- max(dates, na.rm = TRUE)
    } else if (species == "Wild mammals") {
      # Parse dates properly for wild mammals data - use the already parsed dates
      dates <- hpai_data_mammals_mammals_map$`Date Detected`
      min_date <- min(dates, na.rm = TRUE)
      max_date <- max(dates, na.rm = TRUE)
    } else {
      min_date <- Sys.Date() - 365
      max_date <- Sys.Date()
    }
    
    # Ensure we have valid dates
    if (is.na(min_date) || is.na(max_date)) {
      stop("Could not determine valid date range")
    }
    
    print(paste("Date range for", species, ":", min_date, "to", max_date))
    list(min_date = min_date, max_date = max_date)
  }, error = function(e) {
    # Fallback to default dates if there's an error
    list(min_date = Sys.Date() - 365, max_date = Sys.Date())
  })
}

output$plot_title <- renderUI({
  req(input$species)  
  #titles change according to the population selected
  title_text <- if (input$species == "Humans") {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases in humans"
  } else if (input$species == "Poultry (farms)") {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases in poultry (farms)"
  } else if (input$species == "Poultry (birds)") {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases in poultry (birds)"
  } else if (input$species == "Dairy cattle (farms)") {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases in dairy cattle (farms)"
  } else if (input$species == "Domestic cats") {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases in domestic cats"
  } else if (input$species == "Wild birds") {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases in wild birds"
  } else if (input$species == "Wild mammals") {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases in wild mammals"
  } else {
    "Highly Pathogenic Avian Influenza (HPAI) cumulative reported cases"
  }
})

output$dynamic_plot <- renderUI({ #renders ui element that switches between plots or a placeholder
  req(input$species, input$plot_type) #ensures species input and plot type are available
  
  if (input$species %in% c("Humans", "Poultry (farms)", "Poultry (birds)", 
                           "Dairy cattle (farms)", "Domestic cats", 
                           "Wild birds", "Wild mammals")) { #checks if species has an associated plot
    
    if (input$plot_type == "daily_trends") {
      # Show sparkline state trends
      uiOutput("sparkline_state_trends")
    } else {
      # Show cumulative plot (default)
      plotname <- switch(input$species, #maps each species to its cumulative plot id
                         "Humans" = "plotHumansCum",
                         "Poultry (farms)" = "plotPoultryCum_farms",
                         "Poultry (birds)" = "plotPoultryCum_birds",
                         "Dairy cattle (farms)" = "plotCattleCum",
                         "Domestic cats" = "plotCatsCum",
                         "Wild birds" = "plotWildbirdsCum",
                         "Wild mammals" = "plotWildmammalsCum")
      
      plotlyOutput(plotname, height = "600px", width = "100%") #outputs selected plotly plot
    }
  } else { #fallback when species is not in the predefined list
    div(style="text-align: center; padding: 25px; margin-top: 100px", 
        img(src="WIP.png", height="300px")) #displays work-in-progress image
  }
})

#Humans
output$plotHumansCum <- renderPlotly({ #renders cumulative human case plot as an interactive plotly object
  req(input$species=="Humans", input$date) #ensures species is humans and a date is selected
  
  # Make the plot reactive to timeline changes and state selection
  timeline_range <- input$timeline_range
  
  hpai_data_humans <- hpai_data_humans %>%
    mutate(Date_confirmed=mdy(Date_confirmed)) %>% #parses date strings into date objects
    group_by(State, Date_confirmed) %>% #groups data by state and date
    summarise( #aggregates daily data by state and date
      Cases=sum(NumberOfCases), #sums number of cases
      Status=paste(unique(Status), collapse=", "), #collapses unique status values into a string
      Age=paste(unique(Age), collapse=", "), #collapses unique age values into a string
      Source=paste(unique(Source), collapse=", "), #collapses unique source values into a string
      .groups="drop" #ungroups after summarization
    ) %>%
    arrange(Date_confirmed) %>% #sorts rows by date
    group_by(State) %>% #regroups by state
    mutate(Cumulative_Cases=cumsum(Cases)) #computes cumulative cases per state
  
  hpai_data_humans <- hpai_data_humans %>%
    group_by(State) %>% #groups again by state
    complete(Date_confirmed=seq(min(Date_confirmed), max(Date_confirmed), by="day"), 
             fill=list(Cases=0, Cumulative_Cases=NA)) %>% #fills missing dates with zero cases
    fill(Cumulative_Cases, .direction="down") %>% #fills cumulative cases downward
    ungroup() #removes grouping
  
  hpai_data_humans_cum <- hpai_data_humans %>%
    mutate(Date_confirmed=as.Date(Date_confirmed, format="%m/%d/%Y")) #ensures date format is consistent
  
  hpai_data_humans_cum <- hpai_data_humans_cum %>%
    complete(Date_confirmed=seq(min(Date_confirmed), max(Date_confirmed), by="day"), 
             State, 
             fill=list(Cases=0, Cumulative_Cases=NA)) %>% #fills all combinations of state and date
    group_by(State) %>% #regroups by state
    fill(Cumulative_Cases, .direction="down") %>% #fills cumulative cases downward
    ungroup() #ungroups
  
  state_starts <- hpai_data_humans_cum %>%
    group_by(State) %>% #groups by state
    summarise(Date_confirmed=min(Date_confirmed) - 1, #adds one day before the first case
              Cumulative_Cases=0, #initializes with zero cases
              .groups="drop") #ungroups
  
  hpai_data_humans_cum <- bind_rows(hpai_data_humans_cum, state_starts) %>% #adds start rows to the data
    arrange(State, Date_confirmed) %>% #sorts again
    group_by(State) %>% #groups again
    fill(Cumulative_Cases, .direction="down") %>% #fills downward again
    ungroup() %>% #ungroups
    mutate(tooltip_text = paste0("Cumulative cases: ", Cumulative_Cases, " (", State, ", ", Date_confirmed, ")",
                                 ifelse(!is.na(Cases) & Cases > 0, 
                                        paste0(" (", Status, ")"), 
                                        ""))) #always show cumulative cases
  
  hpai_data_humans_cum <- hpai_data_humans_cum %>%
    group_by(State) %>% #groups by state
    arrange(Date_confirmed) %>% #sorts dates
    mutate(Case_Increase=Cumulative_Cases > lag(Cumulative_Cases, default=0)) %>% #flags if new cases occurred
    ungroup() #ungroups
  
  # Add total cases line
  total_cases <- hpai_data_humans_cum %>%
    group_by(Date_confirmed) %>%
    summarise(
      Cumulative_Cases = sum(Cumulative_Cases, na.rm = TRUE),
      Cases = sum(Cases, na.rm = TRUE),
      State = "Total",
      tooltip_text = paste0("Total cases: ", sum(Cumulative_Cases, na.rm = TRUE), " (", Date_confirmed, ")"),
      Case_Increase = sum(Cases, na.rm = TRUE) > 0,
      .groups = "drop"
    )
  
  hpai_data_humans_cum <- bind_rows(hpai_data_humans_cum, total_cases)
  
  # Get all states (excluding Total)
  all_states <- hpai_data_humans_cum %>%
    filter(State != "Total") %>%
    pull(State) %>%
    unique()
  
  # Filter data to show all states + Total
  hpai_data_humans_cum_filtered <- hpai_data_humans_cum %>%
    filter(State %in% c(all_states, "Total"))
  
  # Apply timeline filtering if timeline range is selected
  if (!is.null(timeline_range) && length(timeline_range) == 2) {
    # For cumulative data, we need to preserve the cumulative nature
    # Filter individual state data normally
    state_data_filtered <- hpai_data_humans_cum_filtered %>%
      filter(State != "Total") %>%
      filter(Date_confirmed >= timeline_range[1] & Date_confirmed <= timeline_range[2])
    
    # For Total line, we need to recalculate cumulative sum within the filtered range
    # but ensure it doesn't go below the previous cumulative values
    total_data_filtered <- hpai_data_humans_cum_filtered %>%
      filter(State == "Total") %>%
      filter(Date_confirmed >= timeline_range[1] & Date_confirmed <= timeline_range[2])
    
    # Recalculate the Total line to ensure it's truly cumulative within the filtered range
    if (nrow(total_data_filtered) > 0) {
      # Get the cumulative sum of all states within the filtered range
      state_cumulative_in_range <- state_data_filtered %>%
        group_by(Date_confirmed) %>%
        summarise(
          Daily_Total = sum(Cumulative_Cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(Date_confirmed) %>%
        mutate(Cumulative_Total = cummax(Daily_Total))  # Use cummax to ensure it never decreases
      
      # Update the total data with the corrected cumulative values
      total_data_filtered <- total_data_filtered %>%
        select(-Cumulative_Cases) %>%
        left_join(state_cumulative_in_range, by = "Date_confirmed") %>%
        mutate(
          Cumulative_Cases = Cumulative_Total,
          tooltip_text = paste0("Total cases: ", Cumulative_Cases, " (", Date_confirmed, ")")
        ) %>%
        select(-Daily_Total, -Cumulative_Total)
    }
    
    # Combine the filtered data
    hpai_data_humans_cum_filtered <- bind_rows(state_data_filtered, total_data_filtered)
  }
  
  p <- ggplot(hpai_data_humans_cum_filtered, aes( #starts ggplot
    x=Date_confirmed, #x-axis is date
    y=Cumulative_Cases, #y-axis is cumulative cases
    color=State, #line color by state
    group=State, #grouping by state
    text=tooltip_text #tooltip for plotly
  )) +
    geom_line(size=0.8, alpha=0.7) + #adds thin lines for sparkline effect
    labs(x="Date confirmed", 
         y="Cumulative reported cases", 
         color="State") + #adds labels
    scale_x_date(date_breaks="3 months", date_labels="%b %Y") + #formats x-axis labels
    scale_y_continuous( #formats y-axis
      limits=c(0, max(hpai_data_humans_cum_filtered$Cumulative_Cases, na.rm=TRUE)),  
      breaks=pretty(c(0, max(hpai_data_humans_cum_filtered$Cumulative_Cases, na.rm=TRUE)), n=10)
    ) +
    theme_function() + #applies clean theme
    theme(
      axis.text.x=element_text(angle=45, hjust=1), #rotates x-axis labels
      axis.title.x=element_blank(), #removes x-axis title
      panel.grid.major=element_line(color="grey90", size=0.1), #lightens major grid lines
      panel.grid.minor=element_blank() #removes minor grid lines
    ) +
    scale_color_manual(values = generate_color_palette(unique(hpai_data_humans_cum_filtered$State)))
  
  ggplotly(p, tooltip="text") %>% #converts ggplot to interactive plotly
    layout( #customizes layout
      autosize=TRUE,
      font=list(size=18), #global font size
      xaxis=list(tickfont=list(size=18, family="Arial", color="black")),
      yaxis=list(
        tickfont=list(size=18, family="Arial", color="black"),
        title=list(
          text="Cumulative reported cases", #y-axis title
          font=list(size=20, family="Arial", color="black", weight="bold"),
          standoff=20)
      ),
      title=list(font=list(size=20)),
      legend=list(
        font=list(size=15, family="Arial", color="black"), 
        title=list(
          text="State<br>", 
          font=list(size=18, family="Arial", color="black", weight="bold")
        )
      )
    )
})

#Poultry (flocks)
output$plotPoultryCum_birds <- renderPlotly({
  req(input$species == "Poultry (birds)", input$date)
  
  # Make the plot reactive to timeline changes
  timeline_range <- input$timeline_range
  
  hpai_data_poultry_cum <- hpai_data_poultry %>%
    mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format="%m-%d-%Y")) %>%
    group_by(State, County, `Outbreak Date`) %>%
    summarise(
      FlockSize = sum(FlockSize, na.rm = TRUE),
      FlockType = paste(unique(FlockType), collapse=", "),
      .groups = "drop"
    ) %>%
    arrange(`Outbreak Date`) %>%
    group_by(State) %>%
    mutate(Cumulative_FlockSize = cumsum(FlockSize))
  
  hpai_data_poultry_cum <- hpai_data_poultry_cum %>%
    group_by(State) %>%
    complete(`Outbreak Date` = seq(min(`Outbreak Date`), max(`Outbreak Date`), by="day"), 
             fill = list(FlockSize = 0, Cumulative_FlockSize = NA)) %>%
    fill(Cumulative_FlockSize, .direction = "down") %>% 
    ungroup()
  
  state_starts <- hpai_data_poultry_cum %>%
    group_by(State) %>%
    summarise(
      `Outbreak Date` = min(`Outbreak Date`) - 1,
      Cumulative_FlockSize = 0,
      .groups = "drop"
    )
  
  hpai_data_poultry_cum <- bind_rows(hpai_data_poultry_cum, state_starts) %>%
    arrange(State, `Outbreak Date`) %>%
    group_by(State) %>%
    fill(Cumulative_FlockSize, .direction = "down") %>% 
    ungroup() %>%
    mutate(tooltip_text = paste0("Cumulative birds: ", format(Cumulative_FlockSize, big.mark=","), 
                                 " (", State, ", ", `Outbreak Date`, ")",
                                 ifelse(!is.na(FlockSize) & FlockSize > 0, 
                                        paste0("\nNew birds: ", format(FlockSize, big.mark=","), 
                                               "\nFlock Type: ", FlockType, "\nCounty: ", County), 
                                        "\nNew birds: 0")))  
  
  hpai_data_poultry_cum <- hpai_data_poultry_cum %>%
    group_by(State) %>%
    arrange(`Outbreak Date`) %>%
    mutate(Flock_Increase = Cumulative_FlockSize > lag(Cumulative_FlockSize, default = 0)) %>% 
    ungroup()
  
  # Add total cases line
  total_cases <- hpai_data_poultry_cum %>%
    group_by(`Outbreak Date`) %>%
    summarise(
      Cumulative_FlockSize = sum(Cumulative_FlockSize, na.rm = TRUE),
      FlockSize = sum(FlockSize, na.rm = TRUE),
      State = "Total",
      tooltip_text = paste0("Total birds: ", format(sum(Cumulative_FlockSize, na.rm = TRUE), big.mark=","), " (", `Outbreak Date`, ")"),
      Flock_Increase = sum(FlockSize, na.rm = TRUE) > 0,
      .groups = "drop"
    )
  
  hpai_data_poultry_cum <- bind_rows(hpai_data_poultry_cum, total_cases)
  
  # Get all states (excluding Total)
  all_states <- hpai_data_poultry_cum %>%
    filter(State != "Total") %>%
    pull(State) %>%
    unique()
  
  # Filter data to show all states + Total
  hpai_data_poultry_cum_filtered <- hpai_data_poultry_cum %>%
    filter(State %in% c(all_states, "Total"))
  
  # Apply timeline filtering if timeline range is selected
  if (!is.null(timeline_range) && length(timeline_range) == 2) {
    # For cumulative data, we need to preserve the cumulative nature
    # Filter individual state data normally
    state_data_filtered <- hpai_data_poultry_cum_filtered %>%
      filter(State != "Total") %>%
      filter(`Outbreak Date` >= timeline_range[1] & `Outbreak Date` <= timeline_range[2])
    
    # For Total line, we need to recalculate cumulative sum within the filtered range
    # but ensure it doesn't go below the previous cumulative values
    total_data_filtered <- hpai_data_poultry_cum_filtered %>%
      filter(State == "Total") %>%
      filter(`Outbreak Date` >= timeline_range[1] & `Outbreak Date` <= timeline_range[2])
    
    # Recalculate the Total line to ensure it's truly cumulative within the filtered range
    if (nrow(total_data_filtered) > 0) {
      # Get the cumulative sum of all states within the filtered range
      state_cumulative_in_range <- state_data_filtered %>%
        group_by(`Outbreak Date`) %>%
        summarise(
          Daily_Total = sum(Cumulative_FlockSize, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(`Outbreak Date`) %>%
        mutate(Cumulative_Total = cummax(Daily_Total))  # Use cummax to ensure it never decreases
      
      # Update the total data with the corrected cumulative values
      total_data_filtered <- total_data_filtered %>%
        select(-Cumulative_FlockSize) %>%
        left_join(state_cumulative_in_range, by = "Outbreak Date") %>%
        mutate(
          Cumulative_FlockSize = Cumulative_Total,
          tooltip_text = paste0("Total birds: ", format(Cumulative_FlockSize, big.mark=","), " (", `Outbreak Date`, ")")
        ) %>%
        select(-Daily_Total, -Cumulative_Total)
    }
    
    # Combine the filtered data
    hpai_data_poultry_cum_filtered <- bind_rows(state_data_filtered, total_data_filtered)
  }
  
  p <- ggplot(hpai_data_poultry_cum_filtered, aes(
    x = `Outbreak Date`, 
    y = Cumulative_FlockSize / 1e6, 
    color = State, 
    group = State, 
    text = tooltip_text 
  )) +
    geom_line(size=0.8, alpha=0.7) +
    labs(x = "Outbreak Date", 
         y = "Cumulative reported birds",
         color = "State") +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      labels = scales::comma_format(scale = 1, suffix = "M"),
      limits = c(0, max(hpai_data_poultry_cum_filtered$Cumulative_FlockSize, na.rm = TRUE) / 1e6),  
      breaks = pretty(c(0, max(hpai_data_poultry_cum_filtered$Cumulative_FlockSize, na.rm = TRUE) / 1e6), n = 10)  
    ) +
    scale_color_manual(values = generate_color_palette(unique(hpai_data_poultry_cum_filtered$State))) +
    theme_function() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey90", size = 0.1),
      panel.grid.minor = element_blank()
    ) 
  
  ggplotly(p, tooltip = "text") %>% 
    layout(
      autosize = TRUE,
      font = list(size = 18, color = "black"),
      xaxis = list(tickfont = list(size = 18, family = "Arial", color = "black")),
      yaxis = list(
        tickfont = list(size = 18, family = "Arial", color = "black"),
        title = list(
          text = "Cumulative reported birds",
          font = list(size = 20, family = "Arial", color = "black", weight = "bold"),
          standoff = 20)
      ),
      title = list(font = list(size = 20, color = "black")),
      legend = list(
        font = list(size = 15, family = "Arial", color = "black"), 
        title = list(
          text = "State<br>", 
          font = list(size = 18, family = "Arial", color = "black", weight = "bold")
        )
      )
    ) 
})

#Poultry (farms)
output$plotPoultryCum_farms <- renderPlotly({
  req(input$species == "Poultry (farms)", input$date)
  
  # Make the plot reactive to timeline changes
  timeline_range <- input$timeline_range
  
  hpai_data_poultry_cum <- hpai_data_poultry %>%
    mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format="%m-%d-%Y")) %>%
    group_by(State, `Outbreak Date`) %>%
    summarise(
      Cases = n(),
      County = paste(unique(County), collapse=", "),
      FlockType = paste(unique(FlockType), collapse=", "),
      .groups = "drop"
    ) %>%
    arrange(`Outbreak Date`) %>%
    group_by(State) %>%
    mutate(Cumulative_Cases = cumsum(Cases))
  
  hpai_data_poultry_cum <- hpai_data_poultry_cum %>%
    group_by(State) %>%
    complete(`Outbreak Date` = seq(min(`Outbreak Date`), max(`Outbreak Date`), by="day"), 
             fill = list(Cases = 0, Cumulative_Cases = NA)) %>%
    fill(Cumulative_Cases, .direction = "down") %>% 
    ungroup()
  
  state_starts <- hpai_data_poultry_cum %>%
    group_by(State) %>%
    summarise(
      `Outbreak Date` = min(`Outbreak Date`) - 1,
      Cumulative_Cases = 0,
      .groups = "drop"
    )
  
  hpai_data_poultry_cum <- bind_rows(hpai_data_poultry_cum, state_starts) %>%
    arrange(State, `Outbreak Date`) %>%
    group_by(State) %>%
    fill(Cumulative_Cases, .direction = "down") %>% 
    ungroup() %>%
    mutate(tooltip_text = paste0("Cumulative farms: ", format(Cumulative_Cases, big.mark=","), 
                                 " (", State, ", ", `Outbreak Date`, ")",
                                 ifelse(!is.na(Cases) & Cases > 0, 
                                        paste0("\nFlock Type: ", FlockType, "\nCounty: ", County), 
                                        "")))  
  
  hpai_data_poultry_cum <- hpai_data_poultry_cum %>%
    group_by(State) %>%
    arrange(`Outbreak Date`) %>%
    mutate(Case_Increase = Cumulative_Cases > lag(Cumulative_Cases, default = 0)) %>% 
    ungroup()
  
  # Add total cases line
  total_cases <- hpai_data_poultry_cum %>%
    group_by(`Outbreak Date`) %>%
    summarise(
      Cumulative_Cases = sum(Cumulative_Cases, na.rm = TRUE),
      Cases = sum(Cases, na.rm = TRUE),
      State = "Total",
      tooltip_text = paste0("Total farms: ", format(sum(Cumulative_Cases, na.rm = TRUE), big.mark=","), " (", `Outbreak Date`, ")"),
      Case_Increase = sum(Cases, na.rm = TRUE) > 0,
      .groups = "drop"
    )
  
  hpai_data_poultry_cum <- bind_rows(hpai_data_poultry_cum, total_cases)
  
  # Get all states (excluding Total)
  all_states <- hpai_data_poultry_cum %>%
    filter(State != "Total") %>%
    pull(State) %>%
    unique()
  
  # Filter data to show all states + Total
  hpai_data_poultry_cum_filtered <- hpai_data_poultry_cum %>%
    filter(State %in% c(all_states, "Total"))
  
  # Apply timeline filtering if timeline range is selected
  if (!is.null(timeline_range) && length(timeline_range) == 2) {
    # For cumulative data, we need to preserve the cumulative nature
    # Filter individual state data normally
    state_data_filtered <- hpai_data_poultry_cum_filtered %>%
      filter(State != "Total") %>%
      filter(`Outbreak Date` >= timeline_range[1] & `Outbreak Date` <= timeline_range[2])
    
    # For Total line, we need to recalculate cumulative sum within the filtered range
    # but ensure it doesn't go below the previous cumulative values
    total_data_filtered <- hpai_data_poultry_cum_filtered %>%
      filter(State == "Total") %>%
      filter(`Outbreak Date` >= timeline_range[1] & `Outbreak Date` <= timeline_range[2])
    
    # Recalculate the Total line to ensure it's truly cumulative within the filtered range
    if (nrow(total_data_filtered) > 0) {
      # Get the cumulative sum of all states within the filtered range
      state_cumulative_in_range <- state_data_filtered %>%
        group_by(`Outbreak Date`) %>%
        summarise(
          Daily_Total = sum(Cumulative_Cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(`Outbreak Date`) %>%
        mutate(Cumulative_Total = cummax(Daily_Total))  # Use cummax to ensure it never decreases
      
      # Update the total data with the corrected cumulative values
      total_data_filtered <- total_data_filtered %>%
        select(-Cumulative_Cases) %>%
        left_join(state_cumulative_in_range, by = "Outbreak Date") %>%
        mutate(
          Cumulative_Cases = Cumulative_Total,
          tooltip_text = paste0("Total farms: ", format(Cumulative_Cases, big.mark=","), " (", `Outbreak Date`, ")")
        ) %>%
        select(-Daily_Total, -Cumulative_Total)
    }
    
    # Combine the filtered data
    hpai_data_poultry_cum_filtered <- bind_rows(state_data_filtered, total_data_filtered)
  }
  
  p <- ggplot(hpai_data_poultry_cum_filtered, aes(
    x = `Outbreak Date`, 
    y = Cumulative_Cases, 
    color = State, 
    group = State, 
    text = tooltip_text 
  )) +
    geom_step(size=0.8, alpha=0.7) +
    labs(x = "Outbreak Date", 
         y = "Cumulative reported cases",
         color = "State") +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, max(hpai_data_poultry_cum_filtered$Cumulative_Cases, na.rm = TRUE)),  
      breaks = pretty(c(0, max(hpai_data_poultry_cum_filtered$Cumulative_Cases, na.rm = TRUE)), n = 10)  
    ) +
    scale_color_manual(values = generate_color_palette(unique(hpai_data_poultry_cum_filtered$State))) +
    theme_function() %>%
    apply_plot_styling() 
  
  ggplotly(p, tooltip = "text") %>% 
    layout(
      autosize = TRUE,
      font = list(size = 18, color = "black"),
      xaxis = list(tickfont = list(size = 18, family = "Arial", color = "black")),
      yaxis = list(
        tickfont = list(size = 18, family = "Arial", color = "black"),
        title = list(
          text = "Cumulative reported cases",
          font = list(size = 20, family = "Arial", color = "black", weight = "bold"),
          standoff = 20)
      ),
      title = list(font = list(size = 20, color = "black")),
      legend = list(
        font = list(size = 15, family = "Arial", color = "black"), 
        title = list(
          text = "State<br>", 
          font = list(size = 18, family = "Arial", color = "black", weight = "bold")
        )
      )
    ) 
})

#Dairy cattle
output$plotCattleCum <- renderPlotly({
  req(input$species == "Dairy cattle (farms)", input$date)
  
  # Make the plot reactive to timeline changes
  timeline_range <- input$timeline_range
  
  # Debug: Check if cattle data exists and has the right structure
  if (is.null(hpai_data_cattle_cum) || nrow(hpai_data_cattle_cum) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No cattle data available"))
  }
  
  # Debug: Check data structure
  print("Cattle data structure:")
  print(str(hpai_data_cattle_cum))
  print("Cattle data sample:")
  print(head(hpai_data_cattle_cum))
  print("Unique states in cattle data:")
  print(unique(hpai_data_cattle_cum$State))
  
  hpai_data_cattle_cum <- hpai_data_cattle_cum %>%
    group_by(State, `Date`) %>%
    summarise(
      Cases = n(),
      Production = paste(unique(Production), collapse=", "),
      .groups = "drop"
    ) %>%
    arrange(`Date`) %>%
    group_by(State) %>%
    mutate(Cumulative_Cases = cumsum(Cases))
  
  hpai_data_cattle_cum <- hpai_data_cattle_cum %>%
    group_by(State) %>%
    complete(Date = seq(min(Date), max(Date), by="day"), 
             fill = list(Cases = 0, Cumulative_Cases = NA)) %>%
    fill(Cumulative_Cases, .direction = "down") %>% 
    ungroup()
  
  state_starts <- hpai_data_cattle_cum %>%
    group_by(State) %>%
    summarise(
      Date = min(Date) - 1,
      Cumulative_Cases = 0,
      .groups = "drop"
    )
  
  hpai_data_cattle_cum <- bind_rows(hpai_data_cattle_cum, state_starts) %>%
    arrange(State, Date) %>%
    group_by(State) %>%
    fill(Cumulative_Cases, .direction = "down") %>% 
    ungroup() %>%
    mutate(tooltip_text = paste0("Cumulative farms: ", format(Cumulative_Cases, big.mark=","), 
                                 " (", State, ", ", Date, ")",
                                 ifelse(!is.na(Cases) & Cases > 0, 
                                        paste0("\nProduction: ", Production), 
                                        "")))  
  
  hpai_data_cattle_cum <- hpai_data_cattle_cum %>%
    group_by(State) %>%
    arrange(Date) %>%
    mutate(Case_Increase = Cumulative_Cases > lag(Cumulative_Cases, default = 0)) %>% 
    ungroup()
  
  # Add total cases line
  total_cases <- hpai_data_cattle_cum %>%
    group_by(Date) %>%
    summarise(
      Cumulative_Cases = sum(Cumulative_Cases, na.rm = TRUE),
      Cases = sum(Cases, na.rm = TRUE),
      State = "Total",
      tooltip_text = paste0("Total farms: ", format(sum(Cumulative_Cases, na.rm = TRUE), big.mark=","), " (", Date, ")"),
      Case_Increase = sum(Cases, na.rm = TRUE) > 0,
      .groups = "drop"
    )
  
  hpai_data_cattle_cum <- bind_rows(hpai_data_cattle_cum, total_cases)
  
  # Get all states (excluding Total)
  all_states <- hpai_data_cattle_cum %>%
    filter(State != "Total") %>%
    pull(State) %>%
    unique()
  
  # Filter data to show all states + Total
  hpai_data_cattle_cum_filtered <- hpai_data_cattle_cum %>%
    filter(State %in% c(all_states, "Total"))
  
    # Apply timeline filtering if timeline range is selected
  if (!is.null(timeline_range) && length(timeline_range) == 2) {
    print(paste("Timeline range:", timeline_range[1], "to", timeline_range[2]))
    
    # For cumulative data, we need to preserve the cumulative nature
    # Filter individual state data normally
    state_data_filtered <- hpai_data_cattle_cum_filtered %>%
      filter(State != "Total") %>%
      filter(Date >= timeline_range[1] & Date <= timeline_range[2])
    
    print(paste("State data filtered rows:", nrow(state_data_filtered)))
    
    # For Total line, we need to recalculate cumulative sum within the filtered range
    # but ensure it doesn't go below the previous cumulative values
    total_data_filtered <- hpai_data_cattle_cum_filtered %>%
      filter(State == "Total") %>%
      filter(Date >= timeline_range[1] & Date <= timeline_range[2])
    
    print(paste("Total data filtered rows:", nrow(total_data_filtered)))
    
    # Recalculate the Total line to ensure it's truly cumulative within the filtered range
    if (nrow(total_data_filtered) > 0) {
      # Get the cumulative sum of all states within the filtered range
      state_cumulative_in_range <- state_data_filtered %>%
        group_by(Date) %>%
        summarise(
          Daily_Total = sum(Cumulative_Cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(Date) %>%
        mutate(Cumulative_Total = cummax(Daily_Total))  # Use cummax to ensure it never decreases
      
      # Update the total data with the corrected cumulative values
      total_data_filtered <- total_data_filtered %>%
        select(-Cumulative_Cases) %>%
        left_join(state_cumulative_in_range, by = "Date") %>%
        mutate(
          Cumulative_Cases = Cumulative_Total,
          tooltip_text = paste0("Total farms: ", format(Cumulative_Cases, big.mark=","), " (", Date, ")")
        ) %>%
        select(-Daily_Total, -Cumulative_Total)
    }
    
    # Combine the filtered data
    hpai_data_cattle_cum_filtered <- bind_rows(state_data_filtered, total_data_filtered)
    print(paste("Final filtered data rows:", nrow(hpai_data_cattle_cum_filtered)))
  } else {
    print("No timeline filtering applied")
  }
  
  # Safety check: ensure we have data to plot
  if (is.null(hpai_data_cattle_cum_filtered) || nrow(hpai_data_cattle_cum_filtered) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No cattle data available for the selected time range"))
  }
  
  # Debug: Check filtered data
  print("Filtered cattle data structure:")
  print(str(hpai_data_cattle_cum_filtered))
  print("Unique states in filtered cattle data:")
  print(unique(hpai_data_cattle_cum_filtered$State))
  
  # Safety check: ensure we have valid data for plotting
  if (all(is.na(hpai_data_cattle_cum_filtered$Cumulative_Cases)) || 
      all(hpai_data_cattle_cum_filtered$Cumulative_Cases == 0)) {
    return(plotly_empty() %>% 
             layout(title = "No cumulative cases data available for cattle"))
  }
  
  # Safety check: ensure we have valid dates
  if (all(is.na(hpai_data_cattle_cum_filtered$Date))) {
    return(plotly_empty() %>% 
             layout(title = "No valid dates available for cattle data"))
  }
  
  p <- ggplot(hpai_data_cattle_cum_filtered, aes(
    x = Date, 
    y = Cumulative_Cases, 
    color = State, 
    group = State, 
    text = tooltip_text 
  )) +
    geom_step(size=0.8, alpha=0.7) +
    labs(x = "Date", 
         y = "Cumulative reported cases",
         color = "State") +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, max(hpai_data_cattle_cum_filtered$Cumulative_Cases, na.rm = TRUE)),  
      breaks = pretty(c(0, max(hpai_data_cattle_cum_filtered$Cumulative_Cases, na.rm = TRUE)), n = 10)  
    ) +
    scale_color_manual(values = generate_color_palette(unique(hpai_data_cattle_cum_filtered$State))) +
    theme_function() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
      axis.text.y = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.title.y = element_text(color = "black"),
      panel.grid.major = element_line(color = "grey90", size = 0.1),
      panel.grid.minor = element_blank()
    ) 
  
  ggplotly(p, tooltip = "text") %>% 
    layout(
      autosize = TRUE,
      font = list(size = 18, color = "black"),
      xaxis = list(tickfont = list(size = 18, family = "Arial", color = "black")),
      yaxis = list(
        tickfont = list(size = 18, family = "Arial", color = "black"),
        title = list(
          text = "Cumulative reported cases",
          font = list(size = 20, family = "Arial", color = "black", weight = "bold"),
          standoff = 20)
      ),
      title = list(font = list(size = 20, color = "black")),
      legend = list(
        font = list(size = 15, family = "Arial", color = "black"), 
        title = list(
          text = "State<br>", 
          font = list(size = 18, family = "Arial", color = "black", weight = "bold")
        )
      )
    ) 
})

hpai_data_cats_cum <- hpai_data_mammals_cats_cum %>%
  mutate(`Date Detected` = mdy(`Date Detected`)) %>%
  group_by(State, `Date Detected`) %>%
  summarise(
    Cases = n(),
    Species_Info = paste(unique(Species), collapse = ", "), 
    County_Info = paste(unique(County), collapse = ", "),   
    .groups = "drop"
  ) %>%
  arrange(`Date Detected`) %>%
  group_by(State) %>%
  mutate(Cumulative_Cases = cumsum(Cases))

# Patch for cats
date_col <- "Date Detected"
hpai_data_cats_cum <- hpai_data_cats_cum %>%
  group_by(State) %>%
  filter(!all(is.na(.data[[date_col]]))) %>%
  complete(`Date Detected` = seq(min(.data[[date_col]], na.rm = TRUE), max(.data[[date_col]], na.rm = TRUE), by = "day"),
           fill = list(Cases = 0, Cumulative_Cases = NA, Species_Info = "", County_Info = "")) %>%
  fill(Cumulative_Cases, Species_Info, County_Info, .direction = "down") %>%
  ungroup()

state_starts <- hpai_data_cats_cum %>%
  group_by(State) %>%
  summarise(
    `Date Detected` = min(`Date Detected`) - 1,
    Cumulative_Cases = 0,
    Species_Info = "",
    County_Info = "",
    .groups = "drop"
  )

hpai_data_cats_cum <- bind_rows(hpai_data_cats_cum, state_starts) %>%
  arrange(State, `Date Detected`) %>%
  group_by(State) %>%
  fill(Cumulative_Cases, Species_Info, County_Info, .direction = "down") %>%
  ungroup() %>%
  mutate(tooltip_text = paste0("Cumulative cases: ", Cumulative_Cases, 
                               " (", State, ", ", `Date Detected`, ")",
                               ifelse(!is.na(Cases) & Cases > 0, 
                                      paste0("\nSpecies: ", Species_Info, "\nCounty: ", County_Info), 
                                      ""))) 

hpai_data_cats_cum <- hpai_data_cats_cum %>%
  group_by(State) %>%
  arrange(`Date Detected`) %>%
  mutate(Case_Increase = Cumulative_Cases > lag(Cumulative_Cases, default = 0)) %>%
  ungroup()

# Add total cases line
total_cases <- hpai_data_cats_cum %>%
  group_by(`Date Detected`) %>%
  summarise(
    Cumulative_Cases = sum(Cumulative_Cases, na.rm = TRUE),
    Cases = sum(Cases, na.rm = TRUE),
    State = "Total",
    tooltip_text = paste0("Total cases: ", sum(Cumulative_Cases, na.rm = TRUE), " (", `Date Detected`, ")"),
    Case_Increase = sum(Cases, na.rm = TRUE) > 0,
    .groups = "drop"
  )

hpai_data_cats_cum <- bind_rows(hpai_data_cats_cum, total_cases)

# Get all states (excluding Total)
all_states <- hpai_data_cats_cum %>%
  filter(State != "Total") %>%
  pull(State) %>%
  unique()

# Filter data to show all states + Total
hpai_data_cats_cum_filtered <- hpai_data_cats_cum %>%
  filter(State %in% c(all_states, "Total"))

output$plotCatsCum <- renderPlotly({
  req(input$species == "Domestic cats", input$date)
  
  # Make the plot reactive to timeline changes
  timeline_range <- input$timeline_range
  
  # Apply timeline filtering if timeline range is selected
  if (!is.null(timeline_range) && length(timeline_range) == 2) {
    # For cumulative data, we need to preserve the cumulative nature
    # Filter individual state data normally
    state_data_filtered <- hpai_data_cats_cum_filtered %>%
      filter(State != "Total") %>%
      filter(`Date Detected` >= timeline_range[1] & `Date Detected` <= timeline_range[2])
    
    # For Total line, we need to recalculate cumulative sum within the filtered range
    # but ensure it doesn't go below the previous cumulative values
    total_data_filtered <- hpai_data_cats_cum_filtered %>%
      filter(State == "Total") %>%
      filter(`Date Detected` >= timeline_range[1] & `Date Detected` <= timeline_range[2])
    
    # Recalculate the Total line to ensure it's truly cumulative within the filtered range
    if (nrow(total_data_filtered) > 0) {
      # Get the cumulative sum of all states within the filtered range
      state_cumulative_in_range <- state_data_filtered %>%
        group_by(`Date Detected`) %>%
        summarise(
          Daily_Total = sum(Cumulative_Cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(`Date Detected`) %>%
        mutate(Cumulative_Total = cummax(Daily_Total))  # Use cummax to ensure it never decreases
      
      # Update the total data with the corrected cumulative values
      total_data_filtered <- total_data_filtered %>%
        select(-Cumulative_Cases) %>%
        left_join(state_cumulative_in_range, by = "Date Detected") %>%
        mutate(
          Cumulative_Cases = Cumulative_Total,
          tooltip_text = paste0("Total cases: ", Cumulative_Cases, " (", `Date Detected`, ")")
        ) %>%
        select(-Daily_Total, -Cumulative_Total)
    }
    
    # Combine the filtered data
    hpai_data_cats_cum_filtered <- bind_rows(state_data_filtered, total_data_filtered)
  }
  
  p <- ggplot(hpai_data_cats_cum_filtered, aes(
    x = `Date Detected`,
    y = Cumulative_Cases,
    color = State,
    group = State,
    text = tooltip_text
  )) +
    geom_step(size=0.8, alpha=0.7) +
    labs(x = "Date detected",
         y = "Cumulative reported cases",
         color = "State") +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, max(hpai_data_cats_cum_filtered$Cumulative_Cases, na.rm = TRUE)),
      breaks = pretty(c(0, max(hpai_data_cats_cum_filtered$Cumulative_Cases, na.rm = TRUE)), n = 10)
    ) +
    scale_color_manual(values = generate_color_palette(unique(hpai_data_cats_cum_filtered$State))) +
    theme_function() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.1),
      panel.grid.minor = element_blank()
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      autosize = TRUE,
      font = list(size = 18),
      xaxis = list(tickfont = list(size = 18, family = "Arial", color = "black")),
      yaxis = list(
        tickfont = list(size = 18, family = "Arial", color = "black"),
        title = list(
          text = "Cumulative reported cases",
          font = list(size = 20, family = "Arial", color = "black", weight = "bold"),
          standoff = 20)
      ),
      title = list(font = list(size = 20)),
      legend = list(
        font = list(size = 15, family = "Arial", color = "black"),
        title = list(
          text = "State<br>",
          font = list(size = 18, family = "Arial", color = "black", weight = "bold")
        )
      )
    ) 
})


#Wild birds
state_divisions <- list(
  "New England" = c("CT", "ME", "MA", "NH", "RI", "VT"),
  "Middle Atlantic" = c("NJ", "NY", "PA"),
  "East North Central" = c("IL", "IN", "MI", "OH", "WI"),
  "West North Central" = c("IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  "South Atlantic" = c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV"),
  "East South Central" = c("AL", "KY", "MS", "TN"),
  "West South Central" = c("AR", "LA", "OK", "TX"),
  "Mountain" = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY"),
  "Pacific" = c("AK", "CA", "HI", "OR", "WA")
)

#cnvert state names to title case (fix lowercase inconsistencies)
hpai_data_wildbirds_cum <- hpai_data_wildbirds_cum %>%
  mutate(State = str_to_title(State)) %>%
  mutate(
    State_abbr = case_when(
      State %in% c(
        "Connecticut",
        "Maine",
        "Massachusetts",
        "New Hampshire",
        "Rhode Island",
        "Vermont"
      ) ~ "New England",
      State %in% c("New Jersey", "New York", "Pennsylvania") ~ "Middle Atlantic",
      State %in% c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin") ~ "East North Central",
      State %in% c(
        "Iowa",
        "Kansas",
        "Minnesota",
        "Missouri",
        "Nebraska",
        "North Dakota",
        "South Dakota"
      ) ~ "West North Central",
      State %in% c(
        "Delaware",
        "District of Columbia",
        "Florida",
        "Georgia",
        "Maryland",
        "North Carolina",
        "South Carolina",
        "Virginia",
        "West Virginia"
      ) ~ "South Atlantic",
      State %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee") ~ "East South Central",
      State %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas") ~ "West South Central",
      State %in% c(
        "Arizona",
        "Colorado",
        "Idaho",
        "Montana",
        "Nevada",
        "New Mexico",
        "Utah",
        "Wyoming"
      ) ~ "Mountain",
      State %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington") ~ "Pacific",
      State %in% c("District of Columbia") ~ "D.C.",
      TRUE ~ "D.C."
    )
  )

print(hpai_data_wildbirds_cum[hpai_data_wildbirds_cum$State_abbr == "D.C", ])

#aggregates data by division
hpai_data_wildbirds_cum <- hpai_data_wildbirds_cum %>%
  mutate(`Date Detected` = mdy(`Date Detected`)) %>%
  group_by(State, `Date Detected`) %>%
  summarise(
    Cases = n(),
    Species_Info = paste(unique(Species), collapse = ", "),  
    County_Info = paste(unique(County), collapse = ", "),    
    .groups = "drop"
  ) %>%
  arrange(`Date Detected`) %>%
  group_by(State) %>%
  mutate(Cumulative_Cases = cumsum(Cases))

# Patch for wild birds
date_col <- "Date Detected"
hpai_data_wildbirds_cum <- hpai_data_wildbirds_cum %>%
  group_by(State) %>%
  filter(!all(is.na(.data[[date_col]]))) %>%
  complete(`Date Detected` = seq(min(.data[[date_col]], na.rm = TRUE), max(.data[[date_col]], na.rm = TRUE), by = "day"),
           fill = list(Cases = 0, Cumulative_Cases = NA, Species_Info = "", County_Info = "")) %>%
  fill(Cumulative_Cases, Species_Info, County_Info, .direction = "down") %>%
  ungroup()

state_starts <- hpai_data_wildbirds_cum %>%
  group_by(State) %>%
  summarise(
    `Date Detected` = min(`Date Detected`) - 1,
    Cumulative_Cases = 0,
    Species_Info = "",
    County_Info = "",
    .groups = "drop"
  )

hpai_data_wildbirds_cum <- bind_rows(hpai_data_wildbirds_cum, state_starts) %>%
  arrange(State, `Date Detected`) %>%
  group_by(State) %>%
  fill(Cumulative_Cases, Species_Info, County_Info, .direction = "down") %>%
  ungroup() %>%
  mutate(tooltip_text = paste0("Cumulative cases: ", Cumulative_Cases, 
                               " (", State, ", ", `Date Detected`, ")",
                               ifelse(!is.na(Cases) & Cases > 0, 
                                      paste0("\nSpecies: ", Species_Info, "\nCounty: ", County_Info), 
                                      ""))) 

hpai_data_wildbirds_cum <- hpai_data_wildbirds_cum %>%
  group_by(State) %>%
  arrange(`Date Detected`) %>%
  mutate(Case_Increase = Cumulative_Cases > lag(Cumulative_Cases, default = 0)) %>%
  ungroup()

# Add total cases line
total_cases <- hpai_data_wildbirds_cum %>%
  group_by(`Date Detected`) %>%
  summarise(
    Cumulative_Cases = sum(Cumulative_Cases, na.rm = TRUE),
    Cases = sum(Cases, na.rm = TRUE),
    State = "Total",
    tooltip_text = paste0("Total cases: ", sum(Cumulative_Cases, na.rm = TRUE), " (", `Date Detected`, ")"),
    Case_Increase = sum(Cases, na.rm = TRUE) > 0,
    .groups = "drop"
  )

hpai_data_wildbirds_cum <- bind_rows(hpai_data_wildbirds_cum, total_cases)

# Get all states (excluding Total)
all_states <- hpai_data_wildbirds_cum %>%
  filter(State != "Total") %>%
  pull(State) %>%
  unique()

# Filter data to show all states + Total
hpai_data_wildbirds_cum_filtered <- hpai_data_wildbirds_cum %>%
  filter(State %in% c(all_states, "Total"))

output$plotWildbirdsCum <- renderPlotly({
  req(input$species == "Wild birds", input$date)
  
  # Make the plot reactive to timeline changes
  timeline_range <- input$timeline_range
  
  # Apply timeline filtering if timeline range is selected
  if (!is.null(timeline_range) && length(timeline_range) == 2) {
    # For cumulative data, we need to preserve the cumulative nature
    # Filter individual state data normally
    state_data_filtered <- hpai_data_wildbirds_cum_filtered %>%
      filter(State != "Total") %>%
      filter(`Date Detected` >= timeline_range[1] & `Date Detected` <= timeline_range[2])
    
    # For Total line, we need to recalculate cumulative sum within the filtered range
    # but ensure it doesn't go below the previous cumulative values
    total_data_filtered <- hpai_data_wildbirds_cum_filtered %>%
      filter(State == "Total") %>%
      filter(`Date Detected` >= timeline_range[1] & `Date Detected` <= timeline_range[2])
    
    # Recalculate the Total line to ensure it's truly cumulative within the filtered range
    if (nrow(total_data_filtered) > 0) {
      # Get the cumulative sum of all states within the filtered range
      state_cumulative_in_range <- state_data_filtered %>%
        group_by(`Date Detected`) %>%
        summarise(
          Daily_Total = sum(Cumulative_Cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(`Date Detected`) %>%
        mutate(Cumulative_Total = cummax(Daily_Total))  # Use cummax to ensure it never decreases
      
      # Update the total data with the corrected cumulative values
      total_data_filtered <- total_data_filtered %>%
        select(-Cumulative_Cases) %>%
        left_join(state_cumulative_in_range, by = "Date Detected") %>%
        mutate(
          Cumulative_Cases = Cumulative_Total,
          tooltip_text = paste0("Total cases: ", Cumulative_Cases, " (", `Date Detected`, ")")
        ) %>%
        select(-Daily_Total, -Cumulative_Total)
    }
    
    # Combine the filtered data
    hpai_data_wildbirds_cum_filtered <- bind_rows(state_data_filtered, total_data_filtered)
  }
  
  p <- ggplot(hpai_data_wildbirds_cum_filtered, aes(
    x = `Date Detected`,
    y = Cumulative_Cases,
    color = State,
    group = State,
    text = tooltip_text
  )) +
    geom_step(size=0.8, alpha=0.7) +
    labs(x = "Date detected",
         y = "Cumulative reported cases",
         color = "State") +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, max(hpai_data_wildbirds_cum_filtered$Cumulative_Cases, na.rm = TRUE)),
      breaks = pretty(c(0, max(hpai_data_wildbirds_cum_filtered$Cumulative_Cases, na.rm = TRUE)), n = 10)
    ) +
    scale_color_manual(values = generate_color_palette(unique(hpai_data_wildbirds_cum_filtered$State))) +
    theme_function() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.1),
      panel.grid.minor = element_blank()
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      autosize = TRUE,
      font = list(size = 18),
      xaxis = list(tickfont = list(size = 18, family = "Arial", color = "black")),
      yaxis = list(
        tickfont = list(size = 18, family = "Arial", color = "black"),
        title = list(
          text = "Cumulative reported cases",
          font = list(size = 20, family = "Arial", color = "black", weight = "bold"),
          standoff = 20)
      ),
      title = list(font = list(size = 20)),
      legend = list(
        font = list(size = 15, family = "Arial", color = "black"),
        title = list(
          text = "State<br>",
          font = list(size = 18, family = "Arial", color = "black", weight = "bold")
        )
      )
    ) 
})


#Wild mammals
hpai_data_mammals_cum <- hpai_data_mammals_cum %>%
  group_by(State, `Date Detected`) %>%
  summarise(
    Cases = n(),
    Species_Info = paste(unique(Species), collapse = ", "),  # Combine unique species
    County_Info = paste(unique(County), collapse = ", "),    # Combine unique counties
    .groups = "drop"
  ) %>%
  arrange(`Date Detected`) %>%
  group_by(State) %>%
  mutate(Cumulative_Cases = cumsum(Cases))

# Patch for wild mammals
date_col <- "Date Detected"
hpai_data_mammals_cum <- hpai_data_mammals_cum %>%
  group_by(State) %>%
  filter(!all(is.na(.data[[date_col]]))) %>%
  complete(`Date Detected` = seq(min(.data[[date_col]], na.rm = TRUE), max(.data[[date_col]], na.rm = TRUE), by = "day"),
           fill = list(Cases = 0, Cumulative_Cases = NA, Species_Info = "", County_Info = "")) %>%
  fill(Cumulative_Cases, Species_Info, County_Info, .direction = "down") %>%
  ungroup()

state_starts <- hpai_data_mammals_cum %>%
  group_by(State) %>%
  summarise(
    `Date Detected` = min(`Date Detected`) - 1,
    Cumulative_Cases = 0,
    Species_Info = "",
    County_Info = "",
    .groups = "drop"
  )

hpai_data_mammals_cum <- bind_rows(hpai_data_mammals_cum, state_starts) %>%
  arrange(State, `Date Detected`) %>%
  group_by(State) %>%
  fill(Cumulative_Cases, Species_Info, County_Info, .direction = "down") %>%
  ungroup() %>%
  mutate(tooltip_text = paste0("Cumulative cases: ", Cumulative_Cases, 
                               " (", State, ", ", `Date Detected`, ")",
                               ifelse(!is.na(Cases) & Cases > 0, 
                                      paste0("\nSpecies: ", Species_Info, "\nCounty: ", County_Info), 
                                      ""))) 

hpai_data_mammals_cum <- hpai_data_mammals_cum %>%
  group_by(State) %>%
  arrange(`Date Detected`) %>%
  mutate(Case_Increase = Cumulative_Cases > lag(Cumulative_Cases, default = 0)) %>%
  ungroup()

# Add total cases line
total_cases <- hpai_data_mammals_cum %>%
  group_by(`Date Detected`) %>%
  summarise(
    Cumulative_Cases = sum(Cumulative_Cases, na.rm = TRUE),
    Cases = sum(Cases, na.rm = TRUE),
    State = "Total",
    tooltip_text = paste0("Total cases: ", sum(Cumulative_Cases, na.rm = TRUE), " (", `Date Detected`, ")"),
    Case_Increase = sum(Cases, na.rm = TRUE) > 0,
    .groups = "drop"
  )

hpai_data_mammals_cum <- bind_rows(hpai_data_mammals_cum, total_cases)

# Get all states (excluding Total)
all_states <- hpai_data_mammals_cum %>%
  filter(State != "Total") %>%
  pull(State) %>%
  unique()

# Filter data to show all states + Total
hpai_data_mammals_cum_filtered <- hpai_data_mammals_cum %>%
  filter(State %in% c(all_states, "Total"))

output$plotWildmammalsCum <- renderPlotly({
  req(input$species == "Wild mammals", input$date)
  
  # Make the plot reactive to timeline changes
  timeline_range <- input$timeline_range
  
  # Apply timeline filtering if timeline range is selected
  if (!is.null(timeline_range) && length(timeline_range) == 2) {
    # For cumulative data, we need to preserve the cumulative nature
    # Filter individual state data normally
    state_data_filtered <- hpai_data_mammals_cum_filtered %>%
      filter(State != "Total") %>%
      filter(`Date Detected` >= timeline_range[1] & `Date Detected` <= timeline_range[2])
    
    # For Total line, we need to recalculate cumulative sum within the filtered range
    # but ensure it doesn't go below the previous cumulative values
    total_data_filtered <- hpai_data_mammals_cum_filtered %>%
      filter(State == "Total") %>%
      filter(`Date Detected` >= timeline_range[1] & `Date Detected` <= timeline_range[2])
    
    # Recalculate the Total line to ensure it's truly cumulative within the filtered range
    if (nrow(total_data_filtered) > 0) {
      # Get the cumulative sum of all states within the filtered range
      state_cumulative_in_range <- state_data_filtered %>%
        group_by(`Date Detected`) %>%
        summarise(
          Daily_Total = sum(Cumulative_Cases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(`Date Detected`) %>%
        mutate(Cumulative_Total = cummax(Daily_Total))  # Use cummax to ensure it never decreases
      
      # Update the total data with the corrected cumulative values
      total_data_filtered <- total_data_filtered %>%
        select(-Cumulative_Cases) %>%
        left_join(state_cumulative_in_range, by = "Date Detected") %>%
        mutate(
          Cumulative_Cases = Cumulative_Total,
          tooltip_text = paste0("Total cases: ", Cumulative_Cases, " (", `Date Detected`, ")")
        ) %>%
        select(-Daily_Total, -Cumulative_Total)
    }
    
    # Combine the filtered data
    hpai_data_mammals_cum_filtered <- bind_rows(state_data_filtered, total_data_filtered)
  }
  
  p <- ggplot(hpai_data_mammals_cum_filtered, aes(
    x = `Date Detected`,
    y = Cumulative_Cases,
    color = State,
    group = State,
    text = tooltip_text
  )) +
    geom_step(size=0.8, alpha=0.7) +
    labs(x = "Date detected",
         y = "Cumulative reported cases",
         color = "State") +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"
    ) +
    scale_y_continuous(
      limits = c(0, max(hpai_data_mammals_cum_filtered$Cumulative_Cases, na.rm = TRUE)),
      breaks = pretty(c(0, max(hpai_data_mammals_cum_filtered$Cumulative_Cases, na.rm = TRUE)), n = 10)
    ) +
    scale_color_manual(values = generate_color_palette(unique(hpai_data_mammals_cum_filtered$State))) +
    theme_function() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      panel.grid.major = element_line(color = "grey90", size = 0.1),
      panel.grid.minor = element_blank()
    )
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      autosize = TRUE,
      font = list(size = 18),
      xaxis = list(tickfont = list(size = 18, family = "Arial", color = "black")),
      yaxis = list(
        tickfont = list(size = 18, family = "Arial", color = "black"),
        title = list(
          text = "Cumulative reported cases",
          font = list(size = 20, family = "Arial", color = "black", weight = "bold"),
          standoff = 20)
      ),
      title = list(font = list(size = 20)),
      legend = list(
        font = list(size = 15, family = "Arial", color = "black"),
        title = list(
          text = "State<br>",
          font = list(size = 18, family = "Arial", color = "black", weight = "bold")
        )
      )
    )
})

# Function to create sparkline charts for state trends
create_sparkline_chart <- function(data, state_name, color = "#ff6b6b") {
  if (is.null(data) || nrow(data) == 0) {
    return(sparkline(c(0), type = "line", width = 200, height = 50, 
                     lineColor = color, fillColor = "transparent"))
  }
  
  # Extract cases data for the state
  state_data <- data %>% 
    filter(State == state_name) %>% 
    arrange(Date_confirmed)
  
  if (nrow(state_data) == 0) {
    return(sparkline(c(0), type = "line", width = 200, height = 50, 
                     lineColor = color, fillColor = "transparent"))
  }
  
  # Create sparkline
  sparkline(
    state_data$Cases,
    type = "line",
    width = 200,
    height = 50,
    lineColor = color,
    fillColor = "rgba(255, 107, 107, 0.1)",
    lineWidth = 2,
    spotColor = color,
    minSpotColor = color,
    maxSpotColor = color,
    highlightSpotColor = color,
    highlightLineColor = color
  )
}

# New function to create case trend plots across states (inspired by COVID-19 trends)
output$daily_trends_plot <- renderPlotly({
  req(input$species)
  
  # Get the appropriate data based on species
  trend_data <- switch(input$species,
    "Humans" = {
      hpai_data_humans %>%
        mutate(Date_confirmed = mdy(Date_confirmed)) %>%
        group_by(State, Date_confirmed) %>%
        summarise(
          Cases = sum(NumberOfCases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (farms)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = n(), # Count outbreaks per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (birds)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = sum(FlockSize, na.rm = TRUE), # Sum birds affected per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Dairy cattle (farms)" = {
      hpai_data_cattle_cum %>%
        group_by(State, Date) %>%
        summarise(
          Cases = n(), # Count outbreaks per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = Date) %>%
        arrange(State, Date_confirmed)
    },
    "Domestic cats" = {
      hpai_data_mammals_cats_map %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(), # Count cases per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild birds" = {
      hpai_data_wildbirds_map %>%
        group_by(State, `Collection Date`) %>%
        summarise(
          Cases = n(), # Count detections per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Collection Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild mammals" = {
      hpai_data_mammals_mammals_map %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(), # Count cases per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    }
  )
  
  if (is.null(trend_data) || nrow(trend_data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No data available for this species"))
  }
  
  # Debug: print data summary
  print(paste("Species:", input$species))
  print(paste("Total data rows:", nrow(trend_data)))
  print(paste("Date range:", min(trend_data$Date_confirmed), "to", max(trend_data$Date_confirmed)))
  print(paste("Number of states:", length(unique(trend_data$State))))
  print("Sample states:")
  print(head(unique(trend_data$State), 10))
  
  # Apply timeline filtering if available
  if (!is.null(input$timeline_range) && length(input$timeline_range) == 2) {
    print(paste("Applying timeline filter:", input$timeline_range[1], "to", input$timeline_range[2]))
    trend_data <- trend_data %>%
      filter(Date_confirmed >= input$timeline_range[1] & Date_confirmed <= input$timeline_range[2])
    print(paste("After filtering, rows:", nrow(trend_data)))
  }
  
  # Calculate global/overall cases per date
  global_trend <- trend_data %>%
    group_by(Date_confirmed) %>%
    summarise(
      Cases = sum(Cases, na.rm = TRUE),
      State = "Total",
      .groups = "drop"
    )
  
  # Get top states by total cases for the grid
  state_totals <- trend_data %>%
    group_by(State) %>%
    summarise(
      Total_Cases = sum(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Cases)) %>%
    head(6) # Top 6 states for 3x3 grid
  
  # Safety check - ensure we have states to plot
  if (nrow(state_totals) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No state data available"))
  }
  
  # Filter data to include only top states + global
  plot_data <- trend_data %>%
    filter(State %in% state_totals$State) %>%
    bind_rows(global_trend)
  
  # Create the main global trend plot
  main_plot <- plot_ly() %>%
    add_trace(
      data = global_trend,
      x = ~Date_confirmed,
      y = ~Cases,
      type = 'scatter',
      mode = 'lines+markers',
      fill = 'tonexty',
      fillcolor = 'rgba(255, 182, 193, 0.3)',
      line = list(color = '#ff6b6b', width = 2),
      marker = list(size = 4),
      name = 'Total Cases',
      hovertemplate = paste(
        '<b>Date:</b> %{x}<br>',
        '<b>Cases:</b> %{y}<br>',
        '<extra></extra>'
      )
    ) %>%
    layout(
      title = list(
        text = paste(input$species, "Cases - Overall Trend"),
        font = list(size = 20, color = "black")
      ),
      xaxis = list(
        title = "Date",
        tickfont = list(size = 14),
        gridcolor = "lightgrey"
      ),
      yaxis = list(
        title = "Cases",
        tickfont = list(size = 14),
        gridcolor = "lightgrey"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      showlegend = FALSE,
      height = 400
    )
  
  # Create individual state plots in a 3x3 grid
  state_plots <- list()
  plot_counter <- 1
  
  # Debug: print state totals
  print(paste("Number of states to plot:", nrow(state_totals)))
  print("States to plot:")
  print(state_totals$State)
  
  for (i in 1:3) {
    for (j in 1:3) {
      if (plot_counter <= nrow(state_totals)) {
        current_state <- state_totals$State[plot_counter]
        print(paste("Processing state:", current_state, "at position:", plot_counter))
        
        state_data <- plot_data %>% filter(State == current_state)
        print(paste("State data rows:", nrow(state_data)))
        
        # Safety check for state data
        if (nrow(state_data) > 0) {
          tryCatch({
            state_plot <- plot_ly() %>%
              add_trace(
                data = state_data,
                x = ~Date_confirmed,
                y = ~Cases,
                type = 'scatter',
                mode = 'lines+markers',
                fill = 'tonexty',
                fillcolor = 'rgba(255, 182, 193, 0.3)',
                line = list(color = '#ff6b6b', width = 1.5),
                marker = list(size = 3),
                name = current_state,
                hovertemplate = paste(
                  '<b>State:</b>', current_state, '<br>',
                  '<b>Date:</b> %{x}<br>',
                  '<b>Cases:</b> %{y}<br>',
                  '<extra></extra>'
                )
              ) %>%
              layout(
                title = list(
                  text = current_state,
                  font = list(size = 16, color = "black")
                ),
                xaxis = list(
                  showticklabels = FALSE,
                  gridcolor = "lightgrey"
                ),
                yaxis = list(
                  gridcolor = "lightgrey"
                ),
                plot_bgcolor = "white",
                paper_bgcolor = "white",
                showlegend = FALSE,
                height = 200,
                margin = list(l = 40, r = 40, t = 40, b = 40)
              )
            
            state_plots[[plot_counter]] <- state_plot
            print(paste("Successfully created plot for:", current_state))
          }, error = function(e) {
            print(paste("Error creating plot for", current_state, ":", e$message))
            # Create a simple error plot instead
            state_plots[[plot_counter]] <<- plot_ly() %>%
              layout(
                title = list(text = paste("Error:", current_state)),
                annotations = list(
                  text = "Plot creation failed",
                  showarrow = FALSE,
                  x = 0.5,
                  y = 0.5
                )
              )
          })
        } else {
          print(paste("No data for state:", current_state))
          # Create empty plot for states with no data
          state_plots[[plot_counter]] <- plot_ly() %>%
            layout(
              title = list(text = current_state),
              annotations = list(
                text = "No data available",
                showarrow = FALSE,
                x = 0.5,
                y = 0.5
              )
            )
        }
        plot_counter <- plot_counter + 1
      }
    }
  }
  
  print(paste("Total plots created:", length(state_plots)))
  print("Plot indices:")
  print(names(state_plots))
  
  # Combine main plot and state plots using subplot
  if (length(state_plots) > 0) {
    tryCatch({
      print("Attempting to create subplot...")
      print(paste("Number of state plots:", length(state_plots)))
      
      # Ensure all plots are valid
      valid_plots <- state_plots[!sapply(state_plots, is.null)]
      print(paste("Valid plots:", length(valid_plots)))
      
      if (length(valid_plots) > 0) {
        # Create subplot with main plot on top and state plots in 3x3 grid below
        combined_plot <- subplot(
          main_plot,
          subplot(valid_plots, nrows = 3, ncols = 3, 
                  heights = c(0.4, 0.2, 0.2, 0.2), 
                  widths = c(0.33, 0.33, 0.34)),
          nrows = 2, heights = c(0.4, 0.6)
        ) %>%
          layout(
            title = list(
              text = paste(input$species, "Cases Trends by State"),
              font = list(size = 24, color = "black"),
              x = 0.5
            ),
            showlegend = FALSE,
            height = 800
          )
        
        print("Subplot created successfully")
        return(combined_plot)
      } else {
        print("No valid state plots, returning main plot only")
        return(main_plot)
      }
    }, error = function(e) {
      print(paste("Error creating subplot:", e$message))
      print("Returning main plot only as fallback")
      return(main_plot)
    })
  } else {
    print("No state plots available, returning main plot only")
    return(main_plot)
  }
})

# Simplified function to create state trend graphs
output$state_trends_grid <- renderPlotly({
  req(input$species)
  
  print(paste("Processing species for state trends:", input$species))
  
  # Get the appropriate data based on species
  trend_data <- tryCatch({
    switch(input$species,
    "Humans" = {
      hpai_data_humans %>%
        mutate(Date_confirmed = mdy(Date_confirmed)) %>%
        group_by(State, Date_confirmed) %>%
        summarise(
          Cases = sum(NumberOfCases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (farms)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = n(),
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (birds)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = sum(FlockSize, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Dairy cattle (farms)" = {
      hpai_data_cattle_cum %>%
        group_by(State, Date) %>%
        summarise(
          Cases = n(),
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = Date) %>%
        arrange(State, Date_confirmed)
    },
    "Domestic cats" = {
      hpai_data_mammals_cats_map %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(),
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild birds" = {
      hpai_data_wildbirds_map %>%
        group_by(State, `Collection Date`) %>%
        summarise(
          Cases = n(),
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Collection Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild mammals" = {
      hpai_data_mammals_mammals_map %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(),
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    }
  )
  }, error = function(e) {
    print(paste("Error getting trend data:", e$message))
    return(NULL)
  })
  
  if (is.null(trend_data) || nrow(trend_data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No data available for this species"))
  }
  
  # Apply timeline filtering if available
  if (!is.null(input$timeline_range) && length(input$timeline_range) == 2) {
    trend_data <- trend_data %>%
      filter(Date_confirmed >= input$timeline_range[1] & Date_confirmed <= input$timeline_range[2])
  }
  
  # Get top 6 states by total cases
  state_totals <- trend_data %>%
    group_by(State) %>%
    summarise(
      Total_Cases = sum(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Cases)) %>%
    head(6)
  
  if (nrow(state_totals) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No state data available"))
  }
  
  # Create a simple line plot with all top states
  plot_ly() %>%
    add_trace(
      data = trend_data %>% filter(State %in% state_totals$State),
      x = ~Date_confirmed,
      y = ~Cases,
      color = ~State,
      type = 'scatter',
      mode = 'lines',
      line = list(width = 2),
      hovertemplate = paste(
        '<b>State:</b> %{fullData.name}<br>',
        '<b>Date:</b> %{x}<br>',
        '<b>Cases:</b> %{y}<br>',
        '<extra></extra>'
      )
    ) %>%
    layout(
      title = list(
        text = paste(input$species, "Case Trends by State"),
        font = list(size = 20, color = "black", weight = "bold"),
        x = 0.5
      ),
      xaxis = list(
        title = "Date",
        tickfont = list(size = 14),
        gridcolor = "lightgrey",
        showgrid = TRUE
      ),
      yaxis = list(
        title = "Cases",
        tickfont = list(size = 14),
        gridcolor = "lightgrey",
        showgrid = TRUE,
        rangemode = "tozero"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      showlegend = TRUE,
      height = 600,
      legend = list(
        font = list(size = 12),
        bgcolor = "white",
        bordercolor = "lightgrey"
      )
    )
})

# Sparkline state trends output
output$sparkline_state_trends <- renderUI({
  req(input$species)
  
  # State name conversion function (same as in map_rendering.R)
  state_abbr_to_name <- function(state) {
    if(state %in% state.abb) {
      return(state.name[match(state, state.abb)])
    }
    return(state)
  }
  
  # Get the appropriate data based on species (same logic as simple_trends_plot)
  trend_data <- switch(input$species,
    "Humans" = {
      hpai_data_humans %>%
        mutate(Date_confirmed = mdy(Date_confirmed)) %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, Date_confirmed) %>%
        summarise(
          Cases = sum(NumberOfCases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (farms)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = n(), # Count outbreaks per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (birds)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = sum(FlockSize, na.rm = TRUE), # Sum birds affected per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Dairy cattle (farms)" = {
      hpai_data_cattle_cum %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, Date) %>%
        summarise(
          Cases = n(), # Count outbreaks per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = Date) %>%
        arrange(State, Date_confirmed)
    },
    "Domestic cats" = {
      hpai_data_mammals_cats_map %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(), # Count cases per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild birds" = {
      hpai_data_wildbirds_map %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Collection Date`) %>%
        summarise(
          Cases = n(), # Count detections per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Collection Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild mammals" = {
      hpai_data_mammals_mammals_map %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(), # Count cases per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    }
  )
  
  if (is.null(trend_data) || nrow(trend_data) == 0) {
    return(div(style = "text-align: center; padding: 50px;", 
               h4("No data available for this species")))
  }
  
  # Apply timeline filtering if available
  if (!is.null(input$timeline_range) && length(input$timeline_range) == 2) {
    trend_data <- trend_data %>%
      filter(Date_confirmed >= input$timeline_range[1] & Date_confirmed <= input$timeline_range[2])
  }
  
  # Get top states by total cases
  state_totals <- trend_data %>%
    group_by(State) %>%
    summarise(
      Total_Cases = sum(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Cases)) %>%
    head(6) # Top 6 states
  
  
  if (nrow(state_totals) == 0) {
    return(div(style = "text-align: center; padding: 50px;", 
               h4("No data available for the selected time range")))
  }
  
  # Create sparkline charts for each state
  sparkline_list <- list()
  
  for (i in 1:nrow(state_totals)) {
    state_name <- state_totals$State[i]
    total_cases <- state_totals$Total_Cases[i]
    
    
    # Create sparkline chart
    sparkline_chart <- create_sparkline_chart(trend_data, state_name)
    
    # Create state info div
    state_div <- div(
      style = "display: flex; align-items: center; margin-bottom: 15px; padding: 10px; border: 1px solid #e0e0e0; border-radius: 5px; background-color: #f9f9f9;",
      div(
        style = "flex: 1;",
        h5(style = "margin: 0; font-weight: bold; color: #333;", state_name),
        p(style = "margin: 5px 0 0 0; font-size: 14px; color: #666;", 
          paste("Total Cases:", format(total_cases, big.mark = ",")))
      ),
      div(
        style = "margin-left: 15px;",
        sparkline_chart
      )
    )
    
    sparkline_list[[i]] <- state_div
  }
  
  # Return the complete UI
  div(
    style = "padding: 20px;",
    h4(style = "text-align: center; margin-bottom: 30px; color: #333;", 
       paste(input$species, "Case Trends by State")),
    div(sparkline_list)
  )
})

# Simple fallback function for case trends (without subplot complexity)
output$simple_trends_plot <- renderPlotly({
  req(input$species)
  
  # State name conversion function (same as in map_rendering.R)
  state_abbr_to_name <- function(state) {
    if(state %in% state.abb) {
      return(state.name[match(state, state.abb)])
    }
    return(state)
  }
  
  # Get the appropriate data based on species
  trend_data <- switch(input$species,
    "Humans" = {
      hpai_data_humans %>%
        mutate(Date_confirmed = mdy(Date_confirmed)) %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, Date_confirmed) %>%
        summarise(
          Cases = sum(NumberOfCases, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (farms)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = n(), # Count outbreaks per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Poultry (birds)" = {
      hpai_data_poultry %>%
        mutate(`Outbreak Date` = as.Date(`Outbreak Date`, format = "%m-%d-%Y")) %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Outbreak Date`) %>%
        summarise(
          Cases = sum(FlockSize, na.rm = TRUE), # Sum birds affected per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Outbreak Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Dairy cattle (farms)" = {
      hpai_data_cattle_cum %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, Date) %>%
        summarise(
          Cases = n(), # Count outbreaks per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = Date) %>%
        arrange(State, Date_confirmed)
    },
    "Domestic cats" = {
      hpai_data_mammals_cats_map %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(), # Count cases per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild birds" = {
      hpai_data_wildbirds_map %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Collection Date`) %>%
        summarise(
          Cases = n(), # Count detections per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Collection Date`) %>%
        arrange(State, Date_confirmed)
    },
    "Wild mammals" = {
      hpai_data_mammals_mammals_map %>%
        mutate(State = sapply(State, state_abbr_to_name)) %>%
        group_by(State, `Date Detected`) %>%
        summarise(
          Cases = n(), # Count cases per day
          .groups = "drop"
        ) %>%
        rename(Date_confirmed = `Date Detected`) %>%
        arrange(State, Date_confirmed)
    }
  )
  
  if (is.null(trend_data) || nrow(trend_data) == 0) {
    return(plotly_empty() %>% 
             layout(title = "No data available for this species"))
  }
  
  # Debug: print data summary
  print(paste("Species:", input$species))
  print(paste("Total data rows:", nrow(trend_data)))
  print(paste("Date range:", min(trend_data$Date_confirmed, na.rm = TRUE), "to", max(trend_data$Date_confirmed, na.rm = TRUE)))
  print(paste("Number of states:", length(unique(trend_data$State))))
  print("Sample states:")
  print(head(unique(trend_data$State), 10))
  
  # Apply timeline filtering if available
  if (!is.null(input$timeline_range) && length(input$timeline_range) == 2) {
    print(paste("Applying timeline filter:", input$timeline_range[1], "to", input$timeline_range[2]))
    trend_data <- trend_data %>%
      filter(Date_confirmed >= input$timeline_range[1] & Date_confirmed <= input$timeline_range[2])
    print(paste("After filtering, rows:", nrow(trend_data)))
  }
  
  # Get top states by total cases
  state_totals <- trend_data %>%
    group_by(State) %>%
    summarise(
      Total_Cases = sum(Cases, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Cases)) %>%
    head(6) # Top 6 states
  
  # Filter to top states only
  plot_data <- trend_data %>%
    filter(State %in% state_totals$State)
  
  # Create a simple line plot with all states
  plot_ly() %>%
    add_trace(
      data = plot_data,
      x = ~Date_confirmed,
      y = ~Cases,
      color = ~State,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(width = 1.5),
      marker = list(size = 4)
    ) %>%
    layout(
      title = list(
        text = paste(input$species, "Cases Trends by State"),
        font = list(size = 20, color = "black")
      ),
      xaxis = list(
        title = "Date",
        tickfont = list(size = 14),
        gridcolor = "lightgrey",
        # Ensure proper date formatting and range
        type = "date",
        tickformat = "%b %Y",
        tickmode = "auto",
        nticks = 10
      ),
      yaxis = list(
        title = "Cases",
        tickfont = list(size = 14),
        gridcolor = "lightgrey",
        # Start from 0 for better visualization
        rangemode = "tozero"
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      height = 600,
      showlegend = TRUE,
      # Add hover mode for better interaction
      hovermode = "closest",
      # Adjust margins to ensure graph uses full width and aligns with blue container
      margin = list(l = 40, r = 40, t = 80, b = 100)
    )
})