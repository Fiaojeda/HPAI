##Updates log
#V0.5. Launch version
rsconnect::setAccountInfo(name='seballanos',
                          token='32D6656993DAC0AE43C72571FFE4A9CA',
                          secret='2GuWNEjFPjuoFP8vrgsMBnduP2Y0sbNND0QKvy9O')

#Cornell College of Veterinary Medicine HPAI Dashboard v0.5.
#September, 2025. Llanos-Soto, S., Barreto-Ojeda, S., Smith, E., Gamble, A., Bento, A., Hayden, A., Travis, A.

#Summary:

#Package library
if (!require(shiny))
  install.packages("shiny")
if (!require(plotly))
  install.packages("plotly")
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(sf))
  install.packages("sf")
if (!require(tigris))
  install.packages("tigris")
if (!require(hrbrthemes))
  install.packages("hrbrthemes")
if (!require(httr))
  install.packages("httr")
if (!require(jsonlite))
  install.packages("jsonlite")
if (!require(rvest))
  install.packages("rvest")
if (!require(RColorBrewer))
  install.packages("RColorBrewer")
if (!require(leaflet))
  install.packages("leaflet")
if (!require(htmltools))
  install.packages("htmltools")
if (!require(shinyjs))
  install.packages("shinyjs")
if (!require(sparkline))
  install.packages("sparkline")



#core packages for Shiny application
library(shiny)         #build interactive web applications
library(shinyjs)       #enhance Shiny apps with JavaScript functionality

#data visualization and interactivity
library(plotly)        #create interactive and animated plots
library(leaflet)       #build interactive web maps
library(RColorBrewer)  #generate color palettes for maps and plots
library(hrbrthemes)    #apply clean, modern themes to ggplot2 visualizations
library(sparkline)     #create sparkline mini charts for data trends

#data manipulation and wrangling
library(tidyverse)     #collection of packages for data manipulation and visualization (includes dplyr, ggplot2, tidyr, etc.)
library(jsonlite)      #work with JSON data (useful for API responses and web data)

#spatial and geospatial data handling
library(sf)            #handle and analyze spatial (vector) data using simple features
library(tigris)        #download US Census TIGER/Line shapefiles (e.g., state and county boundaries)

#web scraping and API requests
library(httr)          #perform HTTP requests (e.g., download data from web)
library(rvest)         #scrape and extract data from HTML web pages

#HTML content creation
library(htmltools)     #create and manage HTML content in Shiny apps


#####================================== DATA IMPORT =====================================
source("data_importation.R")

#===================================================================================================#
#####================================ USER INTERFACE (UI) =====================================######
#===================================================================================================#
#The user interface is what allows users to access the maps and information on HPAI. This section
#defines where each things go (e.g. map, slider, menu, etc.) and how it looks (aesthetics)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(
        "
      .container, .container-fluid {
        width: 100% !important;
        max-width: 100% !important;
        margin: 0px !important;
        padding: 0px !important;
      }

    .leaflet-interactive { cursor: pointer !important; }
    body { background-color: #ffffff; font-family: 'Arial', sans-serif; padding-top: 0px; overflow-y: auto;}
    
    /* Make Leaflet map background white */
    .leaflet-container {
      background-color: #ffffff !important;
    }
    
    /* Ensure map tiles background is white */
    .leaflet-tile-pane {
      background-color: #ffffff !important;
    }
    
    /* Make sure the map div background is white */
    .leaflet-map-pane {
      background-color: #ffffff !important;
    }

.navbar {
  background-color: #b31b1b !important;
  border-bottom: 2px solid #b31b1b;
  width: 100% !important;
  display: flex;
  align-items: center;
  justify-content: center;
  min-height: 150px;
  padding: 10px 0;
  overflow: visible;
  flex-wrap: nowrap;
}

.navbar-brand {
  color: white !important;
  font-weight: bold;
  font-size: clamp(16px, 4vw, 20px) !important;
  white-space: normal !important;
  max-width: 100%;
  line-height: 1.3;
  padding: 0;
  display: flex;
  align-items: center;
  justify-content: center;
  text-align: center;
  width: 100%;
}

.navbar-nav {
  display: flex;
  flex-grow: 0;
  justify-content: flex-end;
  align-items: center;
  margin-left: auto;
  padding-right: 0;
  position: absolute;
  right: 0;
  top: 50%;
  transform: translateY(-50%);
}

.navbar-nav>li>a {
  color: white !important;
  font-weight: normal;
  font-size: clamp(15px, 4vw, 22px) !important;
  text-decoration: underline !important;
  text-underline-offset: 3px !important;
  text-decoration-thickness: 2px !important;
  padding: 10px 15px !important;
  margin: 0 2px !important;
  border-radius: 5px !important;
  transition: all 0.3s ease !important;
}

.navbar-nav>li:last-child>a {
  margin-right: 0 !important;
  padding-right: 15px !important;
}

.navbar-nav>li.active>a {
  background-color: #8b1515 !important;
  color: white !important;
  text-decoration: underline !important;
  text-underline-offset: 3px !important;
  text-decoration-thickness: 2px !important;
  font-weight: bold !important;
}

.navbar-nav>li>a:hover {
  background-color: #8b1515 !important;
  color: white !important;
  text-decoration: underline !important;
  text-underline-offset: 3px !important;
  text-decoration-thickness: 2px !important;
  transform: translateY(-2px) !important;
}

.irs-bar {
  background: #34495e !important;
  border-top: 1px solid #b31b1b!important;
  border-bottom: 1px solid #b31b1b !important;
}

.irs-line {
  background: #838b91 !important; 
  border: 1px solid #003366 !important;
}

.irs-single, .irs-from, .irs-to {
  font-size: 15px !important;  
  font-family: Arial, sans-serif !important;
  color: #FFFFFF !important; 
  background-color: #34495e !important; 
  padding: 2px !important; 
  border-radius: 5px !important; 
}

.irs-grid-text {
  font-size: 16px !important;  
    font-family: Arial, sans-serif !important;
    color: #34495e !important;
}

.irs-slider {
  width: 23px !important;  
  height: 23px !important;
}

#date {
  width: 70% !important; 
  max-width: 70% !important; 
  margin: 0 auto; 
}


.irs-bar-edge {
  background-color: #003366 !important; 
}

.mainPanel { max-height: none; background-color: #ffffff; padding: 20px; border-radius: 10px; width: 100% !important; margin: auto !important;}

/* Timeline slider styling */
#timeline_range {
  margin: 20px 0 !important;
  width: 100% !important;
}
.irs--shiny .irs-bar {
  background-color: #34495e !important;
  border-color: #34495e !important;
}
.irs--shiny .irs-single {
  background-color: #34495e !important;
  border-color: #34495e !important;
}
.irs--shiny .irs-handle {
  background-color: #34495e !important;
  border-color: #34495e !important;
}
.irs--shiny .irs-grid-text {
  display: none !important;
}
.irs--shiny .irs-line {
  height: 8px !important;
}
.irs--shiny .irs-bar {
  height: 8px !important;
}
.irs--shiny .irs-grid {
  height: 20px !important;
}

  ")
    )
  ),
  
  tags$script(HTML("
    function toggleTooltip(element) {
      var tooltip = element.parentNode.querySelector('#tooltip');
      if (tooltip.style.display === 'block') {
        tooltip.style.display = 'none';
      } else {
        tooltip.style.display = 'block';
      }
    }
  ")),
  
  #top Navigation Bar
navbarPage(
  title = div(
    style = "
      display: flex;
      align-items: center;
      justify-content: space-between;
      width: 100%;
      height: 100%;
      padding: 0 25px;
    ",
    # Logo on the left
    div(
      style = "flex-shrink: 0;",
      img(src = "logo_red_carnelian.svg", height = "150px", alt = "Cornell Logo")
    ),
    # Title centered in the middle
    div(
      style = "
        flex: 1;
        display: flex;
        justify-content: center;
        align-items: center;
        text-align: center;
        margin: 0 20px;
      ",
      span("Highly Pathogenic Avian Influenza (HPAI) in the United States", 
           style = "font-size: clamp(14px, 2.5vw, 18px); 
                    font-weight: bold; 
                    color: white; 
                    line-height: 1.1; 
                    white-space: nowrap;")
    ),
    # Empty div for balance (same width as logo)
    div(style = "width: 150px; flex-shrink: 0;")
  ),
    position = "static-top",
    #keeps the tabs always at the top
    collapsible = TRUE,
    #allows collapsing on smaller screens
    
    tabPanel("Summary", fluidRow(
      mainPanel(
        class = "mainPanel",
        fluidRow(
          column(
            12, 
            div(
              style = "display: flex; flex-wrap: wrap; justify-content: center; text-align: center; 
               background-color: #f8f9fa; border-radius: 5px; padding: 10px;",
              h4("Last updates", style= "font-weight:bold; font-size: clamp(8px, 4vw, 18px) !important; color: #34495e; width: 100%;"),
              div(
                style = "font-size: clamp(8px, 4vw, 14px); color:#34495e; display: flex; flex-wrap: wrap; gap: 5px; justify-content: center;",
                HTML(paste("<b>Humans:</b> ", humans_date, " —")),
                HTML(paste("<b>Poultry:</b> ", poultry_date, " —")),
                HTML(paste("<b>Dairy cattle:</b> ", cattle_date, " —")),
                HTML(paste("<b>Domestic cats:</b> ", cats_date, " —")),
                HTML(paste("<b>Wild birds:</b> ", wildbirds_date, " —")),
                HTML(paste("<b>Wild mammals:</b> ", wildmammals_date))
              )
            )
          )
        ),
        
        fluidRow(
          column(
            1,
            
            
            
            
            
          )
          
          ,
          
          column(
            10,
            div(
              style = "border: 3px solid #34495e; padding: 10px; border-radius: 10px; 
           margin: 10px; text-align: center; background-color: #edf4fa; 
           height: auto; display: flex; flex-direction: column; 
           align-items: center; justify-content: center; gap: 10px; 
           position: relative; word-wrap: break-word; 
           overflow-wrap: break-word; white-space: normal; max-width: 100%;",
            
              
              div(
                style = "padding: 2px; border-radius: 10px; margin: 2px; text-align: center; height: auto; 
             display: flex; flex-direction: column; align-items: center; justify-content: center; 
             gap: 2px; color: #34495e; font-weight: bold; font-size: 20px; background-color: #edf4fa; 
           position: relative; word-wrap: break-word; 
           overflow-wrap: break-word; white-space: normal;",
                "Select timeframe:",
                div(
                  style = "font-size: clamp(10px, 4vw, 16px) !important; min-width: 110%;",
                  selectInput(
                    "case_filter",
                    "",
                    choices = c("Last 15 days", "Cumulative reported cases to date"),
                    selected = "Cumulative reported cases to date",
                    width = "110%"
                  )
                )
              ),
              
              div(
                style = "border: 0px solid #34495e; padding: 10px; border-radius: 10px; 
             margin: 10px; text-align: center; background-color: #edf4fa; 
             height: auto; display: flex; flex-direction: row; 
             align-items: center; justify-content: center; 
             gap: 10px; position: relative; word-wrap: break-word; 
             overflow-wrap: break-word; white-space: normal; 
             width: 100%; max-width: 1400px; flex-wrap: wrap;",
                
                
                div(id = "humans_metric_box", style = "border: 2px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 30%; max-width: 210px; min-width: 80px; 
                 background-color: #2c3e50; color: white; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1; cursor: pointer; transition: all 0.3s ease;", 
                 onclick = "Shiny.setInputValue('clicked_species', 'Humans')",
                 onmouseover = "this.style.backgroundColor='#34495e'; this.style.transform='scale(1.05)'",
                 onmouseout = "this.style.backgroundColor='#2c3e50'; this.style.transform='scale(1)'",
                    span(style= "font-size: clamp(10px, 4vw, 17px) !important; color:#F6AE2D; font-weight:bold;", "HUMANS"),
                    uiOutput("HumanCases", style="font-size: clamp(8px, 4vw, 17px) !important;")),
                
                div(id = "poultry_metric_box", style = "border: 2px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 30%; max-width: 210px; min-width: 80px; 
                 background-color: #2c3e50; color: white; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1; cursor: pointer; transition: all 0.3s ease;", 
                 onclick = "Shiny.setInputValue('clicked_species', 'Poultry (farms)')",
                 onmouseover = "this.style.backgroundColor='#34495e'; this.style.transform='scale(1.05)'",
                 onmouseout = "this.style.backgroundColor='#2c3e50'; this.style.transform='scale(1)'",
                    span(style= "font-size: clamp(10px, 4vw, 17px) !important; color:#F6AE2D; font-weight:bold;", "POULTRY FARMS"),
                    uiOutput("poultryCases", style="font-size: clamp(8px, 4vw, 17px) !important;")),
                
                div(id = "dairy_metric_box", style = "border: 2px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 30%; max-width: 210px; min-width: 80px; 
                 background-color: #2c3e50; color: white; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1; cursor: pointer; transition: all 0.3s ease;", 
                 onclick = "Shiny.setInputValue('clicked_species', 'Dairy cattle (farms)')",
                 onmouseover = "this.style.backgroundColor='#34495e'; this.style.transform='scale(1.05)'",
                 onmouseout = "this.style.backgroundColor='#2c3e50'; this.style.transform='scale(1)'",
                    span(style= "font-size: clamp(10px, 4vw, 17px) !important; color:#F6AE2D; font-weight:bold;", "DAIRY FARMS"),
                    uiOutput("cattleCases", style="font-size: clamp(8px, 4vw, 17px) !important;")),
                
                div(id = "cats_metric_box", style = "border: 2px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 30%; max-width: 210px; min-width: 80px; 
                 background-color: #2c3e50; color: white; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1; cursor: pointer; transition: all 0.3s ease;", 
                 onclick = "Shiny.setInputValue('clicked_species', 'Domestic cats')",
                 onmouseover = "this.style.backgroundColor='#34495e'; this.style.transform='scale(1.05)'",
                 onmouseout = "this.style.backgroundColor='#2c3e50'; this.style.transform='scale(1)'",
                    span(style= "font-size: clamp(10px, 4vw, 17px) !important; color:#F6AE2D; font-weight:bold;", "DOMESTIC CATS"),
                    uiOutput("catCases", style="font-size: clamp(8px, 4vw, 17px) !important;")),
                
                div(id = "wildbirds_metric_box", style = "border: 2px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 30%; max-width: 210px; min-width: 80px; 
                 background-color: #2c3e50; color: white; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1; cursor: pointer; transition: all 0.3s ease;", 
                 onclick = "Shiny.setInputValue('clicked_species', 'Wild birds')",
                 onmouseover = "this.style.backgroundColor='#34495e'; this.style.transform='scale(1.05)'",
                 onmouseout = "this.style.backgroundColor='#2c3e50'; this.style.transform='scale(1)'",
                    span(style= "font-size: clamp(10px, 4vw, 17px) !important; color:#F6AE2D; font-weight:bold;", "WILD BIRDS"),
                    uiOutput("wildbirdCases", style="font-size: clamp(8px, 4vw, 17px) !important;")),
                
                div(id = "wildmammals_metric_box", style = "border: 2px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 30%; max-width: 210px; min-width: 80px; 
                 background-color: #2c3e50; color: white; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1; cursor: pointer; transition: all 0.3s ease;", 
                 onclick = "Shiny.setInputValue('clicked_species', 'Wild mammals')",
                 onmouseover = "this.style.backgroundColor='#34495e'; this.style.transform='scale(1.05)'",
                 onmouseout = "this.style.backgroundColor='#2c3e50'; this.style.transform='scale(1)'",
                    span(style= "font-size: clamp(10px, 4vw, 17px) !important; color:#F6AE2D; font-weight:bold;", "WILD MAMMALS"),
                    uiOutput("mammalCases", style="font-size: clamp(8px, 4vw, 17px) !important;"))
              )
            ),
            div(
              style = "padding: 0px; margin: 0px; background-color: #ffffff;
                                     display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 0px; text-align: center;",
              
              # Tooltip for HPAI case info
              div(
                style = "display: flex; align-items: center; gap: 8px; padding-top: 5px; border-radius: 10px;",
                uiOutput("tooltip_ui"),
                span("Cumulative Reported Cases", style = "color: #34495e; font-weight: bold; font-size: 20px;")
              ),
              
              div(
                style = "display: flex; justify-content: center; align-items: center; gap: 0px; font-size: clamp(10px, 4vw, 16px) !important; font-weight: bold;",
                selectInput(
                  "species",
                  "",
                  choices = c(
                    "Humans",
                    "Poultry (farms)",
                    "Poultry (birds)",
                    "Dairy cattle (farms)",
                    "Domestic cats",
                    "Wild birds",
                    "Wild mammals"
                  ),
                  selected = "Humans"
                )
              ),
              div(
                style = "width: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 0px;",
                div(
                  style = "width: 100%; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 5px;",
                  div(
                    style = "width: 85%; display: flex; align-items: center; justify-content: center;",

                    div(style = "text-align: center;", uiOutput("selected_date"))
                    
                  ),
                  
                  #slider
                  div(
                    style = "width: 85%;",
                    sliderInput(
                      "date",
                      "",
                      min = mdy("1/1/2024"),
                      max = mdy("1/1/2025"),
                      value = mdy("12/31/2024"),
                      timeFormat = "%b %Y",
                      step = 1,
                      width = "100%"
                    )
                  )
                ),
                
                #map
                div(style = "width: 100%; margin: 0; padding: 0;", uiOutput("map_container"))
              )
            ),
            
            #cumulative plot
            div(style="border: 3px solid #34495e; padding: 25px; border-radius: 10px; margin: 15px; background-color: #edf4fa;
      flex-direction: column; align-items: center; justify-content: center; gap: 20px; text-align: center; max-height: 1100px;",
                uiOutput("plot_title", style= "padding-top: 5px; padding-bottom: 10px; border-radius: 10px; color: #34495e; font-weight: bold; font-size: 20px;"),
                div(
                  style = "background-color: #f8f9fa; border: 1px solid #34495e; padding: 8px 12px; 
           border-radius: 5px; font-size: clamp(10px, 4vw, 18px); color: #34495e; font-family: Arial; 
           margin: 10px auto; font-weight: normal; text-align: center; 
           max-width: 1000px; display: block;",
                  strong("Instructions: "), 
                  "double-click on a State's name to look for reported cases in that specific State. 
   Double-clicking on a selected state will restore the view to all States. 
   You can select multiple States to compare reported cases. 
   Hovering on specific points in the map will provide detailed information on the report"
                ),
                div(
                  style = "width: 100%; margin-top: 10px; padding: 15px;",
                  uiOutput("timeline_slider_ui")
                ),
                div(
                  style = "width: 100%; margin-top: 10px; padding: 15px;",
                  uiOutput("plot_type_selector")
                ),
                div(
                  style = "width: 100%; height: auto; min-height: 400px; background-color: white; border-radius: 8px; padding: 20px; margin: 0; overflow: hidden;",
                  uiOutput("dynamic_plot")
                )
            )
            ),
          
          column(
            1,
            
            
            
            
            
          ),
          
          column(
            12,
            
            style = "background-color: #b31b1b; color: white; padding: 20px; margin-top: 20px; text-align: center",
            h4("Cornell College of Veterinary Medicine, 2025"),
            h6(
              "The Cornell College of Veterinary Medicine provides this dashboard for informative and research purposes only, with no guarantees of accuracy or reliability, no liability for any resulting loss or damage, and no responsibility for third-party data included."
            ),
            h6("v1.0 (September, 2025)")
          )
        )
      )
    )) ,
    
    
###### ABOUT TAB #####
    tabPanel(
      "About",
      mainPanel(
        class = "mainPanel",
        div(
          style = "font-family: Arial; color: black; text-align: left; margin-top: 0x; margin-bottom: 10px; justify-content: center",
          
          div(
            style = "background-color: white; border: 1px white; padding: 20px; border-radius: 8px;
           font-family: Arial; color: black; text-align: left; margin-top:0px ;margin: 5x;",
            h3(style = "font-size: clamp(10px, 4vw, 24px) !important; font-weight:bold;color: #34495e", "About the team"),
            
            h4(
              style = "line-height: 1.4; font-size: clamp(10px, 4vw, 19px) !important;",
              "This dashboard provides up-to-date information on Highly Pathogenic Avian Influenza (HPAI) cases across wildlife,
    domestic animals, and humans in the United States (US). Our goal is to inform both the general public and the scientific
    community about HPAI occurrences through a One Health perspective, recognizing the interconnectedness of human,
    animal, and environmental health. The development of the HPAI dashboard was a collaborative effort from faculty and
    staff from the Department of Public & Ecosystem Health at the Cornell University College of Veterinary Medicine."
            )
          ),
          
          div(
            style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 15px; background-color: #edf4fa;padding: 10px; border-radius: 10px;",
            
            div(
              style = "border: 0px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 45%; max-width: 1500px; min-width: 80px; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1;",
              
              div(
                style = "padding: 25px;",
                img(src = "Barreto_S.png", width = "200px", style = "border-radius: 10px; width: clamp(60px, 10vw, 200px); height: auto;")
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong(style = "font-size: 20px;", "Sofia Barreto-Ojeda"),
                br(),
                "MPH Epidemiology Candidate",
                br(),
                "Email: ",
                a("ksb236@cornell.edu", href = "mailto:ksb236@cornell.edu"),
                br(),
                a("Profile", href = "https://www.linkedin.com/in/sofia-barreto-ojeda/", target = "_blank")
              )
            ),
            
            div(
              style = "border: 0px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 45%; max-width: 1500px; min-width: 80px; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1;",
              
              div(
                style = "padding: 25px;",
                img(src = "llanos_S.jpg", width = "200px", style = "border-radius: 10px; width: clamp(60px, 10vw, 200px); height: auto;")
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong(style = "font-size: 20px;", "Sebastián G. Llanos-Soto"),
                br(),
                "Postdoctoral Researcher, PhD, DVM",
                br(),
                "Email: ",
                a("sgl67@cornell.edu", href = "mailto:sgl67@cornell.edu"),
                br(),
                a("Profile", href = "https://www.linkedin.com/in/llanossoto", target =
                    "_blank")
              )
            ),
            
            div(
              style = "border: 0px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 45%; max-width: 1500px; min-width: 80px; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1;",
              
              div(
                style = "padding: 25px;",
                img(src = "bento_A.png", width = "200px", style = "border-radius: 10px; width: clamp(60px, 10vw, 200px); height: auto;")
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong(style = "font-size: 20px;", "Ana Bento"),
                br(),
                "Assistant Professor, PhD",
                br(),
                "Email: ",
                a("arb24@cornell.edu", href = "mailto:arb24@cornell.edu"),
                br(),
                a("Profile", href = "https://www.vet.cornell.edu/ana-bento-phd", target =
                    "_blank")
              )
            ),
            
            
            div(
              style = "border: 0px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 45%; max-width: 1500px; min-width: 80px; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1;",
              
              div(
                style = "padding: 25px;",
                img(src = "gamble_A.jpg", width = "200px", style = "border-radius: 10px; width: clamp(60px, 10vw, 200px); height: auto;")
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong(style = "font-size: 20px;", "Amandine Gamble"),
                br(),
                "Assistant Professor, PhD, DVM, MSc",
                br(),
                "Email: ",
                a("amandine.gamble@cornell.edu", href = "mailto:amandine.gamble@cornell.edu"),
                br(),
                a("Profile", href = "https://www.vet.cornell.edu/research/amandine-gamble-phd-dvm-msc", target =
                    "_blank")
              )
            ),
            
            div(
              style = "border: 0px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 45%; max-width: 1500px; min-width: 80px; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1;",
              
              div(
                style = "padding: 25px;",
                img(src = "smith_E.jpg", width = "200px", style = "border-radius: 10px; width: clamp(60px, 10vw, 200px); height: auto;")
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong(style = "font-size: 20px;", "Elodie Smith"),
                br(),
                "Director of Digital Communications, PhD",
                br(),
                "Email: ",
                a("eeg47@cornell.edu", href = "mailto:eeg47@cornell.edu"),
                br(),
                a("Profile", href = "https://www.vet.cornell.edu/about-us/news/marketing-and-communications", target =
                    "_blank")
              )
            ),
            
            div(
              style = "border: 0px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 45%; max-width: 1500px; min-width: 80px; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1;",
              
              div(
                style = "padding: 25px;",
                img(src = "hayden_A.png", width = "200px",style = "border-radius: 10px; width: clamp(60px, 10vw, 200px); height: auto;")
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong(style = "font-size: 20px;", "Alistair Hayden"),
                br(),
                "Assistant Professor of Practice, PhD",
                br(),
                "Email: ",
                a("ath79@cornell.edu", href = "mailto:ath79@cornell.edu"),
                br(),
                a("Profile", href = "https://www.vet.cornell.edu/research/alistair-hayden-phd", target =
                    "_blank")
              )
            ),
            
            div(
              style = "border: 0px solid #2c3e50; padding: 10px; border-radius: 10px;
                 text-align: center; width: 45%; max-width: 1500px; min-width: 80px; 
                 display: flex; flex-direction: column; gap: 5px;
                 word-wrap: break-word; overflow-wrap: break-word; 
                 white-space: normal; flex-shrink: 1",
              
              div(
                style = "padding: 25px;",
                img(src = "travis_A.jpg", width = "200px", style = "border-radius: 10px; width: clamp(60px, 10vw, 200px); height: auto;")
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong(style = "font-size: 20px;", "Alexander J. Travis"),
                br(),
                "Director of Cornell Public Health, VMD, PhD",
                br(),
                "Email: ",
                a("ajt32@cornell.edu", href = "mailto:ajt32@cornell.edu"),
                br(),
                a("Profile", href = "https://www.vet.cornell.edu/research/alexander-j-travis-vmd-phd", target =
                    "_blank")
              )
            )
          ),
          
          div(
            style = "background-color: white; border: 1px white; padding: 20px; border-radius: 8px;
           font-family: Arial; color: black; text-align: left; margin-bottom:5px;",
            h3(style = "font-size: clamp(10px, 4vw, 24px) !important; font-weight:bold; color: #34495e", "Datasets")
          ),
          
          #Datasets
          div(
            style = "display: flex; flex-wrap: wrap; justify-content: center; gap: 10px;
           border-radius: 10px; background-color: #edf4fa; padding: 15px;",
            
            div(
              style = "width: 45%; border: 0px solid #34495e; border-radius: 10px; background-color: #edf4fa;
             padding: 15px; font-size: 18px; display: flex; flex-direction: column;
             align-items: center; text-align: center;",
              img(
                src = "HumanIcon.png",
                height = "75px",
                style = "margin-bottom: 15px; height: clamp(40px, 10vw, 75px);"
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong("Humans"),
                br(),
                downloadLink("download_humans", "Download data"),
                br(),
                "Data sources: ",
                a("CDC newsroom", href = "https://www.cdc.gov/media/index.html", target =
                    "_blank"),
                " and Departments of Public Health from individual states and counties.",
                br(),
                "Data start date: January 4, 2024",
                br(),
                paste("Last update: ",  humans_date)
              )
            ),
            
            div(
              style = "width: 45%; border: 0px solid #34495e; border-radius: 10px; background-color: #edf4fa;
             padding: 15px; font-size: 18px; display: flex; flex-direction: column;
             align-items: center; text-align: center;",
              
              img(
                src = "PoultryIcon.png",
                height = "75px",
                style = "margin-bottom: 15px; height: clamp(40px, 10vw, 75px);"
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong("Poultry"),
                br(),
                downloadLink("download_poultry", "Download data"),
                br(),
                "Data source: ",
                a("CDC", href = "https://www.cdc.gov/bird-flu/situation-summary/data-map-commercial.html", target =
                    "_blank"),
                br(),
                "Data start date: February 08, 2022",
                br(),
                paste("Last update: ",  poultry_date)
              )
            ),
            
            div(
              style = "width: 45%; border: 0px solid #34495e; border-radius: 10px; background-color: #edf4fa;
             padding: 15px; font-size: 18px; display: flex; flex-direction: column;
             align-items: center; text-align: center;",
              
              img(
                src = "CattleIcon.png",
                height = "75px",
                style = "margin-bottom: 15px; height: clamp(40px, 10vw, 75px);"
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong("Dairy cattle"),
                br(),
                downloadLink("download_cattle", "Download data"),
                br(),
                "Data source: ",
                a("USDA", href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/hpai-confirmed-cases-livestock", target =
                    "_blank"),
                br(),
                "Data start date: March 25, 2024",
                br(),
                paste("Last update: ",  cattle_date)              )
            ),
            
            div(
              style = "width: 45%; border: 0px solid #34495e; border-radius: 10px; background-color: #edf4fa;
             padding: 15px; font-size: 18px; display: flex; flex-direction: column;
             align-items: center; text-align: center;",
              
              img(
                src = "CatIcon.png",
                height = "75px",
                style = "margin-bottom: 15px;  height: clamp(40px, 10vw, 75px);"
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong("Domestic cats"),
                br(),
                downloadLink("download_cats", "Download data"),
                br(),
                "Data source: ",
                a("USDA", href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/mammals", target =
                    "_blank"),
                br(),
                "Data start date: December 23, 2022",
                br(),
                paste("Last update: ",  cats_date)              )
            ),
            
            div(
              style = "width: 45%; border: 0px solid #34495e; border-radius: 10px; background-color: #edf4fa;
             padding: 15px; font-size: 18px; display: flex; flex-direction: column;
             align-items: center; text-align: center;",
              
              img(
                src = "WildbirdIcon.png",
                height = "75px",
                style = "margin-bottom: 15px;  height: clamp(40px, 10vw, 75px);"
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong("Wild birds"),
                br(),
                downloadLink("download_wildbirds", "Download data"),
                br(),
                "Data source: ",
                a("USDA", href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/wild-birds", target =
                    "_blank"),
                br(),
                "Data start date: January 12, 2022",
                br(),
                paste("Last update: ",  "April 02, 2025")              )
            ),
            
            div(
              style = "width: 45%; border: 0px solid #34495e; border-radius: 10px; background-color: #edf4fa;
             padding: 15px; font-size: 18px; display: flex; flex-direction: column;
             align-items: center; text-align: center;",
              
              img(
                src = "WildmammalIcon.png",
                height = "75px",
                style = "margin-bottom: 15px;  height: clamp(40px, 10vw, 75px);"
              ),
              
              div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
                strong("Wild mammals"),
                br(),
                downloadLink("download_mammals", "Download data"),
                br(),
                "Data source: ",
                a("USDA", href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/mammals", target =
                    "_blank"),
                br(),
                "Data start date: May 05, 2022",
                br(),
                paste("Last update: ",  "April 01, 2025")
              )
            )
          )
        ),
        div(
          style = "background-color: white; border: 1px white; padding: 20px; border-radius: 8px;
           font-family: Arial; color: black; text-align: left; margin-bottom:5px;",
          h3(style ="font-size: clamp(10px, 4vw, 24px) !important; font-weight:bold; color: #34495e", "Data trackers & dashboards")
        ),
        div(
          style = "border: 0px solid #34495e; border-radius: 10px; background-color: #edf4fa; padding: 25px;
             margin-bottom: 15px; font-size: 18px; display: flex; flex-direction: column; align-items: flex-start;",
          
          
          
          div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
            strong("Humans"),
            br(),
            a(
              "H5 Bird Flu (CDC): Current Situation",
              href = "https://www.cdc.gov/bird-flu/situation-summary/index.html",
              target = "_blank"
            ),
            br(),
            a(
              "How CDC is monitoring influenza data (CDC)",
              href = "https://www.cdc.gov/bird-flu/h5-monitoring/index.html",
              target = "_blank"
            ),
            br(),
            br()
          ),
          
          div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
            strong("Poultry"),
            br(),
            a(
              "USDA Reported H5N1 Bird Flu Detections in Poultry (CDC)",
              href = "https://www.cdc.gov/bird-flu/situation-summary/data-map-commercial.html",
              target = "_blank"
            ),
            br(),
            a(
              "Confirmations of HPAI in Commercial and Backyard Flocks (USDA)",
              href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/commercial-backyard-flocks",
              target = "_blank"
            ),
            br(),
            br()
          ),
          
          div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
            strong("Dairy Cattle"),
            br(),
            a(
              "HPAI Confirmed Cases in Livestock (USDA)",
              href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/hpai-confirmed-cases-livestock",
              target = "_blank"
            ),
            br(),
            br()
          ),
          
          div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
            strong("Wildlife"),
            br(),
            a(
              "Highly Pathogenic Avian Influenza (Cornell Wildlife Health Lab)",
              href = "https://cwhl.vet.cornell.edu/article/highly-pathogenic-avian-influenza",
              target = "_blank"
            ),
            br(),
            a(
              "Detections of HPAI in Wild Birds (USDA)",
              href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/wild-birds",
              target = "_blank"
            ),
            br(),
            a(
              "Detections of HPAI in Mammals (USDA)",
              href = "https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/mammals",
              target = "_blank"
            ),
            br(),
            br()
          ),
          
          div(style="font-size: clamp(10px, 4vw, 19px) !important; color: #34495e",
            strong("Other"),
            br(),
            a(
              "DiTTO: Digital Twin for Transboundary OneHealth (University of Virginia)",
              href = "https://ditto.bii.virginia.edu/",
              target = "_blank"
            ),
            br()
          )
        ),
        fluidRow(
          column(
            12,
            position = "static-bottom",
            style = "background-color: #b31b1b; color: white; padding: 20px; margin-top: 20px; text-align: center; font-family: Arial;",
            h4("Cornell College of Veterinary Medicine, 2025"),
            h6(
              "The Cornell College of Veterinary Medicine provides this dashboard for informative and research purposes only, with no guarantees of accuracy or reliability, no liability for any resulting loss or damage, and no responsibility for third-party data included."
            ),
            h6("v1.0 (September, 2025)")
          )
        )
      )
    )
    
  )
)

#=========================================================================================#
#####================================== SERVER ======================================######
#=========================================================================================#
#The server contains all the information that we want to display amd the mechanisms that
#connect everything together

server <- function(input, output, session) {
  #####================================== WELCOME WINDOW =====================================
  source("welcome_window.R", local = T)
  
  # Set default plot type
  updateRadioButtons(session, "plot_type", selected = "cumulative")
  
  ####=================================== BOX TOGGLE =========================================
  toggle_states <- reactiveValues(
    humans = TRUE,
    poultry = TRUE,
    cattle = TRUE,
    cats = TRUE,
    wildbirds = TRUE,
    wildmammals = TRUE
  )
  
  #function to generate the toggle UI dynamically
  generate_toggle_ui <- function(id, state_var) { #creates a reactive ui element for a toggle icon
    renderUI({ #returns ui for insertion into the app
      img_src <- if (toggle_states[[state_var]]) #chooses icon based on current toggle state
        "ToggleONIcon.png"
      else
        "ToggleOFFIcon.png"
      tags$img( #generates an <img> tag
        src = img_src, #sets image source to on/off icon
        height = "18px", #defines icon height
        style = "position: absolute; top: 5px; left: 5px; cursor: pointer;", #positions icon and shows pointer on hover
        onclick = paste0("Shiny.setInputValue('", id, "', Math.random())") #sends random value to server when clicked
      )
    })
  }
  
  #assigns the dynamic toggle UI
  output$toggle_humans_ui <- generate_toggle_ui("toggle_humans", "humans")
  output$toggle_poultry_ui <- generate_toggle_ui("toggle_poultry", "poultry")
  output$toggle_cattle_ui <- generate_toggle_ui("toggle_cattle", "cattle")
  output$toggle_cats_ui <- generate_toggle_ui("toggle_cats", "cats")
  output$toggle_wildbirds_ui <- generate_toggle_ui("toggle_wildbirds", "wildbirds")
  output$toggle_wildmammals_ui <- generate_toggle_ui("toggle_wildmammals", "wildmammals")
  
  # Tooltip UI that only shows for humans
  output$tooltip_ui <- renderUI({
    if (input$species == "Humans") {
      HTML('<div style="display: inline-block; position: relative;"><span style="cursor: pointer; color: #007bff; 
      font-weight: bold; font-size: 16px;" onclick="toggleTooltip(this)">ⓘ</span><div id="tooltip" style="position: absolute; 
      bottom: 100%; left: 50%; transform: translateX(-50%); background-color: white; color: #333; padding: 20px; border-radius: 30px; 
      font-size: 14px; z-index: 1000; display: none; width: 90vw; max-width: 600px; min-width: 200px; white-space: normal; 
      text-align: left; border: 2px solid #007bff; box-shadow: 0 4px 8px rgba(0,0,0,0.2); margin-bottom: 8px; 
      word-break: break-word;">The first reported human HPAI case in the US (April 29, 2022) is not visualized in the map to avoid stretching the map slider 
      since the second case in the country was not reported until nearly two years later.</div></div>')
    } else {
      # Return empty div when not humans to maintain layout
      div(style = "display: inline-block; width: 16px;")
    }
  })
  
  
  #toggle event observers
  observeEvent(input$toggle_humans, { #runs when the "toggle_humans" input is triggered (e.g., toggle icon clicked)
    toggle_states$humans <- !toggle_states$humans #flips the current toggle state for humans (on to off or off to on)
    toggle("humans_box") #toggles the visibility of the 'humans_box' UI element
    output$toggle_humans_ui <- generate_toggle_ui("toggle_humans", "humans") #updates the toggle icon to reflect the new state
  })
  
  observeEvent(input$toggle_poultry, {
    toggle_states$poultry <- !toggle_states$poultry
    toggle("poultry_box")
    output$toggle_poultry_ui <- generate_toggle_ui("toggle_poultry", "poultry")
  })
  
  observeEvent(input$toggle_cattle, {
    toggle_states$cattle <- !toggle_states$cattle
    toggle("cattle_box")
    output$toggle_cattle_ui <- generate_toggle_ui("toggle_cattle", "cattle")
  })
  
  observeEvent(input$toggle_cats, {
    toggle_states$cats <- !toggle_states$cats
    toggle("cats_box")
    output$toggle_cats_ui <- generate_toggle_ui("toggle_cats", "cats")
  })
  
  
  observeEvent(input$toggle_wildbirds, {
    toggle_states$wildbirds <- !toggle_states$wildbirds
    toggle("wildbirds_box")
    output$toggle_wildbirds_ui <- generate_toggle_ui("toggle_wildbirds", "wildbirds")
  })
  
  observeEvent(input$toggle_wildmammals, {
    toggle_states$wildmammals <- !toggle_states$wildmammals
    toggle("wildmammals_box")
    output$toggle_wildmammals_ui <- generate_toggle_ui("toggle_wildmammals", "wildmammals")
  })
  
  #####================================== METRIC BOX CLICK HANDLERS ================================
  # Handle clicks on metric boxes to update species selection
  observeEvent(input$clicked_species, {
    req(input$clicked_species)
    updateSelectInput(session, "species", selected = input$clicked_species)
  })
  
  #####================================== DATASET EXPORTATION ================================
  source("dataset_exportation.R", local = T)
  
  #####================================== SUMMARY METRIC BOXES ================================
  #These boxes contain summary information (e.g. first reported case, most affected state, etc.)
  source("boxes_metrics.R", local = T)
  
  #####================================== MAP CACHE CLEARING =====================================
  source("cache_clearing.R", local = T)
  
  #####================================== ADJUSTMENTS TO SLIDER =====================================
  source("slider_adjustments.R", local = T)
  
  #####================================== MAP RENDERING ========================================
  source("map_rendering.R", local = T)
  
  
  source("cumulative_cases.R", local = T)
  #####================================== CUMULATIVE CASES PLOTS ========================================

#These bar plots are not being used, but I once used them so I left the code here in case
#you find it useful
  
  ##Barplots-Humans
  output$barplot_cases <- renderPlotly({
    plot_data <- hpai_data_humans_bar
    
    x_var <- switch(
      input$barplot_type,
      "State" = "State",
      "Age" = "Age",
      "Source" = "Source",
      "Status" = "Status"
    )
    
    plot_summary <- plot_data %>%
      group_by(!!sym(x_var)) %>%
      summarise(TotalCases = sum(NumberOfCases, na.rm = TRUE)) %>%
      ungroup()
    
    maxestimation <- bind_rows(
      plot_data %>%
        group_by(State) %>%
        summarise(TotalCases = sum(NumberOfCases, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Category = "State"),
      
      plot_data %>%
        group_by(Age) %>%
        summarise(TotalCases = sum(NumberOfCases, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Category = "Age"),
      
      plot_data %>%
        group_by(Source) %>%
        summarise(TotalCases = sum(NumberOfCases, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Category = "Source"),
      
      plot_data %>%
        group_by(Status) %>%
        summarise(TotalCases = sum(NumberOfCases, na.rm = TRUE)) %>%
        ungroup() %>%
        mutate(Category = "Status")
    )
    
    
    p <- ggplot(plot_summary, aes(
      x = reorder(!!sym(x_var), TotalCases),
      y = TotalCases,
      text = paste("Reported cases:", TotalCases)
    )) +
      geom_bar(stat = "identity", fill = "#f39c12") +
      coord_flip() +
      labs(x = "", y = "Reported cases") +
      theme_ipsum() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.spacing.y = unit(10, "pt")
      ) +
      scale_y_continuous(
        labels = scales::comma,
        breaks = scales::pretty_breaks(n = 5),
        limits = c(0, max(maxestimation$TotalCases))
      )
    
    ggplotly(p, tooltip = c("text")) %>%
      layout(
        autosize = TRUE,
        font = list(size = 18),
        xaxis = list(
          title = list(
            text = "Reported cases",
            font = list(
              size = 20,
              family = "Arial",
              color = "black",
              weight = "bold"
            ),
            standoff = 20
          ),
          tickfont = list(
            size = 18,
            family = "Arial",
            color = "black"
          ),
          font = list(
            size = 20,
            family = "Arial",
            color = "black",
            weight = "bold"
          ),
          standoff = 20
        ),
        yaxis = list(
          tickfont = list(
            size = 18,
            family = "Arial",
            color = "black"
          ),
          title = list(
            font = list(
              size = 20,
              family = "Arial",
              color = "black",
              weight = "bold"
            ),
            standoff = 20
          )
        ),
        title = list(font = list(size = 20))
      ) 
    
  })
  

  
  
  
  
  
  
  
}

#This runs the shinyapp
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
