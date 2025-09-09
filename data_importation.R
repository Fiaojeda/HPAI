#===========================================================================================================#
#####===================================== DATA MANAGEMENT ============================================######
#===========================================================================================================#

#####===================================== DATA IMPORTATION ==============================================#####
##### HPAI Detections in wild birds #####
#Note: at the moment, wild bird data is manually downloaded from the USDA website: 
#https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/wild-birds
hpai_data_wildbirds <- read_csv("HPAI Detections in Wild Birds.csv")

#There are some NA values in the "Collection Date" column of the dataset. The following code allows to fill 
#those gaps with values in "Date Detected"
hpai_data_wildbirds<-hpai_data_wildbirds %>%
  mutate(`Collection Date`=ifelse(is.na(`Collection Date`), as.character(`Date Detected`), `Collection Date`))

#data is adjusted for mapping
hpai_data_wildbirds_map<-hpai_data_wildbirds %>%
  mutate(`Collection Date`=mdy(`Collection Date`)) #for maps

##### HPAI Detections in wild mammals #####
#Note: at the moment, wild mammal data is manually downloaded from the USDA website: 
#https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/mammals
#The dataset used for wild mammals also contains information of infections in cats. 
#For that reason, we are using the wild mammal dataset to: i) describe wild mammal infections and ii) cat infections, separately.

hpai_data_mammals <- read_csv("HPAI Detections in Mammals.csv")
hpai_data_mammals_mammals<-hpai_data_mammals %>% #wild mammal dataset
  filter(Species!="Domestic cat")

#The following code separates mammals from domestic cats
hpai_data_mammals_cats<-hpai_data_mammals %>% #domestic cat dataset
  filter(Species=="Domestic cat")

#data is adjusted for mapping
hpai_data_mammals_mammals_map<-hpai_data_mammals_mammals %>% #for maps
  mutate(`Date Collected`=mdy(`Date Collected`)) %>% #for maps
  mutate(`Date Detected`=mdy(`Date Detected`))

hpai_data_mammals_cum<- hpai_data_mammals_mammals_map #for cumulative plots

hpai_data_mammals_cats_map<-hpai_data_mammals_cats %>% #for maps
  mutate(`Date Collected`=mdy(`Date Collected`))%>% #for maps
  mutate(`Date Detected`=mdy(`Date Detected`))

##### HPAI Detections in poultry #####
#This is the only dataset that can still be downloaded automatically from the CDC website
url2<-"https://www.cdc.gov/bird-flu/modules/situation-summary/commercial-backyard-flocks.csv"
tryCatch({
  hpai_data_poultry<-read_csv(url2)
}, error = function(e) {
  # If web download fails, try to read from local file
  tryCatch({
    hpai_data_poultry <<- read_csv("commercial-backyard-flocks.csv")
    print("Successfully loaded poultry data from local file")
  }, error = function(e2) {
    # If local file also fails, create empty dataframe with expected structure
    hpai_data_poultry <<- data.frame(
      County = character(),
      State = character(), 
      `Outbreak Date` = character(),
      `Flock Type` = character(),
      `Flock Size` = numeric(),
      stringsAsFactors = FALSE
    )
    warning("Could not download poultry data from CDC website or load from local file. Using empty dataset.")
  })
})

hpai_data_poultry<-hpai_data_poultry %>% #name of the columns were changed to avoid issues with spaces in the middle
  rename(FlockType=`Flock Type`, FlockSize=`Flock Size`)

print(paste("Poultry data loaded:", nrow(hpai_data_poultry), "rows"))
print("Poultry data columns:")
print(names(hpai_data_poultry))
print("Sample poultry data:")
print(head(hpai_data_poultry))

hpai_data_poultry_map<-hpai_data_poultry %>%
  mutate(`Outbreak Date`=as.Date(`Outbreak Date`, format="%m-%d-%Y")) #for maps - handle MM-DD-YYYY format

print(paste("Poultry map data processed:", nrow(hpai_data_poultry_map), "rows"))
print("Sample poultry map data:")
print(head(hpai_data_poultry_map))
print("Date range in poultry data:")
print(range(hpai_data_poultry_map$`Outbreak Date`, na.rm = TRUE))

#manual import cattle data
#https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/hpai-confirmed-cases-livestock

hpai_data_cattle<-read.csv("Table Details by Date.csv", stringsAsFactors=FALSE, fileEncoding="UTF-16LE", header=FALSE)
hpai_data_cattle<-hpai_data_cattle[-1, , drop=FALSE]
hpai_data_cattle<-hpai_data_cattle %>% #splits the first column into multiple columns based on tab separator
  separate(V1, into=c("Date", "State", "Special_ID", "Production", "Species", "No_Measure_Value"), sep="\t", extra="merge")

hpai_data_cattle_map<-hpai_data_cattle %>%
  mutate(`Date`=dmy(`Date`)) #for maps

hpai_data_cattle_cum<-hpai_data_cattle %>%
  mutate(`Date`=dmy(`Date`))

##### HPAI Detections in poultry #####
#This data was manually collected from CDC reports and communications from State Public Health departments. 
#In some cases, there was the need to use Wayback Machine (https://web.archive.org/) to recover information 
#lost after websiteS were updated
hpai_data_humans<- read_csv("Human_Data.csv")
hpai_data_humans_bar<- read_csv("Human_Data.csv")
hpai_data_humans_map<-hpai_data_humans_bar %>% #for maps
  mutate(Date_confirmed=mdy(Date_confirmed))

# State population data for per 100,000 calculations
state_populations <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
            "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
            "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
            "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
            "Wisconsin", "Wyoming"),
  Population = c(5024279, 733391, 7151502, 3011524, 39538223, 5773714, 3605944, 989948, 21538187, 
                 10711908, 1455271, 1839106, 12812508, 6785528, 3190369, 2937880, 4505836, 4657757, 
                 1344212, 6177224, 7029917, 10077331, 5706494, 2961279, 6154913, 1084225, 1961504, 
                 3104614, 1377529, 9288994, 2117522, 20201249, 10439388, 779094, 11799448, 3959353, 
                 4237256, 13002700, 1097379, 5118425, 886667, 6910840, 29145505, 3271616, 643077, 
                 8631393, 7705281, 1793716, 5893718, 576851)
)

print("State population data loaded for per 100,000 calculations")


#obtains the date in which datasets were updated (only applies to some datasets now,
#since most are manually updated). The one for USDA is not currently being used.
get_last_modified_usda<-function(url){ #for USDA websites
  tryCatch({
    page<-read_html(url)
    
    #extracts all <span> elements and find the one that contains "Last Modified"
    spans<-page %>% html_nodes("span") %>% html_text(trim=TRUE)
    last_modified<-spans[grepl("Last Modified:", spans)]
    
    if(length(last_modified)>0){
      #removes "Last Modified: " and return only the date
      date_only<-gsub("Last Modified: ", "", last_modified)
      return(date_only)
    } else {
      return("Date not found.")
    }
  }, error = function(e) {
    return("Connection failed")
  })
}

get_last_modified_cdc<-function(url){ #for CDC websites
  tryCatch({
    page<-read_html(url)
    
    #extracts the specific <time> element with the known class
    node<-page %>% html_nodes("time.cdc-page-title-bar__item--date") %>% html_text(trim=TRUE)
    
    if(length(node)>0){
      return(node)
    } else {
      return("Date not found.")
    }
  }, error = function(e) {
    return("Connection failed")
  })
}

# Function to get file modification date
get_file_modification_date<-function(filename){
  if(file.exists(filename)){
    file_info <- file.info(filename)
    mod_date <- file_info$mtime
    return(format(mod_date, "%B %d, %Y"))
  } else {
    return("File not found")
  }
}

#URLs for USDA & CDC data sources
sources<-list(
  #USDA_wildbirds=list(url="https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/wild-birds", 
  #                     fetch_function=get_last_modified_usda),
  USDA_mammals=list(url="https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/mammals", 
                       fetch_function=get_last_modified_usda) #This one is used for cats as well
  #CDC_poultry=list(url="https://www.cdc.gov/bird-flu/situation-summary/data-map-commercial.html", 
  #                   fetch_function=get_last_modified_cdc),
  #CDC_poultry_new=list(url="https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/commercial-backyard-flocks", 
  #                     fetch_function=get_last_modified_cdc),
  #USDA_cattle=list(url="https://www.aphis.usda.gov/livestock-poultry-disease/avian/avian-influenza/hpai-detections/hpai-confirmed-cases-livestock", 
  #                   fetch_function=get_last_modified_usda)
  #CDC_humans=list(url="https://www.cdc.gov/bird-flu/situation-summary/index.html?cove-tab=1", 
  #                   fetch_function=get_last_modified_cdc)
)

#extracts last modified dates for all sources
last_modified_dates<-lapply(sources, function(source){
  tryCatch({
    source$fetch_function(source$url)
  }, error = function(e) {
    return("Connection failed")
  })
})

#adjust update dates manually (except poultry data)
humans_date<- as.character("March 16, 2025")
poultry_date<- as.character("August 15 2025")
cattle_date<- as.character("August 15, 2025")
cats_date<- as.character("August 15, 2025")
wildbirds_date<- as.character("August 15, 2025")
wildmammals_date<- as.character("August 15, 2025")

# or adjust automatically
#humans_date<- get_file_modification_date("Human_Data.csv")
#poultry_date<- get_file_modification_date("commercial-backyard-flocks.csv")  
#cattle_date<- get_file_modification_date("Table Details by Date.csv")
#cats_date<- get_file_modification_date("HPAI Detections in Mammals.csv")  # Cats data is in the mammals file
#wildbirds_date<- get_file_modification_date("HPAI Detections in Wild Birds.csv")
#wildmammals_date<- get_file_modification_date("HPAI Detections in Mammals.csv")

#####================================== WILD BIRD DATA PREPARATION ========================================#####
#Data is separated by State and County (when data allows), but right now (May 17, 2025) only 
#State-level data is being described with metrics and plots.

#State-level
hpai_data_wildbirds<-hpai_data_wildbirds %>% #column "Bird Species" is renamed to "Species"
  rename(Species='Bird Species')

#for those rows that have "null" as values in the "Collection Date" column, we are replacing these "nulls" with "Date Detected"
hpai_data_wildbirds <- hpai_data_wildbirds %>%
  mutate(`Collection Date` = ifelse(`Collection Date` == "null", `Date Detected`, `Collection Date`))

hpai_data_wildbirds_map<-hpai_data_wildbirds %>% #for maps
  mutate(`Collection Date`=mdy(`Collection Date`)) #for maps
  
hpai_data_wildbirds_map<-hpai_data_wildbirds_map %>%
  mutate(`Date Detected`=mdy(`Date Detected`))

hpai_data_wildbirds_metrics<-hpai_data_wildbirds
hpai_data_wildbirds_cum<-hpai_data_wildbirds

#State-level
state_cases_wildbirds<-hpai_data_wildbirds %>%
  count(State) %>% #count wildbirds cases per State
  rename(NAME=State, cases=n) %>% #state column is renamed to NAME and the count column to cases
  mutate(NAME=tolower(NAME)) #state names are transformed to lower case to make them consistent

#County-level
county_data_wildbirds<-hpai_data_wildbirds %>% 
  group_by(State, County) %>% #cases in wildbirds are grouped by state and county
  summarise(cases=n(), .groups="drop") %>% #counts the number of cases for each county state ("n()") combination (this is needed because some counties in different states have the same name)
  mutate(County=tolower(County), #name of counties are stored in lowercase
         State=tolower(State)) #name of states are stored in lowercase

#####================================== WILD MAMMAL DATA PREPARATION ========================================#####
#State-level
state_cases_mammals<-hpai_data_mammals_mammals %>%
  count(State) %>% #count mammals cases per State
  rename(NAME=State, cases=n) %>% #state column is renamed to NAME and the count column to cases
  mutate(NAME=tolower(NAME)) #state names are transformed to lower case to make them consistent

#County-level
county_data_mammals<-hpai_data_mammals_mammals %>% 
  group_by(State, County) %>% #cases in mammals are grouped by state and county
  summarise(cases=n(), .groups="drop") %>% #counts the number of cases for each county state ("n()") combination (this is needed because some counties in different states have the same name)
  mutate(County=tolower(County), #name of counties are stored in lowercase
         State=tolower(State)) #name of states are stored in lowercase


#####================================== CAT DATA PREPARATION ========================================######
#State-level
state_cases_cat<-hpai_data_mammals_cats %>% 
  count(State) %>% #counts cases in cats by state
  rename(NAME=State, cases=n) %>% #renames the State variable to "NAME"
  mutate(NAME=tolower(NAME)) #all states are rewritten in lowercase

hpai_data_mammals_cats_cum <- hpai_data_mammals_cats

# Group by Species and State and count occurrences
species_state_counts<-hpai_data_wildbirds %>%
  group_by(Species, State) %>%
  tally()  # Count occurrences for each species in each state

# Find the maximum number of cases for a single species in any state
max_cases_single_species_state<-species_state_counts %>%
  group_by(Species) %>%  # Group by Species to get the max within each species
  summarise(max_cases=max(n)) %>%  # Find max cases for each species across all states
  summarise(max_cases_overall=max(max_cases))  # Find the overall max value for any species
max_cases_single_species_state$max_cases_overall


#####================================== POULTRY DATA PREPARATION ========================================######
#State-level
state_cases_poultry<-hpai_data_poultry %>% 
  count(State, wt=FlockSize, name="cases") %>% #counts the number of poultry birds affected by state (wt sums the values in "FlockSize" for each State, since each row in the dataset contains information from a single flock)
  mutate(NAME=tolower(State)) #States are named in lowercase

hpai_data_poultry_cum <- hpai_data_poultry

#County-level
county_cases_poultry<-hpai_data_poultry %>%
  group_by(State, County) %>% #cases in poultry are grouped by state and County
  summarise(cases=sum(FlockSize, na.rm=TRUE), .groups="drop") %>% #cases from the same County are summed together
  mutate(County=tolower(County), State=tolower(State)) #County-level and States are mentioned in lowercase

#####================================== CATTLE DATA PREPARATION ========================================#####
#State-level
state_cases_cattle<-hpai_data_cattle %>%
  count(State) %>% #count cattle cases per State
  rename(NAME=State, cases=n) %>% #state column is renamed to NAME and the count column to cases
  mutate(NAME=tolower(NAME)) #state names are transformed to lower case to make them consistent

#####================================== HUMAN DATA PREPARATION ========================================#####
#State-level
state_cases_humans<-hpai_data_humans %>% 
  count(State, wt=NumberOfCases, name="cases") %>% #counts the number of humans birds affected by state (wt sums the values in "NumberOfCase" for each State, since each row in the dataset contains information from a single flock)
  mutate(NAME=tolower(State)) #States are named in lowercase

###Data for maps
us_states<-st_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json") %>%
  st_transform(crs=4326) 
