#####================================== SUMMARY METRICS =====================================
#These boxes contain summary information (e.g. first reported case, most affected state, etc.)

#Humans
output$HumanCases<-renderUI({ #creates a dynamic UI element for displaying human case counts
  case_data<-if(input$case_filter=="Last 15 days"){ #checks if the user selected the last 15-day filter
    hpai_data_humans_bar%>% #filters dataset to include only recent confirmations
      filter(lubridate::mdy(Date_confirmed)>=Sys.Date()-15)
  }else{
    hpai_data_humans_bar #uses full dataset when no time filter is applied
  }
  
  total_cases<-sum(case_data$NumberOfCases, na.rm=TRUE) #calculates the total number of cases
  
  if(total_cases>0){ #displays count if there is at least one case
    case_text<-ifelse(total_cases==1, "case", "cases") #handles singular or plural wording
    HTML(paste0(
      total_cases, " ", case_text #outputs the formatted text
    )) 
  }else{ #shows a no-cases message when total is zero
    HTML(
      "No cases reported")
  }
})

#Poultry
output$poultryCases<-renderUI({
  case_data<-if(input$case_filter=="Last 15 days"){
    hpai_data_poultry%>%
      filter(as.Date(`Outbreak Date`, format="%m-%d-%Y")>=Sys.Date()-15)
  }else{
    hpai_data_poultry
  }
  
  total_cases<-nrow(case_data)
  
  if(total_cases>0){
    case_text<-ifelse(total_cases==1, "case", "cases")
    HTML(paste0(scales::comma(total_cases), " ", case_text 
    ))
  }else{
    HTML("No cases reported")
  }
})

#Cattle
output$cattleCases<-renderUI({
  case_data<-if(input$case_filter=="Last 15 days"){
    hpai_data_cattle%>%
      filter(lubridate::dmy(Date)>=Sys.Date()-15)
  }else{
    hpai_data_cattle
  }
  
  total_cases<-nrow(case_data)
  
  if(total_cases>0){
    case_text<-ifelse(total_cases==1, "case", "cases")
    HTML(paste0(scales::comma(total_cases), " ", case_text 
    ))
  }else{
    HTML("No cases reported")
  }
})

#Domestic cats
output$catCases<-renderUI({
  case_data<-if(input$case_filter=="Last 15 days"){
    hpai_data_mammals_cats%>%
      filter(lubridate::mdy(`Date Collected`)>=Sys.Date()-15)
  }else{
    hpai_data_mammals_cats
  }
  
  total_cases<-nrow(case_data)
  
  if(total_cases>0){
    case_text<-ifelse(total_cases==1, "case", "cases")
    HTML(paste0(scales::comma(total_cases), " ", case_text 
    ))
  }else{
    HTML("No cases reported")
  }
})

#Wild birds
output$wildbirdCases<-renderUI({
  case_data<-if(input$case_filter=="Last 15 days"){
    hpai_data_wildbirds_metrics%>%
      filter(lubridate::mdy(`Collection Date`)>=Sys.Date()-15)
  }else{
    hpai_data_wildbirds_metrics
  }
  
  total_cases<-nrow(case_data)
  
  if(total_cases>0){
    case_text<-ifelse(total_cases==1, "case", "cases")
    HTML(paste0(scales::comma(total_cases), " ", case_text 
    ))
  }else{
    HTML("No cases reported")
  }
})

#Wild mammals
output$mammalCases<-renderUI({
  case_data<-if(input$case_filter=="Last 15 days"){
    hpai_data_mammals_mammals%>%
      filter(lubridate::mdy(`Date Collected`)>=Sys.Date()-15)
  }else{
    hpai_data_mammals_mammals
  }
  
  total_cases<-nrow(case_data)
  
  if(total_cases>0){
    case_text<-ifelse(total_cases==1, "case", "cases")
    HTML(paste0(scales::comma(total_cases), " ", case_text 
    ))
  }else{
    HTML("No cases reported")
  }
})

############### individual metric boxes for each species #############
output$firstCase<-renderUI({ #creates a dynamic UI element for displaying the first reported case
  first_case<-hpai_data_humans_bar %>%
    arrange(lubridate::mdy(Date_confirmed)) %>% #sorts data by confirmation date in ascending order
    slice_head(n=1) #selects the earliest (first) case
  
  if(nrow(first_case)>0){ #checks if at least one case exists
    first_date<-format(lubridate::mdy(first_case$Date_confirmed), "%b %d, %Y") #formats the confirmation date
    first_state<-first_case$State #extracts the state where the case occurred
    
    HTML(paste0(
      "Colorado", " - ", "Apr 28, 2022") #displays a hardcoded message (ignores extracted values)
    )
  }else{
    HTML(
      "Colorado", " - ", "Apr 28, 2022") #fallback display if no case exists (also hardcoded)
  }
})

output$latestCase<-renderUI({
  latest_case<-hpai_data_humans_bar %>%
    arrange(desc(lubridate::mdy(Date_confirmed))) %>%
    slice_head(n=1)
  
  if(nrow(latest_case)>0){
    latest_date<-format(lubridate::mdy(latest_case$Date_confirmed), "%b %d, %Y")
    latest_state<-latest_case$State
    
    HTML(paste0(
      latest_state, " - ", latest_date,""
    ))
  }else{
    HTML(
      "No recent reported cases")
  }
})

output$totalCases<-renderUI({
  total_cases<-sum(hpai_data_humans_bar$NumberOfCases, na.rm=TRUE)
  
  HTML(paste0(
     scales::comma(total_cases), " reported cases"
  ))
})

output$mostAffectedState<-renderUI({
  most_affected<-hpai_data_humans_bar %>%
    group_by(State) %>%
    summarise(total_cases=sum(NumberOfCases, na.rm=TRUE)) %>%
    arrange(desc(total_cases)) %>%
    slice_head(n=1)
  
  if(nrow(most_affected)>0){
    state_abbreviation<-most_affected$State
    HTML(paste0(
      state_abbreviation, " (", format(most_affected$total_cases, big.mark=","), " cases)"
    ))
  }else{
    HTML("No data available")
  }
})

###Summary Poultry box
output$firstCase_poultry<-renderUI({ #renders ui element showing first poultry outbreak
  first_case<-hpai_data_poultry %>%
    arrange(as.Date(`Outbreak Date`, format="%m-%d-%Y")) %>% #sorts outbreak data by date in ascending order
    slice_head(n=1) #selects the earliest outbreak record
  
  if(nrow(first_case)>0){ #checks if at least one record exists
    first_date<-format(as.Date(first_case$`Outbreak Date`, format="%m-%d-%Y"), "%b %d, %Y") #formats the outbreak date
    first_state<-first_case$State #extracts the state name
    
    HTML(paste0(
      first_state, " - ", first_date  #displays state and date of the first outbreak
    ))
  }else{
    HTML(
      "No reported cases") #fallback message if no data is available
  }
})

output$latestCase_poultry<-renderUI({
  latest_case<-hpai_data_poultry %>%
    arrange(desc(as.Date(`Outbreak Date`, format="%m-%d-%Y"))) %>%  
    slice_head(n=1)  
  
  if(nrow(latest_case)>0){
    latest_date<-format(as.Date(latest_case$`Outbreak Date`, format="%m-%d-%Y"), "%b %d, %Y")
    latest_state<-latest_case$State
    
    HTML(paste0(
      latest_state, " - ", latest_date, ""
    ))
  }else{
    HTML(
      "No recent reported cases")
  }
})

output$totalCase_poultry<-renderUI({
  total_cases<-nrow(hpai_data_poultry)
  
  HTML(paste0(
    format(total_cases, big.mark=","), " reported farms"
  ))
})

output$mostAffectedState_poultry<-renderUI({ #renders ui element for state with the largest affected flock
  most_affected<-hpai_data_poultry%>%
    group_by(State)%>% #groups dataset by state
    summarise(total_birds=sum(FlockSize, na.rm = TRUE)) %>% #calculates total birds affected per state
    arrange(desc(total_birds)) %>% #sorts states by descending bird count
    slice_head(n=1) #keeps the state with the highest count
  
  format_number<-function(num){ #formats large numbers into k/m notation
    if(num >= 1e6) {
      paste0(round(num/1e6, 1), "M")  
    }else if(num >= 1e3) {
      paste0(round(num/1e3, 0), "K")  
    }else{
      as.character(num) 
    }
  }
  
  if(nrow(most_affected)>0){ #checks if data exists
    state_name<-most_affected$State #extracts state name
    total_birds<-format_number(most_affected$total_birds) #formats bird count
    
    HTML(paste0(
      state_name, " (~", total_birds, " birds)" #displays state and bird count
    ))
  }else{
    HTML("No data available") #fallback when dataset is empty
  }
})

###Summary cattle box
output$firstCase_cattle<-renderUI({
  first_case<-hpai_data_cattle %>%
    mutate(Date=lubridate::dmy(Date)) %>%
    arrange(Date) %>% 
    slice_head(n=1)
  
  if(nrow(first_case)>0){
    first_date<-format(first_case$Date, "%b %d, %Y")
    first_state<-first_case$State
    
    HTML(paste0(
      first_state, " - ", first_date
    ))
  }else{
    HTML(
      "No reported cases")
  }
})

output$latestCase_cattle<-renderUI({
  latest_case<-hpai_data_cattle %>%
    mutate(Date=lubridate::dmy(Date)) %>%
    arrange(desc(Date)) %>%
    slice_head(n=1)
  
  if(nrow(latest_case)>0){
    latest_date<-format(latest_case$Date, "%b %d, %Y")
    latest_state<-latest_case$State
    
    HTML(paste0(
      latest_state, " - ", latest_date, ""
    ))
  }else{
    HTML(
      "No recent reported cases")
  }
})

output$totalCase_cattle<-renderUI({
  total_cases<-nrow(hpai_data_cattle)
  
  HTML(paste0(
     format(total_cases, big.mark=","), " reported cases"
  ))
})

output$mostAffectedState_cattle<-renderUI({
  most_affected<-hpai_data_cattle %>%
    count(State, name="total_cases") %>%  
    arrange(desc(total_cases)) %>% 
    slice_head(n=1)
  
  if(nrow(most_affected)>0){
    state_name<-most_affected$State
    HTML(paste0(
       state_name, " (", format(most_affected$total_cases, big.mark=","), " farms)"
    ))
  }else{
    HTML("No data available")
  }
})

###Summary cats
output$firstCase_cats<-renderUI({
  first_case<-hpai_data_mammals_cats %>%
    mutate(`Date Collected`=lubridate::mdy(`Date Collected`)) %>%
    arrange(`Date Collected`) %>%
    slice_head(n=1)
  
  if(nrow(first_case)>0){
    first_date<-format(first_case$`Date Collected`, "%b %d, %Y")
    first_state<-first_case$State
    
    HTML(paste0(
       first_state, " - ", first_date
    ))
  }else{
    HTML(
      "No reported cases")
  }
})

output$latestCase_cats<-renderUI({
  latest_case<-hpai_data_mammals_cats %>%
    mutate(`Date Collected`=lubridate::mdy(`Date Collected`)) %>%
    arrange(desc(`Date Collected`)) %>%
    slice_head(n=1)
  
  if(nrow(latest_case)>0){
    latest_date<-format(latest_case$`Date Collected`, "%b %d, %Y")
    latest_state<-latest_case$State
    
    HTML(paste0(
      latest_state, " - ", latest_date, ""
    ))
  }else{
    HTML(
      "No recent reported cases")
  }
})

output$totalCase_cats<-renderUI({
  total_cases<-nrow(hpai_data_mammals_cats)
  
  HTML(paste0(
    format(total_cases, big.mark=","), " reported cases"
  ))
})

output$mostAffectedState_cats<-renderUI({
  most_affected<-hpai_data_mammals_cats %>%
    count(State, name="total_cases") %>%
    arrange(desc(total_cases)) %>%
    slice_head(n=1)
  
  if(nrow(most_affected)>0){
    state_name<-most_affected$State
    HTML(paste0(
      state_name, " (", format(most_affected$total_cases, big.mark=","), " cases)"
    ))
  }else{
    HTML("No data available")
  }
})

###Summary wildbirds
hpai_data_wildbirds_summary<-hpai_data_wildbirds

output$firstCase_wildbirds<-renderUI({
  first_case<-hpai_data_wildbirds_summary %>%
    mutate(`Collection Date`=lubridate::mdy(`Collection Date`)) %>%
    arrange(`Collection Date`) %>%
    slice_head(n=1)
  
  if(nrow(first_case)>0){
    first_date<-format(first_case$`Collection Date`, "%b %d, %Y")
    first_state<-first_case$State
    
    HTML(paste0(
       first_state, " - ", first_date
    ))
  }else{
    HTML(
      "No reported cases")
  }
})

output$latestCase_wildbirds<-renderUI({
  latest_case<-hpai_data_wildbirds_summary %>%
    mutate(`Collection Date`=lubridate::mdy(`Collection Date`)) %>%
    arrange(desc(`Collection Date`)) %>%
    slice_head(n=1)
  
  if(nrow(latest_case)>0){
    latest_date<-format(latest_case$`Collection Date`, "%b %d, %Y")
    latest_state<-latest_case$State
    
    HTML(paste0(
      latest_state, " - ", latest_date, ""
    ))
  }else{
    HTML(
     "No recent reported cases")
  }
})

output$totalCase_wildbirds<-renderUI({
  total_cases<-nrow(hpai_data_wildbirds_summary)
  
  HTML(paste0(
     format(total_cases, big.mark=","), " reported cases"
  ))
})

output$mostAffectedState_wildbirds<-renderUI({
  most_affected<-hpai_data_wildbirds_summary %>%
    count(State, name="total_cases") %>%
    arrange(desc(total_cases)) %>%
    slice_head(n=1)
  
  if(nrow(most_affected)>0){
    state_name<-most_affected$State
    HTML(paste0(
     state_name, " (", format(most_affected$total_cases, big.mark=","), " cases)"
    ))
  }else{
    HTML("No data available")
  }
})



###Summary mammals
output$firstCase_mammals<-renderUI({
  first_case<-hpai_data_mammals_mammals %>%
    mutate(`Date Collected`=lubridate::mdy(`Date Collected`)) %>%
    arrange(`Date Collected`) %>%
    slice_head(n=1)
  
  if(nrow(first_case)>0){
    first_date<-format(first_case$`Date Collected`, "%b %d, %Y")
    first_state<-first_case$State
    
    HTML(paste0(
      first_state, " - ", first_date
    ))
  }else{
    HTML(
      "No reported cases")
  }
})

output$latestCase_mammals<-renderUI({
  latest_case<-hpai_data_mammals_mammals %>%
    mutate(`Date Collected`=lubridate::mdy(`Date Collected`)) %>%
    arrange(desc(`Date Collected`)) %>%
    slice_head(n=1)
  
  if(nrow(latest_case)>0){
    latest_date<-format(latest_case$`Date Collected`, "%b %d, %Y")
    latest_state<-latest_case$State
    
    HTML(paste0(
      latest_state, " - ", latest_date, ""
    ))
  }else{
    HTML(
      "No recent reported cases")
  }
})

output$totalCase_mammals<-renderUI({
  total_cases<-nrow(hpai_data_mammals_mammals)
  
  HTML(paste0(
    format(total_cases, big.mark=","), " reported cases"
  ))
})

output$mostAffectedState_mammals<-renderUI({
  most_affected<-hpai_data_mammals_mammals %>%
    count(State, name="total_cases") %>% 
    arrange(desc(total_cases)) %>%
    slice_head(n=1)
  
  if(nrow(most_affected)>0){
    state_name<-most_affected$State
    HTML(paste0(
      state_name, " (", format(most_affected$total_cases, big.mark=","), " cases)"
    ))
  }else{
    HTML("No data available")
  }
})