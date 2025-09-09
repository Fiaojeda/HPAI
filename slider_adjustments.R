#####================================== SLIDER =====================================
#Since cases were reported in different years, having a single slider for the map will not work. This piece of code makes it so
#the slider is adjusted according to dataset. The slider will always start in the first day of the year where the initial
#case of HPAI was reported for a particular population (e.g. first case of HPAI in dairy cattle was March 25, 2024, so the slide
#starts in January 1, 2024).  
observe({ #reactively updates date slider to show most recent date
  latest_date <- max(hpai_data_humans_map$Date_confirmed, na.rm = TRUE)
  updateSliderInput(session, "date", value = latest_date) #sets slider value to latest confirmation date
})

observe({ #adjusts date slider range based on selected species
  req(input$species) #ensures species input exists
  
  if(input$species=="Humans" && !is.null(hpai_data_humans_map$Date_confirmed)){
    min_date<-min(hpai_data_humans_map$Date_confirmed, na.rm=TRUE)
    max_date<-max(hpai_data_humans_map$Date_confirmed, na.rm=TRUE)
  }else if(input$species=="Poultry (farms)" && !is.null(hpai_data_poultry_map$`Outbreak Date`)){
    min_date<-min(hpai_data_poultry_map$`Outbreak Date`, na.rm=TRUE)
    max_date<-max(hpai_data_poultry_map$`Outbreak Date`, na.rm=TRUE)
  }else if(input$species=="Poultry (birds)" && !is.null(hpai_data_poultry_map$`Outbreak Date`)){
    min_date<-min(hpai_data_poultry_map$`Outbreak Date`, na.rm=TRUE)
    max_date<-max(hpai_data_poultry_map$`Outbreak Date`, na.rm=TRUE)
  }else if(input$species=="Dairy cattle (farms)" && !is.null(hpai_data_cattle_map$`Date`)){
    min_date<-min(hpai_data_cattle_map$`Date`, na.rm=TRUE)
    max_date<-max(hpai_data_cattle_map$`Date`, na.rm=TRUE)
  }else if(input$species=="Domestic cats" && !is.null(hpai_data_mammals_cats_map$`Date Collected`)){
    min_date<-min(hpai_data_mammals_cats_map$`Date Collected`, na.rm=TRUE)
    max_date<-max(hpai_data_mammals_cats_map$`Date Collected`, na.rm=TRUE)
  }else if(input$species=="Wild mammals" && !is.null(hpai_data_mammals_mammals_map$`Date Collected`)){
    min_date<-min(hpai_data_mammals_mammals_map$`Date Collected`, na.rm=TRUE)
    max_date<-max(hpai_data_mammals_mammals_map$`Date Collected`, na.rm=TRUE)
  }else if(input$species=="Wild birds" && !is.null(hpai_data_wildbirds_map$`Collection Date`)){
    min_date<-min(hpai_data_wildbirds_map$`Collection Date`, na.rm=TRUE)
    max_date<-max(hpai_data_wildbirds_map$`Collection Date`, na.rm=TRUE)
  }else{
    return() #exits if species not recognized or data missing
  }
  
  if(!is.na(min_date) && !is.na(max_date) && min_date < max_date){ #updates slider only when valid range exists
    updateSliderInput(session, "date", 
                      min=min_date, 
                      max=max_date, 
                      value=max_date, #sets slider value to most recent date
                      timeFormat="%b %Y",
                      step=1)
  }
})