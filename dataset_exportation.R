#####================================== DATA EXPORTATION =====================================
#Data used to inform the dashboard is exported and made public for transparency. 
#This is currently in the "About" tab.

#creates a list of datasets used for download buttons
datasets <- list(
  humans = hpai_data_humans,
  poultry = hpai_data_poultry,
  cattle = hpai_data_cattle,
  cats = hpai_data_mammals_cats,
  wildbirds = hpai_data_wildbirds,
  mammals = hpai_data_mammals_mammals
)

#loops through each dataset and generates a corresponding downloadHandler
for(name in names(datasets)){
  local({
    dataset_name <- name
    output[[paste0("download_", dataset_name)]] <- downloadHandler(
      filename = function(){
        paste0("hpai_data_", dataset_name, ".csv")   #name of downloaded file
      },
      content = function(file){
        write.csv(datasets[[dataset_name]], file, row.names = FALSE)  #writes dataset to file
      }
    )
  })
}