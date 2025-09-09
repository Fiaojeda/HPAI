#####================================== WELCOME WINDOW =====================================
#This piece of code is to generate the initial message when you open the dashboard

# Show welcome message only once using localStorage
observe({
  # Check if welcome message has been shown before using JavaScript
  runjs("
    if (!localStorage.getItem('welcomeShown')) {
      localStorage.setItem('welcomeShown', 'true');
      Shiny.setInputValue('showWelcome', Math.random());
    }
  ")
})

observeEvent(input$showWelcome, {
  showModal(modalDialog(
    title=div(style="font-size: clamp(14px, 4vw, 24px) !important; font-weight: bold; font-family: Arial;", "Welcome"),
    HTML("
      <div style='font-size: clamp(12px, 4vw, 18px) !important; font-family: Arial; line-height: 1.5;'>
        <p>Welcome!</p>
        
        <p>This dashboard presents reported cases at the <b>individual level</b> for humans, poultry, cats, wild birds, and mammals. Farm-level data is available for poultry and dairy cattle.</p>
        
        <p><b>Summary boxes</b> at the top display the overall reported and latest reported cases.</p>
        
        <p>Choose the population of interest to explore the temporal reporting of cases using the timeline slider. The slider begins on the year of the case reported for the selected population.</p>
        
        <p><b>Note:</b> The datasets used for this dashboard are made transparent and available for download in the <b>About</b> tab at the top of the screen.</p>
      </div>
    "),
    easyClose=FALSE,
    size="l", 
    footer=tagList(
      modalButton("Close")
    )
  ))
})