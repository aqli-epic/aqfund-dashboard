server <- function(input, output, session) {
  # No sodium → use check_credentials directly
  
  
  # Simulate loading (e.g., loading data, models, etc.)
  Sys.sleep(3)
  
  # Hide the preloader after everything is ready
  waiter_hide()
  
  
  options(shiny.fullstacktrace = TRUE)
  
  
  observeEvent(input$tab_selected, {
    
    shinyjs::hide("index_section")
    shinyjs::hide("about_section")
    shinyjs::hide("impacts_section")
    shinyjs::hide("news_section")
    
    shinyjs::show(paste0(input$tab_selected, "_section"))
    
  })
  
  ###############################################################################
  #####################  
  
 # source("./main.R", local = TRUE)
  source("./gis.R", local = TRUE)
  source("./atglance.R", local = TRUE)
  source("./trend_server.R", local = TRUE)
  
  
  # source("./gbd_server.R", local = TRUE)
  # source("./factsheet_server.R", local = TRUE)
  # source("./aqli_comperison.R", local = TRUE)
  # source("./capital_city_server.R", local = TRUE)
  

  

  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials, passphrase = NULL)
  )
  
  
}