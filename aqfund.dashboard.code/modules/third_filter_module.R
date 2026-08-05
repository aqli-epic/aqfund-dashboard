
mod_filter_gis_ui <- function(id, data) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(
        3,
        
        pickerInput(
          inputId = ns("country_gis"),
          label = "Select Country",
          choices = unique(data$name0),
          multiple = F,
          
          options = list(
            `actions-box` = TRUE,
            size = 6,
            title = "Select Country",
            `selected-text-format` = "count > 2",
            `count-selected-text` = "{0} Country Selected",
            `none-selected-text` = "Country Not Found",
            `select-all-text` = "All",
            `live-search` = TRUE,
            liveSearchPlaceholder = "Country"
          ),
          
          selected = unique(data$name0)[1]
        )),
        
      column(
        3,  
        pickerInput(
          inputId = ns("state_gis"),
          label = "Select State(s):",
          choices = "",# unique(data$name1),
          multiple = F,
          
          options = list(
            `actions-box` = TRUE,
            size = 6,
            title = "Select State",
            `selected-text-format` = "count > 2",
            `count-selected-text` = "{0} State Selected",
            `none-selected-text` = "State Not Found",
            `select-all-text` = "All",
            `live-search` = TRUE,
            liveSearchPlaceholder = "State"
          ),
          
          selected = "", #unique(data$name1)[1]
        )),
        
      column(
        3,
        pickerInput(
          inputId = ns("district_gis"),
          label = "Select District(s):",
          choices = "",#unique(data$name2),
          multiple = F,
          
          options = list(
            `actions-box` = TRUE,
            size = 6,
            title = "Select District",
            `selected-text-format` = "count > 2",
            `count-selected-text` = "{0} District Selected",
            `none-selected-text` = "District Not Found",
            `select-all-text` = "All",
            `live-search` = TRUE,
            liveSearchPlaceholder = "District"
          ),
          
          selected = "", #unique(data$name2)[1]
        )
      ),
      
      column(
        3,
        
        pickerInput(
          inputId = ns("year_gis"),
          label = "Select Year",
          choices = NULL,
          multiple = TRUE,
          selected = NULL,
          
          options = list(
            `actions-box` = TRUE,
            size = 6,
            title = "Select Year",
            `selected-text-format` = "count > 2",
            `count-selected-text` = "{0} Year Selected",
            `none-selected-text` = "Year Not Found",
            `select-all-text` = "All",
            `live-search` = TRUE,
            liveSearchPlaceholder = "Year"
          )
        )
      )
    )
  )
}

# mod_filter_server <- function(id, data){
# 
#   moduleServer(id, function(input, output, session){
# 
#     # Update years when country changes
#     observeEvent(input$country, {
# 
#       yrs <- unique(
#         data[country %in% input$country, year]
#       )
# 
#       updatePickerInput(
#         session,
#         inputId = "year",
#         choices = yrs,
#         selected = yrs
#       )
# 
#     }, ignoreInit = FALSE)
# 
# 
#     filtered_data <- reactive({
# 
#       req(input$country, input$year)
# 
#       data[
#         country %in% input$country &
#           year %in% input$year
#       ]
#     })
# 
# 
#     observe({
#       print(filtered_data())
#     })
# 
# 
#     return(filtered_data)
# 
#   })
# }

mod_filter_gis_server <- function(id, data){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    rv_country_gis      <- reactiveValues()
    rv_state_gis        <- reactiveValues()
    rv_district_gis     <- reactiveValues()
    rv_year_gis             <- reactiveValues()
    
    
    
    isolate({
      rv_country_gis$country_gis       <- input$country_gis
      rv_state_gis$state_gis           <- input$state_gis
      rv_district_gis$district_gis     <- input$district_gis
      rv_year_gis$year                 <- input$year
    })
    
    observeEvent(rv_country_gis$country_gis, {
      updatePickerInput(session,
                        inputId = "state_gis",
                        label = "Select State(s):",
                        choices  = unique(sort(data[name0 %in% rv_country_gis$country_gis, name1])),
                        selected = unique(sort(data[name0 %in% rv_country_gis$country_gis, name1])))
    })
    

    observeEvent(rv_state_gis$state_gis, {
      updatePickerInput(session,
                        inputId = "district_gis",
                        label = "Select district(s):",
                        choices  = unique(sort(data[name1 %in% rv_state_gis$state_gis, name2])),
                        selected = unique(sort(data[name1 %in% rv_state_gis$state_gis, name2])))
    })
    
    
    observeEvent(rv_country_gis$country_gis, {
      updatePickerInput(session,
                        inputId = "year_gis",
                        label = "Select year(s):",
                        choices  = 2025:2026,#unique(sort(data[country %in% input$country, year])),
                        selected = 2025)#unique(sort(data[country %in% input$country, year]))[1])
    })
    
    
    
    observe({
      
      
      if(!isTRUE(input$country_gis_open) & !isTRUE(input$state_gis_open) & !isTRUE(input$district_gis_open) & !isTRUE(input$year_gis_open))
        
      {
        
        rv_country_gis$country_gis       <- input$country_gis
        rv_state_gis$state_gis           <- input$state_gis
        rv_district_gis$district_gis     <- input$district_gis
        rv_year_gis$year_gis                     <- input$year_gis
        
      }
      
    })
    
    
    
    

    


    

    
    
    # Change initial flag after first button click
    
  })
}

## To be copied in the UI
# mod_filter_ui("filter_1")

## To be copied in the server
# mod_filter_server("filter_1")
