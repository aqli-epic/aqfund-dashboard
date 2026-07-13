
mod_filter_ui <- function(id, data) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(
        3,
        
        pickerInput(
          inputId = ns("country"),
          label = "Select Country",
          choices = unique(aqli_gadm0$country),
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
          
          selected = unique(aqli_gadm0$country)[1]
        )
      ),
      
      column(
        3,
        
        pickerInput(
          inputId = ns("year"),
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

mod_filter_server <- function(id, data){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    rv_country      <- reactiveValues()
    rv_year      <- reactiveValues()



    isolate({
      rv_country$country       <- input$country
      rv_year$year             <- input$year
    })



    observeEvent(rv_country$country, {
      updatePickerInput(session,
                        inputId = "year",
                        label = "Select year(s):",
                        choices  = 2025:2026,#unique(sort(data[country %in% input$country, year])),
                        selected = 2025)#unique(sort(data[country %in% input$country, year]))[1])
    })



    observe({


      if(!isTRUE(input$country_open) & !isTRUE(input$year_open))

      {

        rv_country$country <- input$country
        rv_year$year <- input$year

      }

    })




    filtered_data <<- reactive({

        # After first launch: only on button click
        req(input$country, input$year)
     # print(input$country)
        data[
          country %in% input$country &
           year %in% input$year
        ]

    })
    
    
    monthly_data <<- reactive({
      
      # After first launch: only on button click
      req(input$country, input$year)
      # print(input$country)
      monthly_value[
        country_name %in% input$country &
          year %in% input$year
      ]
      
    })
    
    map_city_filter <<- reactive({
      
      # After first launch: only on button click
      req(input$country)
      map_city[
        country_name %in% input$country &
         year %in% input$year
      ]
      
    })
    
    
    map_city_gis_filter <<- reactive({
      
      # After first launch: only on button click
      req(input$country)
      map_city_gis[
        country_name %in% input$country &
          year %in% input$year
      ]
      
    })
    
    

    # Change initial flag after first button click

  })
}

## To be copied in the UI
# mod_filter_ui("filter_1")

## To be copied in the server
# mod_filter_server("filter_1")
