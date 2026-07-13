mod_filter_trend_ui <- function(id, data) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(
        3,
        pickerInput(
          inputId = ns("country_t"),
          label = "Select Country",
          choices = sort(unique(data$country)),
          multiple = FALSE,
          selected = sort(unique(data$country))[1],
          options = list(
            `actions-box` = TRUE,
            size = 6,
            title = "Select Country",
            `none-selected-text` = "Country Not Found",
            `live-search` = TRUE,
            liveSearchPlaceholder = "Country"
          )
        )
      ),
      
      column(
        3,
        pickerInput(
          inputId = ns("year_t"),
          label = "Select Year",
          choices = NULL,
          multiple = TRUE,
          selected = NULL,
          options = list(
            `actions-box` = TRUE,
            size = 6,
            title = "Select Year",
            `selected-text-format` = "count > 2",
            `count-selected-text` = "{0} Years Selected",
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


mod_filter_trend_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$country_t, {
      
      req(input$country_t)
      
      year_choices <- data %>%
        dplyr::filter(country %in% input$country_t) %>%
        dplyr::filter(!is.na(year)) %>%
        dplyr::pull(year) %>%
        unique() %>%
        sort()
      
      updatePickerInput(
        session,
        inputId = "year_t",
        choices = 1998:2026,
        selected = 1998:2026
      )
      
    }, ignoreInit = FALSE)
    
    
    selected_country <- reactive({
      req(input$country_t)
      input$country_t
    })
    
    
    selected_year <- reactive({
      req(input$year_t)
      input$year_t
    })
    
    
    trend_data <- reactive({
      
      req(input$country_t, input$year_t)
      
      data %>%
        dplyr::filter(
          country %in% input$country_t,
          year %in% input$year_t
        )
    })
    
    filtered_data_t <<- reactive({
      
      # After first launch: only on button click
      req(input$country_t, input$year_t)
      # print(input$country)
      final_data[
        country %in% input$country_t &
          year %in% input$year_t
      ]
      
    })
    
    
    monthly_data_t <<- reactive({
      
      # After first launch: only on button click
      req(input$country_t, input$year_t)
      # print(input$country)
      monthly_value[
        country_name %in% input$country_t &
          year %in% input$year_t
      ]
      
    })
    
    return(list(
      country_sel = selected_country,
      year_sel = selected_year,
      trend_data_sel = trend_data
    ))
  })
}