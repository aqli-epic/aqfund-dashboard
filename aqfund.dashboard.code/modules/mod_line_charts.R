# ================================
# Generic Highchart Line Module UI
# ================================

mod_line_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    highchartOutput(ns("line_chart"), height = "440px")
  )
}


# ===================================
# Generic Highchart Line Module Server
# ===================================

mod_line_server <- function(
    id,
    df,
    x_col,
    y_col,
    group_col = NULL,
    chart_title = NULL,
    x_title = NULL,
    y_title = NULL,
    subtitle_text = NULL,
    chart_type = "spline"
) {
  
  moduleServer(id, function(input, output, session) {
    
    output$line_chart <- renderHighchart({
      
      req(df())
      
      # Validation
      shiny::validate(
        need(nrow(df()) > 0, "No data available!"),
        need(x_col %in% names(df()), paste0(x_col, " column not found!")),
        need(y_col %in% names(df()), paste0(y_col, " column not found!"))
      )
      
      data_df <- df()
      
      
      # Base chart
      hc <- highchart() %>%
        
        hc_chart(type = chart_type) %>%
        
        hc_title(
          text = chart_title,
          useHTML = TRUE
        ) %>%
        hc_subtitle(
          text = subtitle_text,
          useHTML = TRUE
        ) %>% 
        hc_xAxis(
          categories = unique(data_df[[x_col]]),
          title = list(
            text = x_title,
            style = list(fontWeight = "bold")
          )
        ) %>%
        
        hc_yAxis(
          min = 0,
          title = list(
            text = y_title,
            style = list(fontWeight = "bold")
          )
        ) %>% 
        
        hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE,
          valueDecimals = 2
        ) %>%
        
        hc_plotOptions(
          series = list(
            marker = list(enabled = TRUE),
            dataLabels = list(enabled = FALSE),
            showInLegend = TRUE
          )
        ) %>%
        
        hc_exporting(enabled = TRUE) %>%
        
        hc_legend(enabled = TRUE)
      
      
      # ==========================
      # If group column available
      # ==========================
      
      if (!is.null(group_col)) {
        
        shiny::validate(
          need(group_col %in% names(data_df),
               paste0(group_col, " column not found!"))
        )
        
        groups <- unique(data_df[[group_col]])
        
        for (grp in groups) {
          
          series_data <- data_df %>%
            filter(.data[[group_col]] == grp) %>%
            pull(.data[[y_col]])
          
          hc <- hc %>%
            hc_add_series(
              name = as.character(grp),
              data = series_data,
              type = chart_type
            )
        }
        
      } else {
        
        # Single series
        
        hc <- hc %>%
          hc_add_series(
            name = y_col,
            data = data_df[[y_col]],
            type = chart_type
          )
      }
      
      hc
      
    })
  })
}