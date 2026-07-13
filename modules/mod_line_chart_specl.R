# Line Chart UI
mod_line_sp_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    highchartOutput(ns("avg_aqi_id"), height = "450px")
    
  )
}

# Line Chart Server

mod_line_sp_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$avg_aqi_id <- renderHighchart({
      req(df())  # Check reactive input exists
      
      # Strong validation
      shiny::validate(
        need(nrow(df()) > 0, "There is no data to show! Kindly select something else!"),
        # need(x_col %in% names(df()), paste0("Column ", x_col, " not found in data!")),
        # need(all(y_cols %in% names(df())), "Some y-axis columns not found in data!")
      )
      
      data_df <- df()  # Store once
      
      # Create highchart
      # data_df <- final_data %>%
      #   filter(country %in% input$country) %>%
      #   arrange(year) %>%
      #   mutate(year = as.numeric(year))
      
      text <- paste0(
        "Annual <span style='color:maroon;'>",
        unique(data_df$country),
        "</span> PM<sub>2.5</sub> Concentration"
      )
      
      # print("--------------------------------")
      # print(data_df)
      # -------------------------
      # SPLIT DATASETS
      # -------------------------
      
      satellite_df <- data_df %>%
        filter(year >= 1998, year <= 2024)
      
      ground_df <- data_df %>%
        filter(year >= 2025, year <= 2026)
      
      # -------------------------
      # HIGHCHART
      # -------------------------
      
      highchart() %>%
        
        hc_chart(type = "line") %>%
        
        # ================= X AXIS =================
      
      
      hc_xAxis(
        type = "linear",
        title = list(text = "Year"),
        
        plotLines = list(
          
          # ================= POST-2024 =================
          list(
            color = "darkgrey", 
            width = 1.5,
            value = 2024,
            dashStyle = "Dash",
            zIndex = 5,
            
            label = list(
              text = "Post-2024:<br>Ground<br>monitoring",
              useHTML = TRUE,          
              rotation = 0,   # 🔥 forces horizontal text
              
              align = "left",
              y = 25,
              x = -27,
              style = list(
                color = "#ff7f0e",
               # fontWeight = "bold",
                fontSize = "10px"
              )
            )
          )
          
          # ================= PRE-2024 ANNOTATION =================
          # list(
          #   color = "transparent",
          #   width = 0,
          #   value = 2002,
          #   
          #   label = list(
          #     text = "1998–2024:<br>Satellite-Based PM2.5 Estimation <br> Excluding Sea Salt and Dust",
          #     useHTML = TRUE,
          #     
          #     rotation = 0,   # 🔥 forces horizontal text
          #     
          #     style = list(
          #       color = "#1f77b4",
          #      # fontWeight = "bold",
          #       fontSize = "10px",
          #       whiteSpace = "nowrap"   # prevents wrapping into vertical stacking
          #     ),
          #     
          #     y = 130,   # adjust vertical position
          #     x = 0
          #   )
          # ),
          
          
          # list(
          #   color = "transparent",
          #   width = 0,
          #   value = 2002,
          # 
          #   label = list(
          #     text = "1998–2024:<br>Satellite-Based PM2.5 Estimation <br>Including Sea Salt and Dust",
          #     useHTML = TRUE,
          # 
          #     rotation = 0,   # 🔥 forces horizontal text
          # 
          #     style = list(
          #       color = "maroon",
          #      # fontWeight = "bold",
          #       fontSize = "10px",
          #       whiteSpace = "nowrap"   # prevents wrapping into vertical stacking
          #     ),
          # 
          #     y = 30,   # adjust vertical position
          #     x = 0
          #   )
          # )
          
          
          )
      ) %>%     
        # ================= Y AXIS =================
      hc_yAxis(
        title = list(text = "PM2.5 (µg/m³)")
      ) %>%
        
        # ================= SERIES 1: SATELLITE =================
      hc_add_series(
        name = "Satellite Data Excluding Sea Salt and Dust (1998–2024)",
        data = lapply(1:nrow(satellite_df), function(i) {
          list(x = satellite_df$year[i], y = satellite_df$pm[i])
        }),
        type = "spline",
        color = "#1f77b4",
        lineWidth = 2
      ) %>%
        hc_add_series(
          name = "Satellite Data Including Sea Salt and Dust (1998–2024)",
          data = lapply(1:nrow(satellite_df), function(i) {
            list(x = satellite_df$year[i], y = satellite_df$pm_wth[i])
          }),
          type = "spline",
          color = "maroon",
          lineWidth = 2
        ) %>%
        
        # ================= SERIES 2: GROUND =================
      hc_add_series(
        name = "Ground Monitoring (2025–2026)",
        data = lapply(1:nrow(ground_df), function(i) {
          list(x = ground_df$year[i], y = ground_df$pm[i])
        }),
        type = "spline",
        color = "#ff7f0e",
        lineWidth = 2
      ) %>%
        
        # ================= NATIONAL STANDARD =================
      hc_add_series(
        name = "PM2.5 National Standard",
        data = lapply(1:nrow(data_df), function(i) {
          list(x = data_df$year[i], y = data_df$natstandard[i])
        }),
        type = "line",
        dashStyle = "ShortDash",
        color = "grey"
      ) %>%
        
        # ================= TITLE =================
      hc_title(text = text, useHTML = TRUE) %>%
        hc_subtitle(
          text = if (all(is.na(data_df$natstandard))) {
            
            "No National PM2.5 Standard"
            
          } else {
            
            paste0(
              "National Avg. PM2.5 Standard (",
              unique(na.omit(data_df$natstandard))[1],
              " µg/m³)"
            )
          }
        ) %>%     
        hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE
        )
    })
  })
}

## To be copied in the UI
# mod_line_ui("create_box_1", title = "Chart Title Here")

## To be copied in the server
# mod_line_server("create_box_1", df = your_data, x_col = "YourX", y_cols = c("YourY1", "YourY2"), chart_title = "Your Chart Title")
