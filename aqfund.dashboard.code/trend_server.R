


rv_country_trd      <- reactiveValues()
rv_state_trd        <- reactiveValues()
rv_district_trd     <- reactiveValues()
rv_year_trd         <- reactiveValues()



isolate({
  rv_country_trd$country_trd       <- input$country_trd
  rv_state_trd$state_trd           <- input$state_trd
  rv_district_trd$district_trd     <- input$district_trd
  rv_year_trd$year_trd                 <- input$year_trd
})

observeEvent(rv_country_trd$country_trd, {
  updatePickerInput(session,
                    inputId = "state_trd",
                    label = "Select State(s):",
                    choices  = unique(sort(sensor_data[name0 %in% rv_country_trd$country_trd, name1])),
                    selected = unique(sort(sensor_data[name0 %in% rv_country_trd$country_trd, name1])))
})


observeEvent(rv_state_trd$state_trd, {
  updatePickerInput(session,
                    inputId = "district_trd",
                    label = "Select district(s):",
                    choices  = unique(sort(sensor_data[name1 %in% rv_state_trd$state_trd, name2])),
                    selected = unique(sort(sensor_data[name1 %in% rv_state_trd$state_trd, name2])))
})


observeEvent(rv_country_trd$country_trd, {
  updatePickerInput(session,
                    inputId = "year_trd",
                    label = "Select year(s):",
                    choices  = 2025:2026,#unique(sort(sensor_data[country %in% input$country, year])),
                    selected = 2025:2026)#unique(sort(sensor_data[country %in% input$country, year]))[1])
})



observe({
  
  
  if(!isTRUE(input$country_trd_open) & !isTRUE(input$state_trd_open) & !isTRUE(input$district_trd_open) & !isTRUE(input$year_trd_open))
    
  {
    
    rv_country_trd$country_trd       <- input$country_trd
    rv_state_trd$state_trd           <- input$state_trd
    rv_district_trd$district_trd     <- input$district_trd
    rv_year_trd$year_trd             <- input$year_trd
    
  }
  
})



get_country_df <- reactive({
  

  openaq_month_trend_c[
    name0 %in% input$country_trd &
      # name1 %in% input$state_trd &
      # name2 %in% input$district_trd &
      year %in% input$year_trd
  ]
  
})

get_state_df <- reactive({
  

  openaq_month_trend_s[
    name0 %in% input$country_trd &
      name1 %in% input$state_trd &
      # name2 %in% input$district_trd &
      year %in% input$year_trd
  ]
  
})


get_district_df <- reactive({

  openaq_month_trend_d[
    name0 %in% input$country_trd &
      name1 %in% input$state_trd &
      name2 %in% input$district_trd &
      year %in% input$year_trd
  ]
  
})

has_value <- function(x) {
  !is.null(x) && length(x) > 0 && !is.na(x[1]) && nzchar(x[1])
}

get_country_df <- reactive({
  
  req(input$country_trd, input$year_trd)
  
  openaq_month_trend_c[
    name0 %in% input$country_trd &
      year %in% input$year_trd
  ]
})

get_state_df <- reactive({
  
  req(input$country_trd, input$state_trd, input$year_trd)
  
  openaq_month_trend_s[
    name0 %in% input$country_trd &
      name1 %in% input$state_trd &
      year %in% input$year_trd
  ]
})

get_district_df <- reactive({
  
  req(input$country_trd, input$state_trd, input$district_trd, input$year_trd)
  
  openaq_month_trend_d[
    name0 %in% input$country_trd &
      name1 %in% input$state_trd &
      name2 %in% input$district_trd &
      year %in% input$year_trd
  ]
})

get_country_aqli <- reactive({
  
  #req(input$country_trd, input$state_trd, input$year_trd)
  
  final_data_country_name2_lvl[
    name0 %in% input$country_trd 
    #  name1 %in% input$state_trd &
     # year %in% input$year_trd
  ]
})

get_state_aqli <- reactive({
  
 # req(input$country_trd, input$state_trd, input$year_trd)
  
  final_data_country_name2_lvl[
    name0 %in% input$country_trd &
      name1 %in% input$state_trd_aq
    #  year %in% input$year_trd
  ]
})

get_district_aqli <- reactive({
  
  #req(input$country_trd, input$state_trd, input$district_trd, input$year_trd)
  
  final_data_country_name2_lvl[
    name0 %in% input$country_trd &
      name1 %in% input$state_trd_aq &
      name2 %in% input$district_trd_aq 
    #  year %in% input$year_trd
  ]
})

df <- reactive({
  
  req(input$country_trd, input$year_trd)
  
  final_data_country_lvl[
    name0 %in% input$country_trd 
      # name1 %in% input$state_trd &
      # name2 %in% input$district_trd &
    #  year %in% input$year_trd
  ]
})


output$monthly_pm25_glm <- highcharter::renderHighchart({
  
  req(input$country_trd, input$year_trd)
  
  month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  
  state_selected <- isTRUE(input$use_state) &&
    has_value(input$state_trd)
  
  district_selected <- isTRUE(input$use_district_id) &&
    has_value(input$state_trd) &&
    has_value(input$district_trd)
  
  # ---------------------------------------------------------
  # DISTRICT LEVEL
  # ---------------------------------------------------------
  
  if (district_selected) {
    
    raw_df <- get_district_df()
    
    title_text <- paste0(
      "Monthly <span style='color:maroon;'>",
      input$country_trd,
      " → ",
      input$state_trd,
      " → ",
      input$district_trd,
      "</span> PM<sub>2.5</sub> Concentration"
    )
    
  } else if (state_selected) {
    
    # ---------------------------------------------------------
    # STATE LEVEL
    # ---------------------------------------------------------
    
    raw_df <- get_state_df()
    
    title_text <- paste0(
      "Monthly <span style='color:maroon;'>",
      input$country_trd,
      " → ",
      input$state_trd,
      "</span> PM<sub>2.5</sub> Concentration"
    )
    
  } else {
    
    # ---------------------------------------------------------
    # DEFAULT COUNTRY LEVEL
    # ---------------------------------------------------------
    
    raw_df <- get_country_df()

    title_text <- paste0(
      "Monthly <span style='color:maroon;'>",
      input$country_trd,
      "</span> PM<sub>2.5</sub> Concentration"
    )
  }
  
  shiny::validate(
    shiny::need(nrow(raw_df) > 0, "No monthly data available for selected filters.")
  )
  
  raw_df <- as.data.frame(raw_df)
  
  # ---------------------------------------------------------
  # Standardize PM column
  # ---------------------------------------------------------
  
  pm_col <- if ("pm25_avg" %in% names(raw_df)) {
    "pm25_avg"
  } else if ("pm25" %in% names(raw_df)) {
    "pm25"
  } else {
    stop("No PM2.5 column found. Expected `pm25_avg` or `pm25`.")
  }
  
  # ---------------------------------------------------------
  # Month order + complete missing months
  # ---------------------------------------------------------
  
  data_df <- raw_df %>%
    dplyr::mutate(
      month_name = dplyr::case_when(
        "month_name" %in% names(raw_df) ~ as.character(month_name),
        "month" %in% names(raw_df) & is.numeric(month) ~ month.abb[month],
        "month" %in% names(raw_df) ~ as.character(month),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::group_by(year, month_name) %>%
    dplyr::summarise(
      pm25_avg = mean(.data[[pm_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      month_name = factor(month_name, levels = month_levels)
    ) %>%
    dplyr::filter(!is.na(month_name)) %>%
    tidyr::complete(
      year,
      month_name = factor(month_levels, levels = month_levels),
      fill = list(pm25_avg = NA_real_)
    ) %>%
    dplyr::arrange(year, month_name)
  
  shiny::validate(
    shiny::need(nrow(data_df) > 0, "No monthly data available after processing.")
  )
  
  # ---------------------------------------------------------
  # Highchart base
  # ---------------------------------------------------------
  
  hc <- highcharter::highchart() %>%
    highcharter::hc_chart(
      type = "spline",
      backgroundColor = "transparent"
    ) %>%
    highcharter::hc_title(
      text = title_text,
      useHTML = TRUE
    ) %>%
    highcharter::hc_subtitle(
      text = "(Ground Monitoring Data)",
      useHTML = TRUE
    ) %>%
    highcharter::hc_xAxis(
      categories = month_levels,
      title = list(text = "Month")
    ) %>%
    highcharter::hc_yAxis(
      min = 0,
      title = list(text = "PM₂.₅ (µg/m³)")
    ) %>%
    highcharter::hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      valueDecimals = 1,
      valueSuffix = " µg/m³"
    ) %>%
    highcharter::hc_legend(
      enabled = TRUE
    ) %>%
    highcharter::hc_plotOptions(
      spline = list(
        marker = list(
          enabled = TRUE,
          radius = 3
        ),
        lineWidth = 2,
        connectNulls = FALSE
      )
    ) %>%
    highcharter::hc_credits(enabled = FALSE)
  
  # ---------------------------------------------------------
  # Add one line per year
  # ---------------------------------------------------------
  
  years <- sort(unique(data_df$year))
  
  for (yr in years) {
    
    yr_data <- data_df %>%
      dplyr::filter(year == yr) %>%
      dplyr::arrange(month_name)
    
    hc <- hc %>%
      highcharter::hc_add_series(
        name = as.character(yr),
        data = yr_data$pm25_avg,
        type = "spline"
      )
  }
  
  hc
})


output$avg_aqi_id <- renderHighchart({
 # req(df())  # Check reactive input exists
  
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
    unique(data_df$name0),
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
  # hc_add_series(
  #   name = "Satellite Data Excluding Sea Salt and Dust (1998–2024)",
  #   data = lapply(1:nrow(satellite_df), function(i) {
  #     list(x = satellite_df$year[i], y = satellite_df$pm_aqli[i])
  #   }),
  #   type = "spline",
  #   color = "#1f77b4",
  #   lineWidth = 2
  # ) %>%
    hc_add_series(
      name = "Satellite Data Including Sea Salt and Dust (1998–2024)",
      data = lapply(1:nrow(satellite_df), function(i) {
        list(x = satellite_df$year[i], y = satellite_df$pm_aqli[i])
      }),
      type = "spline",
      color = "maroon",
      lineWidth = 2
    ) %>%
    
    # ================= SERIES 2: GROUND =================
  hc_add_series(
    name = "Ground Monitoring (2025–2026)",
    data = lapply(1:nrow(ground_df), function(i) {
      list(x = ground_df$year[i], y = ground_df$pm25[i])
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



# =========================================================
# Annual PM2.5 Location Drilldown
# Country trend -> year states -> state districts
# =========================================================

# output$annual_pm25_location_drilldown <- highcharter::renderHighchart({
#   
#   req(input$country_trd)
#   
#   state_selected <- isTRUE(input$use_state_aq) &&
#     has_value(input$state_trd)
#   
#   district_selected <- isTRUE(input$use_district_id_aq) &&
#     has_value(input$state_trd_aq) &&
#     has_value(input$district_trd_aq)
#   
#   # ---------------------------------------------------------
#   # DISTRICT LEVEL
#   # ---------------------------------------------------------
#   
#   if (district_selected) {
#     
#     raw_df <- get_district_aqli()
#     
#     title_text <- paste0(
#       "Annual <span style='color:maroon;'>",
#       input$country_trd,
#       " → ",
#       input$state_trd_aq,
#       " → ",
#       input$district_trd_aq,
#       "</span> PM<sub>2.5</sub> Concentration"
#     )
#     
#   } else if (state_selected) {
#     
#     # ---------------------------------------------------------
#     # STATE LEVEL
#     # ---------------------------------------------------------
#     
#     raw_df <- get_state_aqli()
#     
#     title_text <- paste0(
#       "Annual <span style='color:maroon;'>",
#       input$country_trd,
#       " → ",
#       input$state_trd_aq,
#       "</span> PM<sub>2.5</sub> Concentration"
#     )
#     
#   } else {
#     
#     # ---------------------------------------------------------
#     # DEFAULT COUNTRY LEVEL
#     # ---------------------------------------------------------
#     
#     raw_df <- get_country_aqli()
#     print("raw_df")
#     print(raw_df)
#     title_text <- paste0(
#       "Annual <span style='color:maroon;'>",
#       input$country_trd,
#       "</span> PM<sub>2.5</sub> Concentration"
#     )
#   }
#   
#   shiny::validate(
#     shiny::need(nrow(raw_df) > 0, "No annual data available for selected filters.")
#   )
#   
#   raw_df <- as.data.frame(raw_df)
#   
#   # ---------------------------------------------------------
#   # Standardize PM column
#   # ---------------------------------------------------------
#   
#   pm_col <- if ("pm25_avg" %in% names(raw_df)) {
#     "pm25_avg"
#   } else if ("pm25" %in% names(raw_df)) {
#     "pm25"
#   } else {
#     stop("No PM2.5 column found. Expected `pm25_avg` or `pm25`.")
#   }
#   
#   # ---------------------------------------------------------
#   # Year-wise aggregation
#   # ---------------------------------------------------------
#   
#   data_df <- raw_df %>%
#     dplyr::filter(!is.na(year)) %>%
#     dplyr::group_by(year) %>%
#     dplyr::summarise(
#       pm25_avg = mean(.data[[pm_col]], na.rm = TRUE),
#       .groups = "drop"
#     ) %>%
#     dplyr::filter(!is.na(pm25_avg)) %>%
#     dplyr::arrange(year)
#   
#   shiny::validate(
#     shiny::need(nrow(data_df) > 0, "No annual data available after processing.")
#   )
#   
#   years <- data_df$year
#   
#   # ---------------------------------------------------------
#   # Highchart annual trend
#   # ---------------------------------------------------------
#   
#   highcharter::highchart() %>%
#     highcharter::hc_chart(
#       type = "spline",
#       backgroundColor = "transparent"
#     ) %>%
#     highcharter::hc_title(
#       text = title_text,
#       useHTML = TRUE
#     ) %>%
#     highcharter::hc_subtitle(
#       text = "(Ground Monitoring Data)",
#       useHTML = TRUE
#     ) %>%
#     highcharter::hc_xAxis(
#       categories = as.character(years),
#       title = list(text = "Year")
#     ) %>%
#     highcharter::hc_yAxis(
#       min = 0,
#       title = list(text = "PM₂.₅ (µg/m³)")
#     ) %>%
#     highcharter::hc_tooltip(
#       shared = TRUE,
#       crosshairs = TRUE,
#       valueDecimals = 1,
#       valueSuffix = " µg/m³"
#     ) %>%
#     highcharter::hc_legend(
#       enabled = FALSE
#     ) %>%
#     highcharter::hc_plotOptions(
#       spline = list(
#         marker = list(
#           enabled = TRUE,
#           radius = 4
#         ),
#         lineWidth = 2.5,
#         connectNulls = FALSE
#       )
#     ) %>%
#     highcharter::hc_add_series(
#       name = "Annual PM₂.₅",
#       data = data_df$pm25_avg,
#       type = "spline"
#     ) %>%
#     highcharter::hc_credits(enabled = FALSE)
#   
# })

# 
# trend_weighted_pm <- function(value, weight) {
#   keep <- is.finite(value)
# 
#   if (!any(keep)) {
#     return(NA_real_)
#   }
# 
#   value <- value[keep]
#   weight <- weight[keep]
#   valid_weight <- is.finite(weight) & weight > 0
# 
#   if (any(valid_weight)) {
#     return(round(stats::weighted.mean(
#       value[valid_weight],
#       weight[valid_weight]
#     ), 2))
#   }
# 
#   round(mean(value), 2)
# }
# 
# trend_annual_title <- function(location, level = NULL, year = NULL) {
#   location_html <- htmltools::htmlEscape(location)
# 
#   if (is.null(level)) {
#     return(paste0(
#       "Annual <span style='color:#800000;'>",
#       location_html,
#       "</span> PM<sub>2.5</sub> Concentration"
#     ))
#   }
# 
#   paste0(
#     "<span style='color:#800000;'>",
#     location_html,
#     "</span> - ",
#     year,
#     " ",
#     level,
#     " PM<sub>2.5</sub> Concentration"
#   )
# }
# 
# output$annual_pm25_location_drilldown <- highcharter::renderHighchart({
#   req(input$country_trd)
# 
#   selected_country <- input$country_trd[[1]]
#   location_dt <- data.table::copy(
#     final_data_country_name2_lvl[
#       name0 == selected_country &
#         !is.na(name1) & trimws(name1) != "" &
#         !is.na(name2) & trimws(name2) != ""
#     ]
#   )
# 
#   location_dt[
#     ,
#     pm_value := data.table::fifelse(year <= 2024, pm_aqli, pm25)
#   ]
#   location_dt <- location_dt[is.finite(pm_value)]
# 
#   shiny::validate(
#     shiny::need(
#       nrow(location_dt) > 0,
#       "No annual PM2.5 data are available for this country."
#     )
#   )
# 
#   country_year <- location_dt[
#     ,
#     .(
#       pm_value = trend_weighted_pm(pm_value, tot),
#       natstandard = {
#         standards <- natstandard[is.finite(natstandard)]
#         if (length(standards)) standards[[1]] else NA_real_
#       }
#     ),
#     by = year
#   ][is.finite(pm_value)][order(year)]
# 
#   state_year <- location_dt[
#     ,
#     .(pm_value = trend_weighted_pm(pm_value, tot)),
#     by = .(year, state = name1)
#   ][is.finite(pm_value)]
# 
#   district_year <- location_dt[
#     ,
#     .(pm_value = trend_weighted_pm(pm_value, tot)),
#     by = .(year, state = name1, district = name2)
#   ][is.finite(pm_value)]
# 
#   shiny::validate(
#     shiny::need(
#       nrow(country_year) > 0,
#       "No annual PM2.5 values are available for this country."
#     )
#   )
# 
#   all_years <- country_year$year
#   root_title <- trend_annual_title(selected_country)
#   national_standard <- country_year$natstandard[
#     is.finite(country_year$natstandard)
#   ]
#   root_subtitle <- if (length(national_standard)) {
#     paste0(
#       "National Avg. PM2.5 Standard (",
#       national_standard[[1]],
#       " µg/m³)"
#     )
#   } else {
#     "No National PM2.5 Standard"
#   }
# 
#   source_subtitle <- function(year) {
#     if (year <= 2024) {
#       paste0("Satellite-based annual estimate - ", year)
#     } else {
#       paste0("Ground monitoring annual average - ", year)
#     }
#   }
# 
#   make_root_points <- function(data) {
#     lapply(seq_len(nrow(data)), function(i) {
#       selected_year <- data$year[[i]]
#       list(
#         name = as.character(selected_year),
#         x = match(selected_year, all_years) - 1,
#         y = data$pm_value[[i]],
#         drilldown = paste0("annual_location_year_", selected_year)
#       )
#     })
#   }
# 
#   satellite_points <- make_root_points(country_year[year <= 2024])
#   ground_points <- make_root_points(country_year[year >= 2025])
#   standard_points <- lapply(seq_len(nrow(country_year)), function(i) {
#     list(
#       name = as.character(country_year$year[[i]]),
#       x = i - 1,
#       y = country_year$natstandard[[i]]
#     )
#   })
# 
#   level_colors <- c(
#     "#800000", "#006666", "#D4AF37", "#3B82A0", "#C2413B"
#   )
#   drilldown_series <- list()
# 
#   for (selected_year in all_years) {
#     states <- state_year[year == selected_year][order(-pm_value, state)]
#     state_points <- vector("list", nrow(states))
# 
#     for (state_index in seq_len(nrow(states))) {
#       selected_state <- states$state[[state_index]]
#       district_id <- paste0(
#         "annual_location_year_",
#         selected_year,
#         "_state_",
#         state_index
#       )
#       districts <- district_year[
#         year == selected_year & state == selected_state
#       ][order(-pm_value, district)]
# 
#       state_points[[state_index]] <- list(
#         name = selected_state,
#         y = states$pm_value[[state_index]],
#         drilldown = if (nrow(districts)) district_id else NULL,
#         color = level_colors[
#           (state_index - 1) %% length(level_colors) + 1
#         ]
#       )
# 
#       if (nrow(districts)) {
#         district_points <- lapply(seq_len(nrow(districts)), function(i) {
#           list(
#             name = districts$district[[i]],
#             y = districts$pm_value[[i]],
#             color = level_colors[(i - 1) %% length(level_colors) + 1]
#           )
#         })
# 
#         drilldown_series[[length(drilldown_series) + 1]] <- list(
#           id = district_id,
#           name = selected_state,
#           type = "column",
#           showInLegend = FALSE,
#           data = district_points,
#           custom = list(
#             chartTitle = trend_annual_title(
#               paste(selected_country, selected_state, sep = " - "),
#               level = "District",
#               year = selected_year
#             ),
#             chartSubtitle = source_subtitle(selected_year),
#             axisTitle = "District"
#           )
#         )
#       }
#     }
# 
#     drilldown_series[[length(drilldown_series) + 1]] <- list(
#       id = paste0("annual_location_year_", selected_year),
#       name = paste(selected_country, selected_year, sep = " - "),
#       type = "column",
#       showInLegend = FALSE,
#       data = state_points,
#       custom = list(
#         chartTitle = trend_annual_title(
#           selected_country,
#           level = "State",
#           year = selected_year
#         ),
#         chartSubtitle = source_subtitle(selected_year),
#         axisTitle = "State"
#       )
#     )
#   }
# 
#   chart <- highcharter::highchart() %>%
#     highcharter::hc_add_dependency("modules/drilldown.js") %>%
#     highcharter::hc_chart(
#       type = "spline",
#       height = 590,
#       zoomType = "x",
#       spacingTop = 22,
#       spacingRight = 26,
#       spacingBottom = 24,
#       spacingLeft = 16,
#       animation = list(duration = 300),
#       style = list(fontFamily = "Montserrat, Arial, sans-serif"),
#       events = list(
#         afterDrilldown = highcharter::JS(
#           "function () {
#             var activeSeries = this.series.find(function (series) {
#               return series.visible && series.userOptions.custom &&
#                 series.userOptions.custom.chartTitle;
#             });
# 
#             if (activeSeries) {
#               this.setTitle(
#                 { text: activeSeries.userOptions.custom.chartTitle, useHTML: true },
#                 { text: activeSeries.userOptions.custom.chartSubtitle || '' }
#               );
#               this.xAxis[0].setTitle({
#                 text: activeSeries.userOptions.custom.axisTitle || 'Year'
#               });
#             }
#           }"
#         ),
#         drillup = highcharter::JS(
#           "function (event) {
#             var options = event.seriesOptions || {};
#             var custom = options.custom || {};
# 
#             if (custom.chartTitle) {
#               this.setTitle(
#                 { text: custom.chartTitle, useHTML: true },
#                 { text: custom.chartSubtitle || '' }
#               );
#               this.xAxis[0].setTitle({ text: custom.axisTitle || 'Year' });
#             }
#           }"
#         )
#       )
#     ) %>%
#     highcharter::hc_title(
#       text = root_title,
#       useHTML = TRUE,
#       align = "center",
#       style = list(
#         color = "#333333",
#         fontSize = "22px",
#         fontWeight = "500"
#       )
#     ) %>%
#     highcharter::hc_subtitle(
#       text = root_subtitle,
#       style = list(
#         color = "#666666",
#         fontSize = "14px",
#         fontWeight = "600"
#       )
#     ) %>%
#     highcharter::hc_xAxis(
#       type = "category",
#       title = list(text = "Year"),
#       lineColor = "#cbd5e1",
#       tickColor = "#cbd5e1",
#       labels = list(
#         style = list(color = "#5b6470", fontSize = "11px")
#       )
#     ) %>%
#     highcharter::hc_yAxis(
#       min = 0,
#       title = list(
#         text = "PM2.5 (µg/m³)",
#         style = list(color = "#4b5563", fontWeight = "700")
#       ),
#       gridLineColor = "#e5e7eb",
#       labels = list(style = list(color = "#5b6470"))
#     ) %>%
#     highcharter::hc_add_series(
#       name = "Satellite Data Including Sea Salt and Dust (1998-2024)",
#       type = "spline",
#       data = satellite_points,
#       color = "#800000",
#       lineWidth = 2.5,
#       marker = list(symbol = "circle", radius = 4),
#       custom = list(
#         chartTitle = root_title,
#         chartSubtitle = root_subtitle,
#         axisTitle = "Year"
#       )
#     ) %>%
#     highcharter::hc_add_series(
#       name = "Ground Monitoring (2025-2026)",
#       type = "spline",
#       data = ground_points,
#       color = "#ff7f0e",
#       lineWidth = 2.5,
#       marker = list(symbol = "diamond", radius = 5),
#       custom = list(
#         chartTitle = root_title,
#         chartSubtitle = root_subtitle,
#         axisTitle = "Year"
#       )
#     ) %>%
#     highcharter::hc_add_series(
#       name = "PM2.5 National Standard",
#       type = "line",
#       data = standard_points,
#       color = "#808080",
#       dashStyle = "ShortDash",
#       lineWidth = 2,
#       marker = list(symbol = "square", radius = 4),
#       custom = list(
#         chartTitle = root_title,
#         chartSubtitle = root_subtitle,
#         axisTitle = "Year"
#       )
#     ) %>%
#     highcharter::hc_drilldown(
#       allowPointDrilldown = TRUE,
#       animation = list(duration = 300),
#       activeAxisLabelStyle = list(
#         color = "#800000",
#         fontWeight = "700",
#         textDecoration = "none"
#       ),
#       activeDataLabelStyle = list(
#         color = "#800000",
#         fontWeight = "700",
#         textDecoration = "none"
#       ),
#       drillUpButton = list(
#         position = list(align = "right", x = -8, y = 6),
#         theme = list(
#           fill = "#ffffff",
#           stroke = "#cbd5e1",
#           `stroke-width` = 1,
#           r = 4,
#           style = list(
#             color = "#334155",
#             fontSize = "12px",
#             fontWeight = "700"
#           )
#         )
#       ),
#       series = drilldown_series
#     ) %>%
#     highcharter::hc_tooltip(
#       useHTML = TRUE,
#       formatter = highcharter::JS(
#         "function () {
#           return `<div style='font-family:Montserrat, Arial, sans-serif; font-size:12px; line-height:1.6;'>
#             <b style='font-size:14px;'>${this.point.name}</b><br/>
#             <span style='color:#64748b;'>${this.series.name}:</span>
#             <b>${Highcharts.numberFormat(this.point.y, 1)} µg/m³</b>
#           </div>`;
#         }"
#       )
#     ) %>%
#     highcharter::hc_plotOptions(
#       series = list(
#         cursor = "pointer",
#         dataLabels = list(
#           enabled = FALSE,
#           style = list(textOutline = "none")
#         )
#       ),
#       column = list(
#         borderWidth = 0,
#         borderRadius = 2,
#         groupPadding = 0.08,
#         pointPadding = 0.05,
#         dataLabels = list(
#           enabled = TRUE,
#           format = "{point.y:.1f}",
#           crop = FALSE,
#           overflow = "allow",
#           style = list(
#             color = "#111827",
#             fontSize = "10px",
#             textOutline = "none"
#           )
#         )
#       )
#     ) %>%
#     highcharter::hc_legend(
#       enabled = TRUE,
#       align = "center",
#       verticalAlign = "bottom",
#       itemStyle = list(color = "#333333", fontWeight = "600")
#     ) %>%
#     highcharter::hc_credits(enabled = FALSE) %>%
#     highcharter::hc_exporting(enabled = TRUE)
# 
#   chart$x$hc_opts$lang <- list(drillUpText = "Back to {series.name}")
#   chart
# })


output$avg_aqi_id_ <- renderHighchart({
  # req(df())  # Check reactive input exists
  
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
    unique(data_df$name0),
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
  # hc_add_series(
  #   name = "Satellite Data Excluding Sea Salt and Dust (1998–2024)",
  #   data = lapply(1:nrow(satellite_df), function(i) {
  #     list(x = satellite_df$year[i], y = satellite_df$pm_aqli[i])
  #   }),
  #   type = "spline",
  #   color = "#1f77b4",
  #   lineWidth = 2
  # ) %>%
  
  hc_add_series(
    name = "Satellite Data Including Sea Salt and Dust (1998–2024)",
    data = lapply(1:nrow(satellite_df), function(i) {
      list(x = satellite_df$year[i], y = satellite_df$pm_aqli[i])
    }),
    type = "spline",
    color = "maroon",
    lineWidth = 2
  ) %>%
    
    # ================= SERIES 2: GROUND =================
  hc_add_series(
    name = "Ground Monitoring (2025–2026)",
    data = lapply(1:nrow(ground_df), function(i) {
      list(x = ground_df$year[i], y = ground_df$pm25[i])
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

output$annual_pm25_location_drilldown <- highcharter::renderHighchart({
  
  req(input$country_trd)
  
  state_selected <- isTRUE(input$use_state_aq) &&
    has_value(input$state_trd)
  
  district_selected <- isTRUE(input$use_district_id_aq) &&
    has_value(input$state_trd_aq) &&
    has_value(input$district_trd_aq)
  
  # ---------------------------------------------------------
  # DISTRICT LEVEL
  # ---------------------------------------------------------
  
  if (district_selected) {
    
    raw_df <- get_district_aqli()
    
    title_text <- paste0(
      "Annual <span style='color:maroon;'>",
      input$country_trd,
      " → ",
      input$state_trd_aq,
      " → ",
      input$district_trd_aq,
      "</span> PM<sub>2.5</sub> Concentration"
    )
    
  } else if (state_selected) {
    
    # ---------------------------------------------------------
    # STATE LEVEL
    # ---------------------------------------------------------
    
    raw_df <- get_state_aqli()
    
    title_text <- paste0(
      "Annual <span style='color:maroon;'>",
      input$country_trd,
      " → ",
      input$state_trd_aq,
      "</span> PM<sub>2.5</sub> Concentration"
    )
    
  } else {
    
    # ---------------------------------------------------------
    # DEFAULT COUNTRY LEVEL
    # ---------------------------------------------------------
    
    raw_df <- get_country_aqli()
    
    title_text <- paste0(
      "Annual <span style='color:maroon;'>",
      input$country_trd,
      "</span> PM<sub>2.5</sub> Concentration"
    )
  }
  
  shiny::validate(
    shiny::need(nrow(raw_df) > 0, "No annual data available for selected filters.")
  )
  
  raw_df <- as.data.frame(raw_df)
  
  # ---------------------------------------------------------
  # Required columns check
  # ---------------------------------------------------------
  
  required_cols <- c("year", "pm25", "natstandard")
  
  missing_cols <- setdiff(required_cols, names(raw_df))
  
  shiny::validate(
    shiny::need(
      length(missing_cols) == 0,
      paste0("Missing required columns: ", paste(missing_cols, collapse = ", "))
    )
  )
  
  # ---------------------------------------------------------
  # Clean + yearly aggregation
  # ---------------------------------------------------------
  
  data_df <- raw_df %>%
    dplyr::mutate(
      year = as.numeric(year),
      pm25 = as.numeric(pm25),
      natstandard = as.numeric(natstandard),
      pm25 = dplyr::if_else(is.nan(pm25), NA_real_, pm25),
      natstandard = dplyr::if_else(is.nan(natstandard), NA_real_, natstandard)
    ) %>%
    dplyr::filter(!is.na(year)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      pm25 = mean(pm25, na.rm = TRUE),
      natstandard = dplyr::first(stats::na.omit(natstandard)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      pm25 = dplyr::if_else(is.nan(pm25), NA_real_, pm25),
      natstandard = dplyr::if_else(is.nan(natstandard), NA_real_, natstandard)
    ) %>%
    dplyr::arrange(year)
  
  shiny::validate(
    shiny::need(nrow(data_df) > 0, "No annual data available after processing.")
  )
  
  # ---------------------------------------------------------
  # Split same pm25 column into satellite and ground periods
  # ---------------------------------------------------------
  
  satellite_df <- data_df %>%
    dplyr::filter(year >= 1998, year <= 2024, !is.na(pm25))
  
  ground_df <- data_df %>%
    dplyr::filter(year >= 2025, year <= 2026, !is.na(pm25))
  
  natstandard_df <- data_df %>%
    dplyr::filter(!is.na(natstandard))
  
  subtitle_text <- if (nrow(natstandard_df) == 0) {
    "No National PM2.5 Standard"
  } else {
    paste0(
      "National Avg. PM2.5 Standard (",
      unique(na.omit(data_df$natstandard))[1],
      " µg/m³)"
    )
  }
  
  # ---------------------------------------------------------
  # Highchart
  # ---------------------------------------------------------
  
  highcharter::highchart() %>%
    
    highcharter::hc_chart(
      type = "line",
      backgroundColor = "transparent"
    ) %>%
    
    highcharter::hc_title(
      text = title_text,
      useHTML = TRUE
    ) %>%
    
    highcharter::hc_subtitle(
      text = subtitle_text,
      useHTML = TRUE
    ) %>%
    
    highcharter::hc_xAxis(
      type = "linear",
      title = list(text = "Year"),
      plotLines = list(
        list(
          color = "darkgrey",
          width = 1.5,
          value = 2024,
          dashStyle = "Dash",
          zIndex = 5,
          label = list(
            text = "Post-2024:<br>Ground<br>monitoring",
            useHTML = TRUE,
            rotation = 0,
            align = "left",
            y = 25,
            x = -27,
            style = list(
              color = "#ff7f0e",
              fontSize = "10px"
            )
          )
        )
      )
    ) %>%
    
    highcharter::hc_yAxis(
      min = 0,
      title = list(text = "PM2.5 (µg/m³)")
    ) %>%
    
    highcharter::hc_add_series(
      name = "Satellite Data Including Sea Salt and Dust (1998–2024)",
      data = lapply(seq_len(nrow(satellite_df)), function(i) {
        list(
          x = satellite_df$year[i],
          y = satellite_df$pm25[i]
        )
      }),
      type = "spline",
      color = "maroon",
      lineWidth = 2
    ) %>%
    
    highcharter::hc_add_series(
      name = "Ground Monitoring (2025–2026)",
      data = lapply(seq_len(nrow(ground_df)), function(i) {
        list(
          x = ground_df$year[i],
          y = ground_df$pm25[i]
        )
      }),
      type = "spline",
      color = "#ff7f0e",
      lineWidth = 2
    ) %>%
    
    highcharter::hc_add_series(
      name = "PM2.5 National Standard",
      data = lapply(seq_len(nrow(natstandard_df)), function(i) {
        list(
          x = natstandard_df$year[i],
          y = natstandard_df$natstandard[i]
        )
      }),
      type = "line",
      dashStyle = "ShortDash",
      color = "grey",
      lineWidth = 1.8
    ) %>%
    
    highcharter::hc_tooltip(
      shared = TRUE,
      crosshairs = TRUE,
      valueDecimals = 1,
      valueSuffix = " µg/m³"
    ) %>%
    
    highcharter::hc_legend(
      enabled = TRUE
    ) %>%
    
    highcharter::hc_plotOptions(
      spline = list(
        marker = list(
          enabled = TRUE,
          radius = 3
        ),
        connectNulls = FALSE
      ),
      line = list(
        marker = list(enabled = FALSE)
      )
    ) %>%
    
    highcharter::hc_credits(enabled = FALSE)
  
})