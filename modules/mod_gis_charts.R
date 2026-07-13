# =========================================================
# REQUIRED PACKAGES
# =========================================================

library(shiny)
library(dplyr)
library(leaflet)
library(plotly)
library(htmltools)
library(lubridate)
library(scales)




# output$country_wise_pm_llp <- renderLeaflet({
# 
#     
#     data_df <- gadm1_shp_pm %>%
#       filter(name0 == input$country) %>%
#       select(name1, !!input$year, population) 
#     
#     data_df$label_text <- sprintf(
#       "<div style='font-family:sans-serif;font-size:13px;line-height:1.5;'>
#          <b style='font-size:14px;color:#2c3e50;'>%s</b><br/>
#          <span style='color:#7f8c8d;'>Population: </span>
#          <b style='color:#2c3e50;'>%s</b><br/>
#          
#          <span style='color:#7f8c8d;'><b>PM<sub>2.5</sub> Concentration</b><br/></span>
#          <b style='color:#e67e22;'>%.1f µg/m³</b>
#        </div>",
#       data_df$name1,
#       formatC(data_df$population, format = "f", big.mark = ",", digits = 0),
#       data_df[[input$year_gis]]
#     )
#     
#     pal <- colorBin(
#       palette = c("#b7ebf1", "#8fd8e4", "#3db1c8", "#3f8dac", "#416891", "#434475", "#451f59"),
#       domain = data_df[[input$year_gis]],
#       bins = c(0, 10, 25, 35, 50, 60, Inf),
#       na.color = "grey"
#     )
#     
#     leaflet(data_df) %>%
#       addProviderTiles(providers$CartoDB.Positron) %>%
#       
#       addPolygons(
#         layerId = ~name1,
#         fillColor = ~pal(data_df[[input$year_gis]]),
#         weight = 1,
#         opacity = 1,
#         color = "white",
#         dashArray = "3",
#         fillOpacity = 0.8,
#         highlightOptions = highlightOptions(
#           weight = 2,
#           color = "#666",
#           dashArray = "",
#           fillOpacity = 0.9,
#           bringToFront = TRUE
#         ),
#         label = lapply(data_df$label_text, HTML),
#         labelOptions = labelOptions(
#           style = list("font-weight" = "normal", padding = "3px 8px"),
#           textsize = "13px",
#           direction = "auto"
#         )
#       ) %>%
#       
#       addControl(
#         html = HTML("
#     <div style='padding: 8px; background: rgba(255,255,255,0.8); font-size: 12px; border-radius: 4px;'>
#       <b>PM<sub>2.5</sub> Concentration (µg/m³)</b><br/>
#       <div style='display: flex; flex-wrap: wrap; gap: 6px 10px; align-items: center; margin-top: 6px;'>
#         <div style='display: flex; align-items: center; gap: 4px;'>
#           <div style='background:#b7ebf1; width:20px; height:12px;'></div><span>0–10</span>
#         </div>
#         <div style='display: flex; align-items: center; gap: 4px;'>
#           <div style='background:#8fd8e4; width:20px; height:12px;'></div><span>10–25</span>
#         </div>
#         <div style='display: flex; align-items: center; gap: 4px;'>
#           <div style='background:#3db1c8; width:20px; height:12px;'></div><span>25–35</span>
#         </div>
#         <div style='display: flex; align-items: center; gap: 4px;'>
#           <div style='background:#3f8dac; width:20px; height:12px;'></div><span>35–50</span>
#         </div>
#         <div style='display: flex; align-items: center; gap: 4px;'>
#           <div style='background:#416891; width:20px; height:12px;'></div><span>50–60</span>
#         </div>
#         <div style='display: flex; align-items: center; gap: 4px;'>
#           <div style='background:#434475; width:20px; height:12px;'></div><span>60+</span>
#         </div>
#       </div>
#     </div>
#   "),
#         position = "bottomleft"
#       ) %>%  addFullscreenControl(
#         position = "topleft",      # Optional: position of the button (default is "topleft")
#         pseudoFullscreen = FALSE   # Optional: if TRUE, fullscreen to page width/height, not true browser fullscreen
#       )
#     
#     
#   
#   
#   
# 
#   ########## click events
#   
#   
#   
# })


normalize_month_year <- function(month, year) {

  month_raw <- trimws(tolower(as.character(month)))
  year_raw <- as.integer(as.character(year))

  month_num_numeric <- suppressWarnings(as.integer(month_raw))

  month_abb <- tolower(month.abb)
  month_full <- tolower(month.name)

  month_num_text <- dplyr::case_when(
    month_raw %in% month_abb  ~ match(month_raw, month_abb),
    month_raw %in% month_full ~ match(month_raw, month_full),
    TRUE ~ NA_integer_
  )

  month_num <- dplyr::coalesce(month_num_numeric, month_num_text)

  as.Date(sprintf("%04d-%02d-01", year_raw, month_num))
}


# =========================================================
# GIS UI MODULE
# Only map output stays here
# =========================================================

gis_ui <- function(id, map_height = "450px") {

  ns <- NS(id)

  tagList(

    tags$head(
      tags$style(
        htmltools::HTML("
          .sensor-hover {
            background: rgba(17,24,39,0.96);
            color: white;
            border: none;
            border-radius: 10px;
            padding: 9px 10px;
            font-size: 13px;
            line-height: 1.4;
          }

          .sensor-hover:before {
            display: none;
          }

          .leaflet-container {
            background: #f8fafc !important;
          }
        ")
      )
    ),

    leafletOutput(
      outputId = ns("aq_map"),
      height = map_height
    )
  )
}


# =========================================================
# GIS SERVER MODULE
# Outputs:
# map-aq_map
# map-monthly_trend
# =========================================================

gis_server <- function(
    id,
    df,
    country_reactive,
    year_reactive = NULL
) {

  moduleServer(id, function(input, output, session) {

    selected_sensor <- reactiveVal(NULL)


    # =====================================================
    # SENSOR CLICK
    # =====================================================

    observeEvent(input$aq_map_marker_click, {
      selected_sensor(input$aq_map_marker_click$id)
    })

    observeEvent(input$aq_map_shape_click, {
      selected_sensor(input$aq_map_shape_click$id)
    })


    # =====================================================
    # CLEAN SELECTED YEARS
    # =====================================================

    clean_selected_years <- function(x) {

      if (is.null(x)) return(NULL)

      x <- as.character(x)

      if (length(x) == 0) return(NULL)
      if (any(x %in% c("All", "all", "ALL", ""))) return(NULL)

      years <- suppressWarnings(as.integer(x))
      years <- years[!is.na(years)]

      if (length(years) == 0) return(NULL)

      years
    }


    # =====================================================
    # PREPARE DATA
    # =====================================================

    prepared_df <- reactive({

      req(df())

      data_df <- df()

      req(nrow(data_df) > 0)

      validate(
        need(
          all(
            c(
              "name",
              "lon",
              "lat",
              "month",
              "year",
              "pm25_vvg"
            ) %in% names(data_df)
          ),
          "Required columns missing: name, lon, lat, month, year, pm25_vvg"
        )
      )

      if (!"sensor_id" %in% names(data_df)) {
        data_df$sensor_id <- data_df$name
      }

      if (!"monitors" %in% names(data_df)) {
        data_df$monitors <- 1
      }

      data_df <- data_df %>%
        mutate(
          name = as.character(name),
          sensor_id = as.character(sensor_id),
          sensor_id = dplyr::coalesce(sensor_id, name),

          year = as.integer(as.character(year)),
          pm25_vvg = as.numeric(pm25_vvg),
          lon = as.numeric(lon),
          lat = as.numeric(lat),

          monitors = suppressWarnings(as.numeric(monitors)),
          monitors = ifelse(is.na(monitors), 1, monitors),

          month_date = normalize_month_year(month, year),
          month_num = lubridate::month(month_date),
          month_label = factor(
            month.abb[month_num],
            levels = month.abb
          )
        ) %>%
        filter(
          !is.na(name),
          !is.na(sensor_id),
          !is.na(pm25_vvg),
          !is.na(lat),
          !is.na(lon),
          !is.na(year),
          !is.na(month_date),
          !is.na(month_num)
        )

      selected_years <- NULL

      if (!is.null(year_reactive)) {
        selected_years <- clean_selected_years(year_reactive())
      }

      if (!is.null(selected_years)) {
        data_df <- data_df %>%
          filter(year %in% selected_years)
      }

      data_df
    })


    # =====================================================
    # COUNTRY FILTER
    # =====================================================

    country_df <- reactive({

      data_df <- prepared_df()

      req(nrow(data_df) > 0)

      selected_country <- country_reactive()

      country_col <- intersect(
        c("country", "admin", "country_name", "Country", "Admin", "COUNTRY"),
        names(data_df)
      )

      if (
        length(country_col) > 0 &&
        !is.null(selected_country) &&
        length(selected_country) > 0 &&
        !any(selected_country %in% c("All", "all", "ALL", ""))
      ) {
        data_df <- data_df %>%
          filter(.data[[country_col[1]]] %in% selected_country)
      }

      data_df
    })


    # =====================================================
    # MAP DATA: latest selected year/month
    # =====================================================

    map_df <- reactive({

      data_df <- country_df()

      req(nrow(data_df) > 0)

      latest_year <- max(data_df$year, na.rm = TRUE)

      latest_month <- max(
        data_df$month_num[data_df$year == latest_year],
        na.rm = TRUE
      )

      data_df %>%
        filter(
          year == latest_year,
          month_num == latest_month
        ) %>%
        group_by(
          sensor_id,
          name,
          lon,
          lat
        ) %>%
        summarise(
          pm25 = mean(pm25_vvg, na.rm = TRUE),
          monitors = max(monitors, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          monitors = ifelse(is.finite(monitors), monitors, 1),

          aq_band = case_when(
            pm25 <= 12  ~ "Good",
            pm25 <= 35  ~ "Moderate",
            pm25 <= 55  ~ "High",
            pm25 <= 150 ~ "Very High",
            TRUE        ~ "Extreme"
          ),

          radius = pmin(18, pmax(5, 5 + sqrt(pm25)))
        ) %>%
        filter(!is.na(pm25))
    })





    # =====================================================
    # MAP
    # =====================================================

    output$aq_map <- renderLeaflet({

      data_df <- map_df()

      req(nrow(data_df) > 0)

      selected_country <- country_reactive()

      states_sel <- world %>%
        filter(admin %in% selected_country)

      pal <- colorBin(
        palette = c(
          "#22c55e",
          "#eab308",
          "#f97316",
          "#ef4444",
          "#7f1d1d"
        ),
        bins = c(-Inf, 12, 35, 55, 150, Inf),
        domain = data_df$pm25,
        na.color = "#94a3b8"
      )

      sensor_labels <- lapply(seq_len(nrow(data_df)), function(i) {
        htmltools::HTML(
          paste0(
            "<b>", htmltools::htmlEscape(data_df$name[i]), "</b><br>",
            "PM2.5: ", round(data_df$pm25[i], 1), " µg/m³<br>",
            "Category: ", data_df$aq_band[i], "<br>",
            "Monitors: ", data_df$monitors[i]
          )
        )
      })

      map <- leaflet(data_df) %>%
        
        addProviderTiles(
          providers$CartoDB.Positron
        ) %>%
        addPolygons(
          data = world,
          fill = FALSE,
          color = "#9ca3af",
          weight = 0.5,
          opacity = 0.35
        )

      if (nrow(states_sel) > 0) {
        map <- map %>%
          addPolygons(
            data = states_sel,
            fill = FALSE,
            color = "#2563eb",
            weight = 1,
            opacity = 0.65,
            highlightOptions = highlightOptions(
              color = "#111827",
              weight = 2,
              bringToFront = TRUE
            )
          )
      }

      map <- map %>%
        addCircleMarkers(
          lng = ~lon,
          lat = ~lat,
          layerId = ~sensor_id,
          radius = ~radius,
          fillColor = ~pal(pm25),
          fillOpacity = 0.9,
          color = "#ffffff",
          weight = 1.2,
          stroke = TRUE,
          clusterOptions = markerClusterOptions(),
          label = sensor_labels,
          labelOptions = labelOptions(
            className = "sensor-hover",
            direction = "auto",
            opacity = 1
          )
        ) %>%
        
        addControl(
          position = "bottomright",
          html = htmltools::HTML(
            paste0(
              "<div style='
        background:rgba(255,255,255,0.96);
        padding:10px 14px;
        border-radius:12px;
        box-shadow:0 8px 24px rgba(15,23,42,0.16);
        font-family:Montserrat, sans-serif;
        border:1px solid rgba(226,232,240,0.9);
      '>",
              
              "<div style='
        font-size:13px;
        font-weight:700;
        color:#111827;
        margin-bottom:8px;
        white-space:nowrap;
      '>
        PM<sub>2.5</sub> Concentration <span style='font-size:11px;font-weight:500;color:#64748b;'>(µg/m³)</span>
      </div>",
              
              "<div style='display:flex;align-items:center;gap:10px;'>",
              
              "<div style='display:flex;align-items:center;gap:5px;'>
        <span style='width:13px;height:13px;background:#22c55e;border-radius:50%;display:inline-block;'></span>
        <span style='font-size:12px;color:#334155;'>0–12</span>
      </div>",
              
              "<div style='display:flex;align-items:center;gap:5px;'>
        <span style='width:13px;height:13px;background:#eab308;border-radius:50%;display:inline-block;'></span>
        <span style='font-size:12px;color:#334155;'>12–35</span>
      </div>",
              
              "<div style='display:flex;align-items:center;gap:5px;'>
        <span style='width:13px;height:13px;background:#f97316;border-radius:50%;display:inline-block;'></span>
        <span style='font-size:12px;color:#334155;'>35–55</span>
      </div>",
              
              "<div style='display:flex;align-items:center;gap:5px;'>
        <span style='width:13px;height:13px;background:#ef4444;border-radius:50%;display:inline-block;'></span>
        <span style='font-size:12px;color:#334155;'>55–150</span>
      </div>",
              
              "<div style='display:flex;align-items:center;gap:5px;'>
        <span style='width:13px;height:13px;background:#7f1d1d;border-radius:50%;display:inline-block;'></span>
        <span style='font-size:12px;color:#334155;'>&gt;150</span>
      </div>",
              
              "</div>",
              "</div>"
            )
          )
        ) %>% 

        addScaleBar(
          position = "bottomleft",
          options = scaleBarOptions(imperial = FALSE)
        )

      if (requireNamespace("leaflet.extras", quietly = TRUE)) {
        map <- map %>%
          leaflet.extras::addFullscreenControl(
            position = "topleft",
            pseudoFullscreen = FALSE
          )
      }

      if (nrow(data_df) == 1) {
        map <- map %>%
          setView(
            lng = data_df$lon[1],
            lat = data_df$lat[1],
            zoom = 8
          )
      } else {
        map <- map %>%
          fitBounds(
            lng1 = min(data_df$lon, na.rm = TRUE),
            lat1 = min(data_df$lat, na.rm = TRUE),
            lng2 = max(data_df$lon, na.rm = TRUE),
            lat2 = max(data_df$lat, na.rm = TRUE)
          )
      }

      map
    })


    # =====================================================
    # TREND DATA: SENSOR + COUNTRY AVG, LINE PER YEAR
    # =====================================================

    trend_df <- reactive({

      data_df <- country_df()

      req(nrow(data_df) > 0)

      sensor_selected <- selected_sensor()

      if (
        is.null(sensor_selected) ||
        !(sensor_selected %in% data_df$sensor_id)
      ) {
        sensor_selected <- data_df %>%
          group_by(sensor_id) %>%
          summarise(
            avg_pm = mean(pm25_vvg, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(desc(avg_pm)) %>%
          slice(1) %>%
          pull(sensor_id)
      }

      sensor_lines <- data_df %>%
        filter(sensor_id == sensor_selected) %>%
        group_by(
          year,
          month_num,
          month_label
        ) %>%
        summarise(
          pm25 = mean(pm25_vvg, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          type = "Sensor",
          series = paste0(year, " Sensor")
        )

      country_lines <- data_df %>%
        group_by(
          year,
          month_num,
          month_label
        ) %>%
        summarise(
          pm25 = mean(pm25_vvg, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          type = "Country",
          series = paste0(year, " Country")
        )

      bind_rows(country_lines, sensor_lines)
    })


    # =====================================================
    # PLOTLY WITH LEGEND
    # =====================================================

    output$monthly_trend <- highcharter::renderHighchart({
      
      sensor_selected <- selected_sensor()
      
      if (
        is.null(sensor_selected) ||
        !(sensor_selected %in% country_df()$sensor_id)
      ) {
        sensor_selected <- country_df() %>%
          dplyr::group_by(sensor_id) %>%
          dplyr::summarise(
            avg_pm = mean(pm25_vvg, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(dplyr::desc(avg_pm)) %>%
          dplyr::slice(1) %>%
          dplyr::pull(sensor_id)
      }
      
      sensor_info <- country_df() %>%
        dplyr::filter(sensor_id == sensor_selected) %>%
        dplyr::summarise(
          sensor_name = dplyr::first(name),
          sensor_id   = dplyr::first(sensor_id),
          monitors    = max(monitors, na.rm = TRUE),
          .groups = "drop"
        )
      
      td <- trend_df()
      req(nrow(td) > 0)
      
      td <- td %>%
        dplyr::mutate(
          year = as.character(year),
          month_num = as.integer(month_num),
          pm25 = as.numeric(pm25)
        ) %>%
        dplyr::filter(
          !is.na(year),
          !is.na(month_num),
          !is.na(pm25),
          month_num >= 1,
          month_num <= 12
        )
      
      req(nrow(td) > 0)
      
      years <- sort(unique(td$year))
      
      aqli_colors <- c(
        "#85abd1", 
        "#7f0000",
        "#fee08b",
        "#fc8d59",
        "#d73027",
        "#542788"
       
      )
      
      year_colors <- setNames(
        rep(aqli_colors, length.out = length(years)),
        years
      )
      
      text <- paste0(
        "Monthly PM<sub>2.5</sub> Trend in <span style='color:maroon;'>",
        unique(df()$country),
        "</span>",
        "<br>",
        "<span style='font-size:13px;color:#374151;font-weight:500;'>",
        "Sensor: ",
        htmltools::htmlEscape(sensor_info$sensor_name),
        " | ID: ",
        htmltools::htmlEscape(sensor_info$sensor_id),
        " | Monitors: ",
        sensor_info$monitors,
        "</span>"
      )
      
      hc <- highcharter::highchart() %>%
        
        highcharter::hc_chart(
          type = "spline",
          backgroundColor = "transparent",
          style = list(
            fontFamily = "Montserrat, sans-serif"
          )
        ) %>%
        
        highcharter::hc_title(
          text = text,
          align = "center",
          useHTML = TRUE,
          style = list(
            fontSize = "18px",
            fontWeight = "600",
            color = "#111827"
          )
        ) %>%
        
        highcharter::hc_xAxis(
          categories = month.abb,
          tickLength = 0,
          lineColor = "#d1d5db",
          labels = list(
            style = list(
              color = "#6b7280",
              fontSize = "12px"
            )
          )
        ) %>%
        
        highcharter::hc_yAxis(
          title = list(
            text = "PM2.5 µg/m³",
            style = list(
              color = "#374151",
              fontWeight = "500"
            )
          ),
          gridLineColor = "rgba(209,213,219,0.35)",
          labels = list(
            style = list(
              color = "#6b7280"
            )
          )
        ) %>%
        
        highcharter::hc_tooltip(
          shared = TRUE,
          useHTML = TRUE,
          backgroundColor = "rgba(17,24,39,0.96)",
          borderWidth = 0,
          borderRadius = 10,
          shadow = TRUE,
          style = list(
            color = "#ffffff",
            fontSize = "12px"
          ),
          formatter = highcharter::JS(
            "function () {
          let s = '<div style=\"padding:10px 12px;min-width:190px;\">';
          s += '<div style=\"font-size:13px;font-weight:700;margin-bottom:6px;color:#ffffff;\">' + this.x + '</div>';
          
          this.points.forEach(function(point) {
            s += '<div style=\"margin:5px 0;\">' +
                   '<span style=\"color:' + point.color + ';font-size:14px;\">●</span> ' +
                   point.series.name + ': ' +
                   '<b>' + Highcharts.numberFormat(point.y, 1) + ' µg/m³</b>' +
                 '</div>';
          });
          
          s += '</div>';
          return s;
        }"
          )
        ) %>%
        
        highcharter::hc_legend(
          enabled = TRUE,
          align = "center",
          verticalAlign = "bottom",
          layout = "horizontal",
          itemDistance = 18,
          symbolWidth = 28,
          itemStyle = list(
            color = "#374151",
            fontWeight = "500",
            fontSize = "12px"
          )
        ) %>%
        
        highcharter::hc_plotOptions(
          spline = list(
            animation = list(duration = 800),
            marker = list(enabled = FALSE),
            linecap = "round",
            states = list(
              hover = list(
                lineWidthPlus = 0
              )
            )
          )
        ) %>%
        
        highcharter::hc_exporting(enabled = FALSE) %>%
        highcharter::hc_credits(enabled = FALSE)
      
      for (yr in years) {
        
        yr_color <- year_colors[[yr]]
        
        country_line <- td %>%
          dplyr::filter(year == yr, type == "Country") %>%
          dplyr::group_by(month_num) %>%
          dplyr::summarise(
            pm25 = mean(pm25, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(month_num)
        
        sensor_line <- td %>%
          dplyr::filter(year == yr, type == "Sensor") %>%
          dplyr::group_by(month_num) %>%
          dplyr::summarise(
            pm25 = mean(pm25, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(month_num)
        
        if (nrow(country_line) > 0) {
          
          country_data <- purrr::map2(
            country_line$month_num - 1,
            round(country_line$pm25, 1),
            ~ list(x = .x, y = .y)
          )
          
          hc <- hc %>%
            highcharter::hc_add_series(
              name = paste0(yr, " Country"),
              data = country_data,
              type = "spline",
              lineWidth = 1.5,
              dashStyle = "Dash",
              color = yr_color,
              opacity = 0.5,
              marker = list(
                enabled = FALSE
              )
            )
        }
        
        if (nrow(sensor_line) > 0) {
          
          sensor_data <- purrr::map2(
            sensor_line$month_num - 1,
            round(sensor_line$pm25, 1),
            ~ list(x = .x, y = .y)
          )
          
          hc <- hc %>%
            highcharter::hc_add_series(
              name = paste0(yr, " Sensor - ", sensor_info$sensor_name),
              data = sensor_data,
              type = "spline",
              lineWidth = 4,
              color = yr_color,
              marker = list(
                enabled = TRUE,
                radius = 4,
                symbol = "circle",
                fillColor = yr_color,
                lineColor = "#ffffff",
                lineWidth = 1.5
              )
            )
        }
      }
      
      hc
    })
    })
}