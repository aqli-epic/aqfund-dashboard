library(data.table)
library(highcharter)

set_highchart_lang <- function(chart, ...) {
  chart$x$hc_opts$lang <- list(...)
  chart
}

active_sensor_info_html <- function(title, axis = FALSE) {
  info_text <- paste(
    "An active sensor is a distinct sensor ID with at least one monitoring",
    "record in the period shown. Repeated readings do not increase the count."
  )
  position_class <- if (axis) " plot-info--axis" else ""

  paste0(
    "<span class='plot-title-info'>",
    htmltools::htmlEscape(title),
    "<button type='button' class='plot-info", position_class,
    "' aria-expanded='false' aria-label='",
    htmltools::htmlEscape(info_text, attribute = TRUE),
    "'><span class='plot-info-icon' aria-hidden='true'>i</span>",
    "<span class='plot-info-tooltip' role='tooltip'>",
    htmltools::htmlEscape(info_text),
    "</span></button></span>"
  )
}

# =========================================================
# AQLI-style PM2.5 color
# =========================================================

aqli_pm_color <- function(x) {
  if (is.na(x)) {
    "#d9d9d9"
  } else if (x <= 12) {
    "#7cb342"
  } else if (x <= 35) {
    "#fdd835"
  } else if (x <= 55) {
    "#fb8c00"
  } else if (x <= 150) {
    "#e53935"
  } else {
    "#8e0000"
  }
}


# =========================================================
# Active Sensor Drilldown
# Country -> Year -> Month
# =========================================================

output$active_sensor_drilldown <- highcharter::renderHighchart({
  active_dt <- data.table::as.data.table(sensor_data)[
    year %in% c(2025, 2026) &
      !is.na(name0) &
      trimws(name0) != "" &
      !is.na(sensors_id)
  ]
  active_dt <- active_dt[year %in% input$year_heatmp]
  shiny::validate(
    shiny::need(nrow(active_dt) > 0, "No active sensor data available.")
  )

  country_counts <- active_dt[
    ,
    .(active_sensors = data.table::uniqueN(sensors_id)),
    by = .(country = name0)
  ][order(-active_sensors, country)]

  year_counts <- active_dt[
    ,
    .(active_sensors = data.table::uniqueN(sensors_id)),
    by = .(country = name0, year)
  ][order(country, year)]

  month_counts <- active_dt[
    ,
    .(active_sensors = data.table::uniqueN(sensors_id)),
    by = .(country = name0, year, month)
  ][order(country, year, month)]

  country_colors <- c(
    "#800000", "#006666", "#D4AF37", "#3B82A0", "#C2413B"
  )
  year_colors <- c(`2025` = "#D4AF37", `2026` = "#006666")
  month_colors <- c(
    "#8E2946", "#9F3151", "#B13A5D", "#C14568",
    "#C95773", "#D1697F", "#3B82A0", "#34758F",
    "#2D687E", "#265B6D", "#1F4E5C", "#18414B"
  )

  country_points <- lapply(seq_len(nrow(country_counts)), function(i) {
    list(
      name = country_counts$country[[i]],
      y = country_counts$active_sensors[[i]],
      drilldown = paste0("active_country_", i),
      color = country_colors[(i - 1) %% length(country_colors) + 1]
    )
  })

  drilldown_series <- list()

  for (country_index in seq_len(nrow(country_counts))) {
    selected_country <- country_counts$country[[country_index]]
    country_id <- paste0("active_country_", country_index)
    country_years <- year_counts[country == selected_country]

    year_points <- lapply(seq_len(nrow(country_years)), function(year_index) {
      selected_year <- country_years$year[[year_index]]
      list(
        name = as.character(selected_year),
        y = country_years$active_sensors[[year_index]],
        drilldown = paste0(country_id, "_year_", selected_year),
        color = unname(year_colors[[as.character(selected_year)]])
      )
    })

    drilldown_series[[length(drilldown_series) + 1]] <- list(
      id = country_id,
      name = selected_country,
      type = "bar",
      data = year_points,
      custom = list(
        chartTitle = active_sensor_info_html(
          paste0(selected_country, " - Active Monitors by Year")
        )
      )
    )

    for (selected_year in country_years$year) {
      selected_months <- month_counts[
        country == selected_country & year == selected_year
      ]

      monthly_values <- integer(12)
      valid_months <- selected_months$month %in% 1:12
      monthly_values[selected_months$month[valid_months]] <-
        selected_months$active_sensors[valid_months]

      month_points <- lapply(seq_len(12), function(month_index) {
        list(
          name = month.abb[[month_index]],
          y = monthly_values[[month_index]],
          color = month_colors[[month_index]]
        )
      })

      drilldown_series[[length(drilldown_series) + 1]] <- list(
        id = paste0(country_id, "_year_", selected_year),
        name = paste(selected_country, selected_year, sep = " - "),
        type = "bar",
        data = month_points,
        custom = list(
          chartTitle = active_sensor_info_html(
            paste0(
              selected_country,
              " - ",
              selected_year,
              " - Active Monitors by Month"
            )
          )
        )
      )
    }
  }

  highcharter::highchart() %>%
    highcharter::hc_add_dependency("modules/drilldown.js") %>%
    highcharter::hc_chart(
      type = "bar",
      height = 610,
      spacingTop = 24,
      spacingRight = 36,
      spacingBottom = 20,
      spacingLeft = 16,
      animation = list(duration = 300),
      style = list(
        fontFamily = "Montserrat, Arial, sans-serif"
      ),
      events = list(
        afterDrilldown = highcharter::JS(
          "function () {
            var activeSeries = this.series.find(function (series) {
              return series.visible &&
                series.userOptions.custom &&
                series.userOptions.custom.chartTitle;
            });

            if (activeSeries) {
              this.setTitle({
                text: activeSeries.userOptions.custom.chartTitle,
                useHTML: true
              });
            }
          }"
        ),
        drillup = highcharter::JS(
          "function (event) {
            var options = event.seriesOptions || {};
            var custom = options.custom || {};

            if (custom.chartTitle) {
              this.setTitle({ text: custom.chartTitle, useHTML: true });
            }
          }"
        )
      )
    ) %>%
    highcharter::hc_title(
      text = active_sensor_info_html("Active Monitors by Country"),
      useHTML = TRUE,
      align = "center",
      style = list(
        fontWeight = "500",
        color = "#111827",
        fontSize = "18px"
      )
    ) %>%
    highcharter::hc_xAxis(
      type = "category",
      title = list(text = NULL),
      lineColor = "#d8dee7",
      tickColor = "#d8dee7",
      labels = list(
        style = list(
          color = "#334155",
          fontSize = "11px",
          fontWeight = "600"
        )
      )
    ) %>%
    highcharter::hc_yAxis(
      min = 0,
      allowDecimals = FALSE,
      title = list(
        text = "Distinct active Monitors",
        style = list(
          color = "#374151",
          fontWeight = "700"
        )
      ),
      gridLineColor = "#eef2f7",
      labels = list(
        style = list(color = "#64748b")
      )
    ) %>%
    highcharter::hc_add_series(
      name = "All countries",
      type = "bar",
      data = country_points,
      custom = list(
        chartTitle = active_sensor_info_html("Active Monitors by Country")
      )
    ) %>%
    highcharter::hc_drilldown(
      allowPointDrilldown = TRUE,
      animation = list(duration = 300),
      activeAxisLabelStyle = list(
        color = "#800000",
        fontWeight = "700",
        textDecoration = "none"
      ),
      activeDataLabelStyle = list(
        color = "#800000",
        fontWeight = "700",
        textDecoration = "none"
      ),
      drillUpButton = list(
        position = list(align = "right", x = -8, y = 4),
        theme = list(
          fill = "#ffffff",
          stroke = "#cbd5e1",
          `stroke-width` = 1,
          r = 4,
          style = list(
            color = "#334155",
            fontSize = "12px",
            fontWeight = "700"
          )
        )
      ),
      series = drilldown_series
    ) %>%
    set_highchart_lang(
      drillUpText = "Back to {series.name}"
    ) %>%
    highcharter::hc_tooltip(
      useHTML = TRUE,
      formatter = highcharter::JS(
        "function () {
          return `<div style='font-family:Montserrat, Arial, sans-serif; font-size:12px; line-height:1.6;'>
            <b style='font-size:14px;'>${this.point.name}</b><br/>
            <span style='color:#64748b;'>Active Monitors:</span> <b>${this.point.y}</b>
          </div>`;
        }"
      )
    ) %>%
    highcharter::hc_plotOptions(
      series = list(
        borderWidth = 0,
        borderRadius = 2,
        cursor = "pointer",
        pointPadding = 0.08,
        groupPadding = 0.06,
        dataLabels = list(
          enabled = TRUE,
          inside = FALSE,
          crop = FALSE,
          overflow = "allow",
          style = list(
            color = "#111827",
            fontSize = "11px",
            fontWeight = "700",
            textOutline = "none"
          )
        )
      )
    ) %>%
    highcharter::hc_legend(enabled = FALSE) %>%
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_exporting(enabled = TRUE)
})


# =========================================================
# Funding Priority Matrix
# =========================================================
output$funding_priority_bubble <- highcharter::renderHighchart({
  
  shiny::req(input$year_heatmp)
  
  dt <- data.table::as.data.table(sensor_data)[
    year %in% c(2025, 2026) &
      year %in% input$year_heatmp
  ]
  
  safe_mean <- function(x, digits = 1) {
    
    x <- x[is.finite(x)]
    
    if (length(x) == 0) {
      return(NA_real_)
    }
    
    round(mean(x), digits)
  }
  
  priority_df <- dt[
    !is.na(name0),
    .(
      Sensors = data.table::uniqueN(
        sensors_id[!is.na(sensors_id)]
      ),
      
      Awardees = data.table::uniqueN(
        owner[!is.na(owner)]
      ),
      
      Months = data.table::uniqueN(
        paste(
          year,
          sprintf("%02d", month),
          sep = "-"
        )
      ),
      
      Coverage = safe_mean(
        coverage.percentComplete,
        digits = 1
      ),
      
      PM25 = safe_mean(
        pm25,
        digits = 1
      )
    ),
    by = .(
      Country = name0
    )
  ]
  
  priority_df <- priority_df[
    !is.na(Country) &
      Sensors > 0 &
      is.finite(PM25)
  ][order(-PM25)]
  
  shiny::validate(
    shiny::need(
      nrow(priority_df) > 0,
      "No funding-priority data available for the selected filters."
    )
  )
  
  # ---------------------------------------------------------
  # Monitoring categories
  # ---------------------------------------------------------
  
  priority_df[
    ,
    MonitoringLevel := data.table::fcase(
      Sensors <= 10,
      "Limited monitoring: 1–10",
      
      Sensors <= 25,
      "Moderate monitoring: 11–25",
      
      default = "Broader monitoring: 26+"
    )
  ]
  
  monitoring_levels <- c(
    "Limited monitoring: 1–10",
    "Moderate monitoring: 11–25",
    "Broader monitoring: 26+"
  )
  
  monitoring_colours <- c(
    "Limited monitoring: 1–10" = "#D97706",
    "Moderate monitoring: 11–25" = "#F2B01E",
    "Broader monitoring: 26+" = "#2878B5"
  )
  
  # Highest pollution first
  data.table::setorder(
    priority_df,
    -PM25,
    Sensors,
    Country
  )
  
  priority_df[
    ,
    row_position := .I - 1L
  ]
  
  country_categories <- priority_df$Country
  
  chart_height <- max(
    520,
    250 + nrow(priority_df) * 34
  )
  
  y_max <- ceiling(
    max(priority_df$PM25, na.rm = TRUE) * 1.28 / 10
  ) * 10
  
  # ---------------------------------------------------------
  # Base chart
  # ---------------------------------------------------------
  
  hc <- highcharter::highchart() %>%
    
    highcharter::hc_chart(
      type = "bar",
      height = chart_height,
      spacingTop = 18,
      spacingBottom = 18,
      marginTop = 95,
      marginBottom = 125,
      marginLeft = 215,
      marginRight = 150,
      
      style = list(
        fontFamily = "Montserrat, Arial, sans-serif"
      )
    ) %>%
    
    highcharter::hc_title(
      text = "PM₂.₅ and Active Monitoring by Country",
      align = "center",
      margin = 12,
      
      style = list(
        color = "#111827",
        fontSize = "19px",
        fontWeight = "500"
      )
    ) %>%
    
    highcharter::hc_subtitle(
      text = paste0(
        "Countries are ranked by average PM₂.₅ concentration. ",
        "Colour indicates the number of active monitors."
      ),
      align = "center",
      
      style = list(
        color = "#64748B",
        fontSize = "12px"
      )
    ) %>%
    
    highcharter::hc_xAxis(
      categories = country_categories,
      reversed = TRUE,
      
      title = list(
        text = ""
      ),
      
      lineWidth = 0,
      tickWidth = 0,
      
      labels = list(
        useHTML = TRUE,
        
        style = list(
          color = "#1F2937",
          fontSize = "12px",
          fontWeight = "500",
          width = "185px",
          textOverflow = "none",
          whiteSpace = "normal"
        )
      )
    ) %>%
    
    highcharter::hc_yAxis(
      min = 0,
      max = y_max,
      
      title = list(
        text = "Average PM₂.₅ concentration (µg/m³)",
        
        style = list(
          color = "#374151",
          fontSize = "12px",
          fontWeight = "700"
        )
      ),
      
      gridLineWidth = 1,
      gridLineColor = "#E9EEF4",
      
      labels = list(
        style = list(
          color = "#475569",
          fontSize = "11px"
        )
      ),
      
      plotLines = list(
        list(
          value = 5,
          color = "#111827",
          dashStyle = "ShortDash",
          width = 1.2,
          zIndex = 5,
          
          label = list(
            text = "WHO guideline: 5",
            rotation = 0,
            align = "left",
            x = 5,
            y = -8,
            
            style = list(
              color = "#111827",
              fontSize = "10px",
              fontWeight = "700"
            )
          )
        )
      )
    ) %>%
    
    highcharter::hc_caption(
      text = paste0(
        "High PM₂.₅ combined with a limited number of active monitors ",
        "may indicate a stronger case for further assessment and monitoring investment."
      ),
      align = "left",
      
      style = list(
        color = "#64748B",
        fontSize = "11px"
      )
    )
  
  # ---------------------------------------------------------
  # Add one bar series for each monitoring category
  # ---------------------------------------------------------
  
  for (level in monitoring_levels) {
    
    level_df <- priority_df[
      MonitoringLevel == level
    ]
    
    if (nrow(level_df) == 0) {
      next
    }
    
    series_data <- highcharter::list_parse(
      level_df[
        ,
        .(
          x = row_position,
          y = PM25,
          name = Country,
          Sensors,
          Awardees,
          Months,
          Coverage,
          PM25,
          MonitoringLevel
        )
      ]
    )
    
    hc <- hc %>%
      
      highcharter::hc_add_series(
        data = series_data,
        type = "bar",
        name = level,
        color = unname(monitoring_colours[level])
      )
  }
  
  hc %>%
    
    highcharter::hc_tooltip(
      useHTML = TRUE,
      
      formatter = highcharter::JS(
        "
        function () {

          const coverage =
            this.point.Coverage === null ||
            this.point.Coverage === undefined ||
            !isFinite(this.point.Coverage)
              ? 'Not available'
              : Highcharts.numberFormat(
                  this.point.Coverage,
                  1
                ) + '%';

          return `
            <div style='
              min-width: 205px;
              font-family: Inter, Arial, sans-serif;
              font-size: 12px;
              line-height: 1.65;
            '>

              <div style='
                font-size: 14px;
                font-weight: 700;
                margin-bottom: 5px;
                color: #111827;
              '>
                ${this.point.name}
              </div>

              <span style='color:#64748B;'>
                Average PM₂.₅:
              </span>
              <b>
                ${Highcharts.numberFormat(
                  this.point.PM25,
                  1
                )} µg/m³
              </b>

              <br/>

              <span style='color:#64748B;'>
                Active monitors:
              </span>
              <b>${this.point.Sensors}</b>

              <br/>

              <span style='color:#64748B;'>
                Data availability:
              </span>
              <b>${coverage}</b>

              <br/>

              <span style='color:#64748B;'>
                Awardees/owners:
              </span>
              <b>${this.point.Awardees}</b>

              <br/>

              <span style='color:#64748B;'>
                Reporting months:
              </span>
              <b>${this.point.Months}</b>

            </div>
          `;
        }
        "
      )
    ) %>%
    
    highcharter::hc_plotOptions(
      bar = list(
        grouping = FALSE,
        pointWidth = 16,
        borderWidth = 0,
        borderRadius = 3,
        
        dataLabels = list(
          enabled = TRUE,
          inside = FALSE,
          align = "left",
          x = 7,
          crop = FALSE,
          overflow = "allow",
          
          formatter = highcharter::JS(
            "
            function () {
              return (
                Highcharts.numberFormat(
                  this.point.PM25,
                  1
                ) +
                '  ·  ' +
                this.point.Sensors +
                ' monitors'
              );
            }
            "
          ),
          
          style = list(
            color = "#475569",
            fontSize = "10px",
            fontWeight = "500",
            textOutline = "none"
          )
        )
      ),
      
      series = list(
        animation = list(
          duration = 350
        ),
        
        states = list(
          inactive = list(
            opacity = 0.3
          )
        )
      )
    ) %>%
    
    # ---------------------------------------------------------
  # Bottom horizontally distributed legend
  # ---------------------------------------------------------
  
  highcharter::hc_legend(
    enabled = TRUE,
    
    layout = "horizontal",
    align = "center",
    verticalAlign = "bottom",
    
    width = "90%",
    x = 0,
    y = 8,
    
    floating = FALSE,
    
    itemWidth = 220,
    itemDistance = 0,
    
    symbolRadius = 4,
    symbolHeight = 10,
    symbolWidth = 10,
    symbolPadding = 7,
    
    padding = 0,
    margin = 12,
    
    itemStyle = list(
      color = "#475569",
      fontSize = "10px",
      fontWeight = "500"
    ),
    
    itemHoverStyle = list(
      color = "#111827"
    )
  ) %>%
    
    highcharter::hc_responsive(
      rules = list(
        list(
          condition = list(
            maxWidth = 720
          ),
          
          chartOptions = list(
            chart = list(
              marginLeft = 125,
              marginRight = 25,
              marginTop = 115,
              marginBottom = 145
            ),
            
            subtitle = list(
              style = list(
                fontSize = "10px"
              )
            ),
            
            xAxis = list(
              labels = list(
                style = list(
                  width = "105px",
                  fontSize = "10px"
                )
              )
            ),
            
            plotOptions = list(
              bar = list(
                pointWidth = 13,
                
                dataLabels = list(
                  enabled = FALSE
                )
              )
            ),
            
            legend = list(
              layout = "horizontal",
              align = "center",
              verticalAlign = "bottom",
              width = "100%",
              x = 0,
              y = 5,
              itemWidth = 165,
              itemDistance = 0
            )
          )
        )
      )
    ) %>%
    
    highcharter::hc_credits(
      enabled = FALSE
    ) %>%
    
    highcharter::hc_exporting(
      enabled = TRUE
    )
})

# output$funding_priority_bubble <- highcharter::renderHighchart({
#   
#   dt <- data.table::as.data.table(sensor_data)
#   
#   dt <- dt[
#     year %in% c(2025, 2026)
#   ]
#   
#   dt <- dt[
#     year %in% input$year_heatmp
#   ]
#   
#   priority_df <- dt[
#     ,
#     .(
#       Sensors = data.table::uniqueN(sensors_id),
#       Awardees = data.table::uniqueN(owner),
#       Months = data.table::uniqueN(paste(year, month)),
#       Coverage = round(mean(coverage.percentComplete, na.rm = TRUE), 1),
#       PM25 = round(mean(pm25, na.rm = TRUE), 2)
#     ),
#     by = .(Country = name0)
#   ]
#   
#   priority_df <- priority_df[
#     !is.na(Country) &
#       !is.na(Sensors) &
#       Sensors > 0 &
#       !is.na(PM25)
#   ]
#   
#   shiny::validate(
#     shiny::need(
#       nrow(priority_df) > 0,
#       "No funding priority data available for selected filters."
#     )
#   )
#   
#   priority_df[
#     ,
#     `:=`(
#       x = Sensors,
#       y = PM25,
#       z = pmax(Coverage, 40),
#       name = Country,
#       color = vapply(PM25, aqli_pm_color, character(1))
#     )
#   ]
#   
#   # ---------------------------------------------------------
#   # X-axis padding for logarithmic axis
#   # ---------------------------------------------------------
#   
#   min_sensors <- min(priority_df$Sensors, na.rm = TRUE)
#   max_sensors <- max(priority_df$Sensors, na.rm = TRUE)
#   
#   x_min <- max(0.5, min_sensors / 1.6)
#   x_max <- max_sensors * 1.25
#   
#   bubble_data <- highcharter::list_parse(
#     priority_df[
#       ,
#       .(
#         x,
#         y,
#         z,
#         name,
#         color,
#         Sensors,
#         Awardees,
#         Months,
#         Coverage,
#         PM25
#       )
#     ]
#   )
#   
#   highcharter::highchart() %>%
#     
#     highcharter::hc_chart(
#       type = "bubble",
#       zoomType = "xy",
#       height = 470,
#       spacingBottom = 20,
#       spacingTop = 18,
#       spacingRight = 24,
#       spacingLeft = 10,
#       style = list(
#         fontFamily = "Montserrat, Arial, sans-serif"
#       )
#     ) %>%
#     
#     highcharter::hc_title(
#       text = "Funding Priority Matrix",
#       align = "center",
#       style = list(
#         fontWeight = "500",
#         color = "#111827",
#         fontSize = "18px"
#       )
#     ) %>%
#     
#     highcharter::hc_caption(
#       text = "Bubble size represents average data availability <br>Countries with high PM₂.₅ and limited sensor coverage may represent stronger opportunities for future monitoring investment.",
#       align = "left",
#       style = list(
#         color = "#64748b",
#         fontSize = "13px",
#         fontFamily = "Montserrat, Arial, sans-serif"
#       )
#     ) %>%
#     
#     highcharter::hc_xAxis(
#       type = "logarithmic",
#       
#       title = list(
#         text = active_sensor_info_html("Active Monitors (logarithmic scale)", axis = TRUE),
#         useHTML = TRUE,
#         style = list(
#           fontWeight = "700",
#           color = "#374151"
#         )
#       ),
#       
#       min = x_min,
#       max = x_max,
#       
#       startOnTick = FALSE,
#       endOnTick = FALSE,
#       minPadding = 0.08,
#       maxPadding = 0.12,
#       
#       gridLineWidth = 1,
#       gridLineColor = "#eef2f7",
#       
#       labels = list(
#         style = list(
#           color = "#334155"
#         )
#       )
#     ) %>%
#     
#     highcharter::hc_yAxis(
#       title = list(
#         text = "Average PM₂.₅ (µg/m³)",
#         style = list(
#           fontWeight = "700",
#           color = "#374151"
#         )
#       ),
#       
#       min = 0,
#       maxPadding = 0.15,
#       
#       gridLineWidth = 1,
#       gridLineColor = "#eef2f7",
#       
#       labels = list(
#         style = list(
#           color = "#334155"
#         )
#       ),
#       
#       plotLines = list(
#         list(
#           value = 5,
#           color = "#111827",
#           dashStyle = "ShortDash",
#           width = 1.2,
#           zIndex = 4,
#           label = list(
#             text = "WHO guideline: 5 µg/m³",
#             align = "right",
#             x = -4,
#             y = -6,
#             style = list(
#               color = "#111827",
#               fontSize = "11px",
#               fontWeight = "700"
#             )
#           )
#         )
#       )
#     ) %>%
#     
#     highcharter::hc_add_series(
#       data = bubble_data,
#       name = "Countries",
#       type = "bubble",
#       minSize = 22,
#       maxSize = 58
#     ) %>%
#     
#     highcharter::hc_tooltip(
#       useHTML = TRUE,
#       formatter = highcharter::JS(
#         "
#         function () {
#           return `
#             <div style='font-family:Inter, Arial, sans-serif; font-size:12px; line-height:1.6;'>
#               <b style='font-size:14px;'>${this.point.name}</b><br/>
#               <span style='color:#64748b;'>Active monitors:</span> <b>${this.point.Sensors}</b><br/>
#               <span style='color:#64748b;'>Average PM₂.₅:</span> <b>${this.point.PM25} µg/m³</b><br/>
#               <span style='color:#64748b;'>Data Availability:</span> <b>${this.point.Coverage}%</b><br/>
#               <span style='color:#64748b;'>Awardees Name:</span> <b>${this.point.Awardees}</b><br/>
#               <span style='color:#64748b;'>Reporting months:</span> <b>${this.point.Months}</b>
#             </div>
#           `;
#         }
#         "
#       )
#     ) %>%
#     
#     highcharter::hc_plotOptions(
#       bubble = list(
#         marker = list(
#           lineColor = "#ffffff",
#           lineWidth = 2,
#           fillOpacity = 0.9
#         )
#       ),
#       series = list(
#         dataLabels = list(
#           enabled = FALSE
#         )
#       )
#     ) %>%
#     
#     highcharter::hc_legend(
#       enabled = FALSE
#     ) %>%
#     
#     highcharter::hc_credits(
#       enabled = FALSE
#     ) %>%
#     
#     highcharter::hc_exporting(
#       enabled = TRUE
#     )
# })
# output$funding_priority_bubble <- highcharter::renderHighchart({
#   
#   dt <- data.table::as.data.table(sensor_data)
#   
#   dt <- dt[
#     year %in% c(2025, 2026)
#   ]
#   
#   
#   dt <- dt[
#     year %in% input$year_heatmp
#   ]
#  
#   
#   priority_df <- dt[
#     ,
#     .(
#       Sensors = data.table::uniqueN(sensors_id),
#       Awardees = data.table::uniqueN(owner),
#       Months = data.table::uniqueN(paste(year, month)),
#       Coverage = round(mean(coverage.percentComplete, na.rm = TRUE), 1),
#       PM25 = round(mean(pm25, na.rm = TRUE), 2)
#     ),
#     by = .(Country = name0)
#   ]
#   
#   priority_df <- priority_df[
#     !is.na(Country) &
#       !is.na(Sensors) &
#       Sensors > 0 &
#       !is.na(PM25)
#   ]
#   
#   priority_df[
#     ,
#     `:=`(
#       x = Sensors,
#       y = PM25,
#       z = pmax(Coverage, 40),
#       name = Country,
#       color = vapply(PM25, aqli_pm_color, character(1))
#     )
#   ]
#   
#   max_sensors <- max(priority_df$Sensors, na.rm = TRUE)
#   x_gap <- max_sensors * 0.08
#   
#   bubble_data <- highcharter::list_parse(
#     priority_df[
#       ,
#       .(
#         x,
#         y,
#         z,
#         name,
#         color,
#         Sensors,
#         Awardees,
#         Months,
#         Coverage,
#         PM25
#       )
#     ]
#   )
#   
#   highcharter::highchart() %>%
#     
#     highcharter::hc_chart(
#       type = "bubble",
#       zoomType = "xy",
#       height = 470,
#     #  backgroundColor = "#ffffff",
#       spacingTop = 18,
#       spacingRight = 24,
#       spacingBottom = 20,
#       spacingLeft = 10,
#       style = list(
#         fontFamily = "Montserrat, Arial, sans-serif"
#       )
#     ) %>% 
#     
#     highcharter::hc_title(
#       text = "Funding Priority Matrix",
#       align = "center",
#       style = list(
#         fontWeight = "500",
#         color = "#111827",
#         fontSize = "18px"
#       )
#     ) %>%
#     
#     # highcharter::hc_subtitle(
#     #   text = "Countries with high PM₂.₅ and limited sensor coverage may represent stronger opportunities for future monitoring investment.",
#     #   align = "center",
#     #   style = list(
#     #     color = "#64748b",
#     #     fontSize = "13px"
#     #   )
#     # ) %>%
#     highcharter::hc_caption(
#       text = "Bubble size represents average data coverage. <br>Countries with high PM₂.₅ and limited sensor coverage may represent stronger opportunities for future monitoring investment.",
#       align = "left",
#       style = list(
#         color = "#64748b",
#         fontSize = "13px",
#         fontFamily = "Montserrat, Arial, sans-serif"
#       )
#     ) %>%
#     
#     highcharter::hc_xAxis(
#       type = "logarithmic",
#       
#       title = list(
#         text = active_sensor_info_html("Active Sensors", axis = TRUE),
#         useHTML = TRUE,
#         style = list(
#           fontWeight = "700",
#           color = "#374151"
#         )
#       ),
#       
#       min = 1,
#       max = max_sensors * 1.15,
#       startOnTick = FALSE,
#       endOnTick = FALSE,
#       
#       gridLineWidth = 1,
#       gridLineColor = "#eef2f7",
#       
#       labels = list(
#         style = list(
#           color = "#334155"
#         )
#       )
#     ) %>%    
#     highcharter::hc_yAxis(
#       title = list(
#         text = "Average PM₂.₅ (µg/m³)",
#         style = list(
#           fontWeight = "700",
#           color = "#374151"
#         )
#       ),
#       
#       min = 0,
#       maxPadding = 0.15,
#       
#       gridLineWidth = 1,
#       gridLineColor = "#eef2f7",
#       
#       labels = list(
#         style = list(
#           color = "#334155"
#         )
#       ),
#       
#       plotLines = list(
#         list(
#           value = 5,
#           color = "#111827",
#           dashStyle = "ShortDash",
#           width = 1.2,
#           zIndex = 4,
#           label = list(
#             text = "WHO guideline: 5 µg/m³",
#             align = "right",
#             x = -4,
#             y = -6,
#             style = list(
#               color = "#111827",
#               fontSize = "11px",
#               fontWeight = "700"
#             )
#           )
#         )
#       )
#     ) %>%
#     
#     highcharter::hc_add_series(
#       data = bubble_data,
#       name = "Countries",
#       type = "bubble",
#       minSize = 22,
#       maxSize = 58
#     ) %>%
#     
#     highcharter::hc_tooltip(
#       useHTML = TRUE,
#       formatter = highcharter::JS(
#         "
#         function () {
#           return `
#             <div style='font-family:Inter, Arial, sans-serif; font-size:12px; line-height:1.6;'>
#               <b style='font-size:14px;'>${this.point.name}</b><br/>
#               <span style='color:#64748b;'>Active sensors:</span> <b>${this.point.Sensors}</b><br/>
#               <span style='color:#64748b;'>Average PM₂.₅:</span> <b>${this.point.PM25} µg/m³</b><br/>
#               <span style='color:#64748b;'>Data coverage:</span> <b>${this.point.Coverage}%</b><br/>
#               <span style='color:#64748b;'>Awardees:</span> <b>${this.point.Awardees}</b><br/>
#               <span style='color:#64748b;'>Reporting months:</span> <b>${this.point.Months}</b>
#             </div>
#           `;
#         }
#         "
#       )
#     ) %>%
#     
#     highcharter::hc_plotOptions(
#       bubble = list(
#         marker = list(
#           lineColor = "#ffffff",
#           lineWidth = 2,
#           fillOpacity = 0.9
#         )
#       ),
#       series = list(
#         dataLabels = list(
#           enabled = FALSE
#         )
#       )
#     ) %>%
#     
#     highcharter::hc_legend(
#       enabled = FALSE
#     ) %>%
#     
#     highcharter::hc_credits(
#       enabled = FALSE
#     ) %>%
#     
#     highcharter::hc_exporting(
#       enabled = TRUE
#     )
# })



output$country_month_heatmap <- highcharter::renderHighchart({
  
  month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  
  # ---------------------------------------------------------
  # Fixed Country Order By Hemisphere
  # ---------------------------------------------------------
  
  northern_countries <- c(
    "Pakistan",
    "Nepal",
    "Nigeria",
    "Gambia",
    "Ghana",
    "Liberia",
    "Cameroon",
    "Lebanon",
    "Honduras",
    "Cote d'Ivoire",
    "Bhutan",
    "Burkina Faso"
  )
  
  equatorial_countries <- c(
    "Democratic Republic of the Congo",
    "Uganda"
  )
  
  southern_countries <- c(
    "Malawi",
    "Argentina",
    "Zambia",
    "Mozambique",
    "Botswana"
  )
  
  country_order <- c(
    northern_countries,
    equatorial_countries,
    southern_countries
  )
  
  hemisphere_lookup <- data.frame(
    name0 = country_order,
    hemisphere = c(
      rep("Northern Hemisphere", length(northern_countries)),
      rep("Equatorial / Mixed", length(equatorial_countries)),
      rep("Southern Hemisphere", length(southern_countries))
    ),
    stringsAsFactors = FALSE
  )
  
  # ---------------------------------------------------------
  # Prepare Data
  # ---------------------------------------------------------
  
  heat_df <- openaq_month_trend_c %>%
    dplyr::filter(
      year %in% input$year_heatmp,
      name0 %in% country_order,
      !is.na(name0)
    ) %>%
    dplyr::mutate(
      month_name = dplyr::case_when(
        "month_name" %in% names(openaq_month_trend_c) ~ as.character(month_name),
        "month" %in% names(openaq_month_trend_c) & is.numeric(month) ~ month.abb[month],
        "month" %in% names(openaq_month_trend_c) ~ as.character(month),
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(month_name)) %>%
    dplyr::group_by(name0, month_name) %>%
    dplyr::summarise(
      pm25_avg = round(mean(pm25, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      month_name = factor(month_name, levels = month_levels)
    ) %>%
    dplyr::filter(!is.na(month_name))
  
  shiny::validate(
    shiny::need(nrow(heat_df) > 0, "No heatmap data available.")
  )
  
  # ---------------------------------------------------------
  # Complete Missing Country-Month Cells Using Fixed Order
  # ---------------------------------------------------------
  
  heat_df <- heat_df %>%
    tidyr::complete(
      name0 = country_order,
      month_name = factor(month_levels, levels = month_levels),
      fill = list(pm25_avg = NA_real_)
    ) %>%
    dplyr::left_join(
      hemisphere_lookup,
      by = "name0"
    ) %>%
    dplyr::mutate(
      country_id = match(name0, country_order) - 1,
      month_id = match(as.character(month_name), month_levels) - 1
    ) %>%
    dplyr::arrange(country_id, month_id)
  
  heat_data <- highcharter::list_parse(
    heat_df %>%
      dplyr::transmute(
        x = month_id,
        y = country_id,
        value = pm25_avg,
        country = name0,
        hemisphere = hemisphere,
        month = as.character(month_name)
      )
  )
  
  # ---------------------------------------------------------
  # Hemisphere Divider Lines + Labels
  # ---------------------------------------------------------
  
  northern_count <- length(northern_countries)
  equatorial_count <- length(equatorial_countries)
  
  y_plot_lines <- list(
    list(
      value = -0.5,
      color = "transparent",
      width = 0,
      zIndex = 5,
      label = list(
        text = "Northern Hemisphere",
        align = "left",
        x = 6,
        y = 14,
        style = list(
          color = "#111827",
          fontSize = "11px",
          fontWeight = "800",
          fontFamily = "Montserrat, Arial, sans-serif"
        )
      )
    ),
    list(
      value = northern_count - 0.5,
      color = "#111827",
      width = 1,
      dashStyle = "ShortDash",
      zIndex = 5,
      label = list(
        text = "Equatorial / Mixed",
        align = "left",
        x = 6,
        y = 14,
        style = list(
          color = "#111827",
          fontSize = "11px",
          fontWeight = "800",
          fontFamily = "Montserrat, Arial, sans-serif"
        )
      )
    ),
    list(
      value = northern_count + equatorial_count - 0.5,
      color = "#111827",
      width = 1,
      dashStyle = "ShortDash",
      zIndex = 5,
      label = list(
        text = "Southern Hemisphere",
        align = "left",
        x = 6,
        y = 14,
        style = list(
          color = "#111827",
          fontSize = "11px",
          fontWeight = "800",
          fontFamily = "Montserrat, Arial, sans-serif"
        )
      )
    )
  )
  
  # ---------------------------------------------------------
  # Highchart
  # ---------------------------------------------------------
  
  highcharter::highchart() %>%
    
    highcharter::hc_chart(
      type = "heatmap",
      height = 670,
      spacingTop = 25,
      spacingRight = 28,
      spacingBottom = 45,
      spacingLeft = 25,
      animation = FALSE,
      style = list(
        fontFamily = "Montserrat, Arial, sans-serif"
      )
    ) %>%
    
    highcharter::hc_title(
      text = "Country-Month PM<sub>2.5</sub> Concentration Heatmap",
      useHTML = TRUE,
      align = "center",
      style = list(
        fontFamily = "Montserrat, Arial, sans-serif",
        fontWeight = "500",
        color = "#111827",
        fontSize = "18px"
      )
    ) %>%
    
    highcharter::hc_caption(
      text = "Rows are grouped by hemisphere in a fixed order. Columns represent months,<br> and cell color indicates the average PM2.5 concentration.",
      align = "left",
      style = list(
        color = "#64748b",
        fontSize = "13px",
        fontFamily = "Montserrat, Arial, sans-serif"
      )
    ) %>%    
    highcharter::hc_xAxis(
      categories = month_levels,
      title = list(text = NULL),
      opposite = TRUE,
      labels = list(
        style = list(
          fontFamily = "Montserrat, Arial, sans-serif",
          color = "#334155",
          fontSize = "12px",
          fontWeight = "600"
        )
      )
    ) %>%
    
    highcharter::hc_yAxis(
      categories = country_order,
      title =list(
        text = "Country",
        style = list(
          fontWeight = "700",
          color = "#374151"
        )
      ),
      
      reversed = TRUE,
      plotLines = y_plot_lines,
      labels = list(
        useHTML = TRUE,
        formatter = highcharter::JS(
          "
          function () {
            var text = this.value;

            if (text.length > 22) {
              var words = text.split(' ');
              var line1 = '';
              var line2 = '';

              for (var i = 0; i < words.length; i++) {
                if ((line1 + ' ' + words[i]).trim().length <= 22) {
                  line1 = (line1 + ' ' + words[i]).trim();
                } else {
                  line2 = (line2 + ' ' + words[i]).trim();
                }
              }

              return '<span style=\"display:block; line-height:13px; text-align:right;\">' +
                       line1 + '<br/>' + line2 +
                     '</span>';
            }

            return '<span style=\"display:block; text-align:right;\">' + text + '</span>';
          }
          "
        ),
        style = list(
          fontFamily = "Montserrat, Arial, sans-serif",
          color = "#334155",
          fontSize = "12px",
          fontWeight = "600",
          width = "170px"
        )
      )
    ) %>%
    
    highcharter::hc_colorAxis(
      dataClasses = list(
        list(
          from = 0,
          to = 12,
          color = "#7cb342",
          name = "≤ 12"
        ),
        list(
          from = 12,
          to = 35,
          color = "#fdd835",
          name = "12–35"
        ),
        list(
          from = 35,
          to = 55,
          color = "#fb8c00",
          name = "35–55"
        ),
        list(
          from = 55,
          to = 150,
          color = "#e53935",
          name = "55–150"
        ),
        list(
          from = 150,
          color = "#8e0000",
          name = "> 150"
        )
      )
    ) %>%
    
    highcharter::hc_add_series(
      name = "Average PM₂.₅",
      data = heat_data,
      type = "heatmap",
      borderWidth = 1,
      borderColor = "#ffffff",
      nullColor = "#f1f5f9",
      animation = FALSE,
      states = list(
        hover = list(
          enabled = FALSE,
          brightness = 0,
          borderColor = "#ffffff"
        ),
        inactive = list(
          enabled = FALSE,
          opacity = 1
        )
      )
    ) %>%
    
    highcharter::hc_tooltip(
      useHTML = TRUE,
      formatter = highcharter::JS(
        "
        function () {
          if (this.point.value === null || this.point.value === undefined) {
            return `
              <div style='font-family:Montserrat, Arial, sans-serif; font-size:12px; line-height:1.6;'>
                <b>${this.point.country}</b><br/>
                <span style='color:#64748b;'>Hemisphere:</span> <b>${this.point.hemisphere}</b><br/>
                <span style='color:#64748b;'>Month:</span> <b>${this.point.month}</b><br/>
                <span style='color:#64748b;'>Average PM₂.₅:</span> <b>No data</b>
              </div>
            `;
          }

          return `
            <div style='font-family:Montserrat, Arial, sans-serif; font-size:12px; line-height:1.6;'>
              <b>${this.point.country}</b><br/>
              <span style='color:#64748b;'>Hemisphere:</span> <b>${this.point.hemisphere}</b><br/>
              <span style='color:#64748b;'>Month:</span> <b>${this.point.month}</b><br/>
              <span style='color:#64748b;'>Average PM₂.₅:</span> <b>${this.point.value.toFixed(1)} µg/m³</b>
            </div>
          `;
        }
        "
      )
    ) %>%
    
    highcharter::hc_plotOptions(
      series = list(
        animation = FALSE,
        stickyTracking = FALSE,
        states = list(
          hover = list(
            enabled = FALSE,
            brightness = 0,
            halo = list(size = 0)
          ),
          inactive = list(
            enabled = FALSE,
            opacity = 1
          )
        )
      ),
      heatmap = list(
        animation = FALSE,
        borderWidth = 1,
        borderColor = "#ffffff",
        dataLabels = list(
          enabled = FALSE
        ),
        states = list(
          hover = list(
            enabled = FALSE,
            brightness = 0,
            borderColor = "#ffffff"
          ),
          inactive = list(
            enabled = FALSE,
            opacity = 1
          )
        )
      )
    ) %>%
    
    highcharter::hc_legend(
      align = "center",
      verticalAlign = "bottom",
      layout = "horizontal",
      itemStyle = list(
        fontFamily = "Montserrat, Arial, sans-serif",
        fontSize = "11px",
        color = "#374151"
      )
    ) %>%
    
    highcharter::hc_credits(enabled = FALSE) %>%
    highcharter::hc_exporting(enabled = TRUE)
})
