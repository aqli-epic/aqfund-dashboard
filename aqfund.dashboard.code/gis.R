


rv_country_gis      <- reactiveValues()
rv_state_gis        <- reactiveValues()
rv_district_gis     <- reactiveValues()
rv_year_gis         <- reactiveValues()



isolate({
  rv_country_gis$country_gis       <- input$country_gis
  rv_state_gis$state_gis           <- input$state_gis
  rv_district_gis$district_gis     <- input$district_gis
  rv_year_gis$year_gis                 <- input$year_gis
})

observeEvent(rv_country_gis$country_gis, {
  updatePickerInput(session,
                    inputId = "state_gis",
                    label = "Select State(s):",
                    choices  = unique(sort(sensor_data[name0 %in% rv_country_gis$country_gis, name1])),
                    selected = unique(sort(sensor_data[name0 %in% rv_country_gis$country_gis, name1])))
})


# observeEvent(rv_state_gis$state_gis, {
#   updatePickerInput(session,
#                     inputId = "district_gis",
#                     label = "Select district(s):",
#                     choices  = unique(sort(sensor_data[name1 %in% rv_state_gis$state_gis, name2])),
#                     selected = unique(sort(sensor_data[name1 %in% rv_state_gis$state_gis, name2])))
# })


observeEvent(rv_country_gis$country_gis, {
  updatePickerInput(session,
                    inputId = "year_gis",
                    label = "Select year(s):",
                    choices  = 2025:2026,#unique(sort(sensor_data[country %in% input$country, year])),
                    selected = 2025)#unique(sort(sensor_data[country %in% input$country, year]))[1])
})



observe({
  
  
  if(!isTRUE(input$country_gis_open) & !isTRUE(input$state_gis_open) & !isTRUE(input$district_gis_open) & !isTRUE(input$year_gis_open))
    
  {
    
    rv_country_gis$country_gis       <- input$country_gis
    rv_state_gis$state_gis           <- input$state_gis
    rv_district_gis$district_gis     <- input$district_gis
    rv_year_gis$year_gis             <- input$year_gis
    
  }
  
})


      all_data <- reactive({
        
        # After first launch: only on button click
        # req(input$country_gis, input$year_gis)
        # print(input$country)
        sensor_data[
          name0 %in% input$country_gis &
            name1 %in% input$state_gis &
          #  name2 %in% input$district_gis &
            year %in% input$year_gis
        ]
        
      })



      gis_data_state <- reactive({
        
        # After first launch: only on button click
       # req(input$country_gis, input$year_gis)
        # print(input$country)
        openaq_data_state[
          name0 %in% input$country_gis &
            name1 %in% input$state_gis &
           # name2 %in% input$district_gis &
            year %in% input$year_gis
        ]
        
      })

      gis_data_district <- reactive({
        
        # After first launch: only on button click
        # req(input$country_gis, input$year_gis)
        # print(input$country)
        openaq_data_district[
          name0 %in% input$country_gis &
            name1 %in% input$state_gis &
         #   name2 %in% input$district_gis &
            year %in% input$year_gis
        ]
        
      })
    
      # openaq_month_filr <- reactive({
      #   
      #   # After first launch: only on button click
      #   # req(input$country_gis, input$year_gis)
      #   # print(input$country)
      #   openaq_month_trend[
      #     name0 %in% input$country_gis &
      #       # name1 %in% input$state_gis &
      #       # name2 %in% input$district_gis &
      #       year %in% input$year_gis
      #   ]
      #   
      # })  


      
#########################
      kpi_values <- reactive({
        
        dt <- data.table::as.data.table(all_data())
        
        list(
          countries = data.table::uniqueN(sensor_data$name0),
          sensors = data.table::uniqueN(dt$sensors_id),
          owners = data.table::uniqueN(dt$group_name),
          avg_pm = round(mean(dt$pm25, na.rm = TRUE), 0),
          data_cov = round(mean(dt$coverage.percentComplete, na.rm = TRUE), 0)
        )
      })
      output$vb_countries <- renderText({ length(unique(sensor_data$name0)) })
      output$vb_sensor <- renderText({ length(unique(all_data()$sensors_id)) })
      output$vb_owner <- renderText({ length(unique(all_data()$group_name)) })
      output$vb_avg_pm <- renderText({ round(mean(all_data()$pm25, na.rm=T),0)})
      output$vb_data_cov <- renderText({ round(mean(all_data()$coverage.percentComplete, na.rm=T),0)})
      
      
      
##########################      
      
      

font.size <- "8pt"

opts1 <- list(
  
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#008000', 'color': '#fff'});",
    
    paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
    
    "}"),
  
  searchHighlight = TRUE,
  # columnDefs = list(list(targets = c(1:10), searchable = FALSE)),
  pageLength = 10,
  lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')),
  # dom = 't',
  scrollX = TRUE,
  scrollY = 300,
  scroller = TRUE,
  fixedColumns = TRUE,
  # buttons = c('copy', 'csv', 'excel'),
  buttons = list(list(extend = c('excel'), filename= paste0("pm25_data_",Sys.time()))),
  
  dom = 'lfrtiBp'
  
)
 

#############################################################################################
# =========================================================
# MAP HELPERS
# =========================================================
# =========================================================
# MAP HELPERS
# =========================================================

# =========================================================
# MAP HELPERS
# =========================================================

safe_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

get_year_col <- function(df, year_input) {
  
  yr <- as.character(year_input[1])
  
  if (yr %in% names(df)) {
    return(yr)
  }
  
  yr_x <- paste0("X", yr)
  
  if (yr_x %in% names(df)) {
    return(yr_x)
  }
  
  stop(paste("Year column not found:", yr, "or", yr_x))
}

trim_leaflet_sf <- function(data, cols) {
  dplyr::select(data, dplyr::any_of(cols))
}

filter_selected_states <- function(data) {
  selected_states <- input$state_gis

  if (
    is.null(selected_states) ||
      length(selected_states) == 0 ||
      any(is.na(selected_states)) ||
      any(trimws(selected_states) == "")
  ) {
    return(data)
  }

  if (!any(selected_states %in% data$name1)) {
    return(data)
  }

  dplyr::filter(data, name1 %in% selected_states)
}

first_non_missing <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]

  if (length(x) == 0) {
    return(NA_character_)
  }

  x[[1]]
}

pm25_bins <- c(0, 15, 30, 45, 60, 90, Inf)

pm25_palette <- c(
  "#e0f7fa",
  "#9adbe8",
  "#5cc4d6",
  "#2f9bb8",
  "#1f6f9a",
  "#0f3d73"
)

sat_bins <- c(0, 10, 25, 35, 50, 60, Inf)

sat_palette <- c(
  "#b7ebf1",
  "#8fd8e4",
  "#3db1c8",
  "#3f8dac",
  "#416891",
  "#434475",
  "#451f59"
)

pm25_legend_html <- htmltools::HTML("
  <div style='
    padding:10px 12px;
    background:rgba(255,255,255,0.92);
    font-family:Arial, sans-serif;
    font-size:12px;
    border-radius:6px;
    box-shadow:0 1px 5px rgba(0,0,0,0.25);
  '>
    <b>PM<sub>2.5</sub> (µg/m³)</b>

    <div style='display:flex; flex-wrap:wrap; gap:6px 10px; margin-top:6px;'>
      <div><span style='display:inline-block;width:18px;height:10px;background:#e0f7fa;'></span> 0–15</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#9adbe8;'></span> 15–30</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#5cc4d6;'></span> 30–45</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#2f9bb8;'></span> 45–60</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#1f6f9a;'></span> 60–90</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#0f3d73;'></span> 90+</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#d9d9d9;'></span> No data</div>
    </div>
  </div>
")

sat_legend_html <- htmltools::HTML("
  <div style='
    padding:10px 12px;
    background:rgba(255,255,255,0.92);
    font-family:Arial, sans-serif;
    font-size:12px;
    border-radius:6px;
    box-shadow:0 1px 5px rgba(0,0,0,0.25);
  '>
    <b>Satellite PM<sub>2.5</sub> (µg/m³)</b>

    <div style='display:flex; flex-wrap:wrap; gap:6px 10px; margin-top:6px;'>
      <div><span style='display:inline-block;width:18px;height:10px;background:#b7ebf1;'></span> 0–10</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#8fd8e4;'></span> 10–25</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#3db1c8;'></span> 25–35</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#3f8dac;'></span> 35–50</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#416891;'></span> 50–60</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:#434475;'></span> 60+</div>
      <div><span style='display:inline-block;width:18px;height:10px;background:grey;'></span> No data</div>
    </div>
  </div>
")


# =========================================================
# CACHED GROUND MONITORING DATA
# =========================================================

pm25_state_map_data <- reactive({
  
  req(input$country_gis, input$year_gis)
  
  yr <- input$year_gis[1]
  
  file <- paste0(
    "data/map_cache/",
    safe_name(input$country_gis),
    "_",
    yr,
    "_state_pm25.qs2"
  )
  
  validate(
    need(file.exists(file), paste("Missing state PM2.5 cache file:", file))
  )
  
  read_map_qs(file)
})


pm25_district_map_data <- reactive({
  
  req(input$country_gis, input$year_gis)
  
  yr <- input$year_gis[1]
  
  file <- paste0(
    "data/map_cache/",
    safe_name(input$country_gis),
    "_",
    yr,
    "_district_pm25.qs2"
  )
  
  validate(
    need(file.exists(file), paste("Missing district PM2.5 cache file:", file))
  )
  
  read_map_qs(file)
})


# =========================================================
# CACHED SATELLITE DATA
# =========================================================

satellite_state_map_data <- reactive({
  
  req(input$country_gis)
  
  file <- paste0(
    "data/map_cache_satellite/",
    safe_name(input$country_gis),
    "_state_satellite.qs2"
  )
  
  validate(
    need(file.exists(file), paste("Missing satellite state cache file:", file))
  )
  
  read_map_qs(file)
})


satellite_district_map_data <- reactive({
  
  req(input$country_gis)
  
  file <- paste0(
    "data/map_cache_satellite/",
    safe_name(input$country_gis),
    "_district_satellite.qs2"
  )
  
  validate(
    need(file.exists(file), paste("Missing satellite district cache file:", file))
  )
  
  read_map_qs(file)
})


# =========================================================
# MAIN MAP: STATE LAYER ONLY
# Panes keep sensors always clickable above polygons
# =========================================================

output$country_wise_pm <- leaflet::renderLeaflet({
  
  req(input$switch_btn, input$country_gis)
  
  # ---------------------------------------------------------
  # Ground Monitoring PM2.5
  # ---------------------------------------------------------
  
  if (input$switch_btn == "pm25") {
    
    req(input$year_gis)
    
    yr <- input$year_gis[1]
    
    data_df_state <- pm25_state_map_data() %>%
      trim_leaflet_sf(c("name1", "dis_pm")) %>%
      dplyr::mutate(
        label_text_state = sprintf(
          "
          <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
            <b style='font-size:14px; color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>Year: </span>
            <b style='color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>PM<sub>2.5</sub> Concentration:</span><br/>
            <b style='color:#e67e22;'>%s</b>
          </div>
          ",
          name1,
          yr,
          ifelse(
            is.na(dis_pm),
            "No data",
            paste0(sprintf("%.1f", dis_pm), " µg/m³")
          )
        )
      )
    
    pal_state <- leaflet::colorBin(
      palette = pm25_palette,
      domain = data_df_state$dis_pm,
      bins = pm25_bins,
      na.color = "#d9d9d9"
    )
    
    leaflet::leaflet(
      options = leaflet::leafletOptions(preferCanvas = TRUE)
    ) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      
      leaflet::addMapPane("statePane", zIndex = 410) %>%
      leaflet::addMapPane("districtPane", zIndex = 430) %>%
      leaflet::addMapPane("sensorPane", zIndex = 650) %>%
      
      leaflet::addPolygons(
        data = data_df_state,
        group = "State",
        layerId = ~name1,
        fillColor = ~pal_state(dis_pm),
        weight = 0.8,
        opacity = 1,
        color = "#ffffff",
        fillOpacity = 0.8,
        
        options = leaflet::pathOptions(
          pane = "statePane",
          smoothFactor = 1
        ),
        
        label = lapply(data_df_state$label_text_state, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            "padding" = "4px 8px",
            "border-radius" = "4px"
          ),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "#333333",
          fillOpacity = 0.9,
          bringToFront = FALSE
        )
      ) %>%
      
      leaflet::addControl(
        html = pm25_legend_html,
        position = "bottomleft"
      ) %>%
      
      leaflet.extras::addFullscreenControl(
        position = "topleft",
        pseudoFullscreen = FALSE
      )
  }
  
  # ---------------------------------------------------------
  # Satellite PM2.5
  # ---------------------------------------------------------
  
  else {
    
    req(input$year_gis_aqli)
    
    data_df_state <- satellite_state_map_data()
    
    year_col <- get_year_col(data_df_state, input$year_gis_aqli)
    
    data_df_state <- data_df_state %>%
      trim_leaflet_sf(c("name1", "population", year_col)) %>%
      dplyr::mutate(
        pm_value = .data[[year_col]],
        label_text_state = sprintf(
          "
          <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
            <b style='font-size:14px;color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>Population: </span>
            <b style='color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>Satellite PM<sub>2.5</sub> Concentration:</span><br/>
            <b style='color:#e67e22;'>%s</b>
          </div>
          ",
          name1,
          formatC(population, format = "f", big.mark = ",", digits = 0),
          ifelse(
            is.na(pm_value),
            "No data",
            paste0(sprintf("%.1f", pm_value), " µg/m³")
          )
        )
      )
    
    pal_state <- leaflet::colorBin(
      palette = sat_palette,
      domain = data_df_state$pm_value,
      bins = sat_bins,
      na.color = "grey"
    )
    
    leaflet::leaflet(
      options = leaflet::leafletOptions(preferCanvas = TRUE)
    ) %>%
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
      
      leaflet::addMapPane("statePane", zIndex = 410) %>%
      leaflet::addMapPane("districtPane", zIndex = 430) %>%
      leaflet::addMapPane("sensorPane", zIndex = 650) %>%
      
      leaflet::addPolygons(
        data = data_df_state,
        group = "State",
        layerId = ~name1,
        fillColor = ~pal_state(pm_value),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        
        options = leaflet::pathOptions(
          pane = "statePane",
          smoothFactor = 1
        ),
        
        label = lapply(data_df_state$label_text_state, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            "padding" = "3px 8px"
          ),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = FALSE
        )
      ) %>%
      
      leaflet::addControl(
        html = sat_legend_html,
        position = "bottomleft"
      ) %>%
      
      leaflet.extras::addFullscreenControl(
        position = "topleft",
        pseudoFullscreen = FALSE
      )
  }
})


# =========================================================
# LOAD DISTRICTS ON DEMAND
# District layer stays below sensors because of districtPane
# =========================================================

observeEvent(input$load_district_layer, {
  
  req(input$switch_btn, input$country_gis)
  
  # ---------------------------------------------------------
  # Ground Monitoring Districts
  # ---------------------------------------------------------
  
  if (input$switch_btn == "pm25") {
    
    req(input$year_gis)
    
    yr <- input$year_gis[1]
    
    data_df_district <- pm25_district_map_data() %>%
      trim_leaflet_sf(c("name1", "name2", "dis_pm")) %>%
      filter_selected_states() %>%
      dplyr::mutate(
        label_text_district = sprintf(
          "
          <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
            <b style='font-size:14px; color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>Year: </span>
            <b style='color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>PM<sub>2.5</sub> Concentration:</span><br/>
            <b style='color:#e67e22;'>%s</b>
          </div>
          ",
          paste(name1, name2, sep = " :: "),
          yr,
          ifelse(
            is.na(dis_pm),
            "No data",
            paste0(sprintf("%.1f", dis_pm), " µg/m³")
          )
        )
      )
    
    pal_district <- leaflet::colorBin(
      palette = pm25_palette,
      domain = data_df_district$dis_pm,
      bins = pm25_bins,
      na.color = "#d9d9d9"
    )
    
    leaflet::leafletProxy("country_wise_pm") %>%
      leaflet::clearGroup("District") %>%
      leaflet::addPolygons(
        data = data_df_district,
        group = "District",
        layerId = ~name2,
        fillColor = ~pal_district(dis_pm),
        weight = 0.4,
        opacity = 1,
        color = "#ffffff",
        fillOpacity = 0.65,
        
        options = leaflet::pathOptions(
          pane = "districtPane",
          smoothFactor = 1.5
        ),
        
        label = lapply(data_df_district$label_text_district, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            "padding" = "4px 8px",
            "border-radius" = "4px"
          ),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 1.5,
          color = "#333333",
          fillOpacity = 0.85,
          bringToFront = FALSE
        )
      )
  }
  
  # ---------------------------------------------------------
  # Satellite Districts
  # ---------------------------------------------------------
  
  else {
    
    req(input$year_gis_aqli)
    
    data_df_district <- satellite_district_map_data()
    
    year_col <- get_year_col(data_df_district, input$year_gis_aqli)
    
    data_df_district <- data_df_district %>%
      trim_leaflet_sf(c("name1", "name2", "population", year_col)) %>%
      filter_selected_states() %>%
      dplyr::mutate(
        pm_value = .data[[year_col]],
        label_text_district = sprintf(
          "
          <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
            <b style='font-size:14px;color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>Population: </span>
            <b style='color:#2c3e50;'>%s</b><br/>
            <span style='color:#7f8c8d;'>Satellite PM<sub>2.5</sub> Concentration:</span><br/>
            <b style='color:#e67e22;'>%s</b>
          </div>
          ",
          paste0(name1, " :: ", name2),
          formatC(population, format = "f", big.mark = ",", digits = 0),
          ifelse(
            is.na(pm_value),
            "No data",
            paste0(sprintf("%.1f", pm_value), " µg/m³")
          )
        )
      )
    
    pal_district <- leaflet::colorBin(
      palette = sat_palette,
      domain = data_df_district$pm_value,
      bins = sat_bins,
      na.color = "grey"
    )
    
    leaflet::leafletProxy("country_wise_pm") %>%
      leaflet::clearGroup("District") %>%
      leaflet::addPolygons(
        data = data_df_district,
        group = "District",
        layerId = ~name2,
        fillColor = ~pal_district(pm_value),
        weight = 0.4,
        opacity = 1,
        color = "white",
        fillOpacity = 0.65,
        
        options = leaflet::pathOptions(
          pane = "districtPane",
          smoothFactor = 1.5
        ),
        
        label = lapply(data_df_district$label_text_district, htmltools::HTML),
        labelOptions = leaflet::labelOptions(
          style = list(
            "font-weight" = "normal",
            "padding" = "3px 8px"
          ),
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 1.5,
          color = "#666",
          fillOpacity = 0.85,
          bringToFront = FALSE
        )
      )
  }
})


# =========================================================
# LOAD SENSORS ON DEMAND
# Sensors always stay above state/district because of sensorPane
# =========================================================

observeEvent(input$load_sensor_layer, {
  
  req(input$country_gis)
  
  sensor_data_ac <- sensor_data %>%
    dplyr::filter(
      name0 == input$country_gis,
      !is.na(lng),
      !is.na(lat)
    )
  
  if (input$switch_btn == "pm25" && !is.null(input$year_gis)) {
    sensor_data_ac <- sensor_data_ac %>%
      dplyr::filter(year %in% input$year_gis)
  }

  sensor_data_ac <- sensor_data_ac %>%
    dplyr::mutate(
      sensor_key = as.character(sensors_id),
      sensor_key = dplyr::if_else(
        is.na(sensor_key) | trimws(sensor_key) == "",
        paste(owner, round(lat, 5), round(lng, 5), sep = "_"),
        sensor_key
      ),
      month_key = paste(year, month, sep = "-")
    ) %>%
    dplyr::group_by(sensor_key, lng, lat) %>%
    dplyr::summarise(
      owner = first_non_missing(owner),
      location_name = first_non_missing(location_name),
      pm25 = round(mean(pm25, na.rm = TRUE), 1),
      coverage = round(mean(coverage.percentComplete, na.rm = TRUE), 1),
      reporting_months = dplyr::n_distinct(month_key),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      location_name = dplyr::if_else(
        is.na(location_name) | trimws(location_name) == "",
        "Location unavailable",
        location_name
      ),
      location_html = htmltools::htmlEscape(location_name),
      owner_html = htmltools::htmlEscape(owner)
    )

  if (nrow(sensor_data_ac) == 0) {
    leaflet::leafletProxy("country_wise_pm") %>%
      leaflet::clearGroup("Sensors")

    shiny::showNotification(
      "No sensor locations are available for the selected country and year.",
      type = "message"
    )

    return(invisible(NULL))
  }
  
  leaflet::leafletProxy("country_wise_pm") %>%
    leaflet::clearGroup("Sensors") %>%
    leaflet::addCircleMarkers(
      data = sensor_data_ac,
      group = "Sensors",
      lng = ~lng,
      lat = ~lat,
      layerId = ~sensor_key,
      radius = 4,
      color = "#dc2626",
      fillColor = "#dc2626",
      fillOpacity = 0.95,
      stroke = TRUE,
      weight = 1,
      
      options = leaflet::pathOptions(
        pane = "sensorPane"
      ),

      clusterOptions = leaflet::markerClusterOptions(
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = FALSE,
        removeOutsideVisibleBounds = TRUE,
        disableClusteringAtZoom = 9
      ),

      label = lapply(sensor_data_ac$location_html, htmltools::HTML),
      labelOptions = leaflet::labelOptions(
        direction = "top",
        offset = c(0, -4),
        style = list(
          "font-family" = "Montserrat, Arial, sans-serif",
          "font-size" = "12px",
          "font-weight" = "700",
          "padding" = "5px 8px"
        )
      ),
      
      popup = ~paste0(
        "<div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>",
        "<b>Monitor Name:</b> ", location_html, "<br/>",
        "<b>Awardee Group:</b> ", owner_html, "<br/>",
        "<b>PM<sub>2.5</sub> Concentration:</b> ", round(pm25, 1), " µg/m³<br/>",
        "<b>Data Availability:</b> ", coverage, "%<br/>",
        "<b>Reporting months:</b> ", reporting_months, "<br/>",
        "<b>Coordinate:</b> ", round(lat, 6), " ", round(lng, 6),
        "</div>"
      )
    )
})


# =========================================================
# CLEAR EXTRA LAYERS
# =========================================================

observeEvent(input$clear_map_layers, {
  
  leaflet::leafletProxy("country_wise_pm") %>%
    leaflet::clearGroup("District") %>%
    leaflet::clearGroup("Sensors")
})


  # output$country_wise_pm <- renderLeaflet({
  #   
  #   if (input$switch_btn == "pm25") {
  #     
  #      req(input$country_gis, input$year_gis)
  #     
  #     
  #     pm_bins <- c(0, 15, 30, 45, 60, 90, Inf)
  #     
  #     pm_palette <- c(
  #       "#e0f7fa",
  #       "#9adbe8",
  #       "#5cc4d6",
  #       "#2f9bb8",
  #       "#1f6f9a",
  #       "#0f3d73"
  #     )
  #     
  #     #############################
  #     
  #     
  # 
  #     
  #     
  #     
  #     ##################################
  #     
  #     
  #     data_df_state <- gadm1_shp_pm %>%
  #       dplyr::filter(name0 == input$country_gis) %>%
  #       dplyr::left_join(
  #         gis_data_state(),
  #         by = c("name0", "name1")
  #       ) %>%
  #       dplyr::mutate(
  #         label_text_state = sprintf(
  #           "
  #       <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
  #         <b style='font-size:14px; color:#2c3e50;'>%s</b><br/>
  #         <span style='color:#7f8c8d;'>Year: </span>
  #         <b style='color:#2c3e50;'>%s</b><br/>
  #         <span style='color:#7f8c8d;'>PM<sub>2.5</sub> Concentration:</span><br/>
  #         <b style='color:#e67e22;'>%s µg/m³</b>
  #       </div>
  #       ",
  #           name1,
  #           input$year_gis,
  #           ifelse(is.na(dis_pm), "No data", sprintf("%.1f", dis_pm))
  #         )
  #       )
  #     
  #     data_df_district <- gadm2_shp %>%
  #       dplyr::filter(name0 == input$country_gis) %>%
  #       dplyr::left_join(
  #         gis_data_district(),
  #         by = c("name0", "name1", "name2")
  #       ) %>%
  #       dplyr::mutate(
  #         label_text_district = sprintf(
  #           "
  #       <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
  #         <b style='font-size:14px; color:#2c3e50;'>%s</b><br/>
  #         <span style='color:#7f8c8d;'>Year: </span>
  #         <b style='color:#2c3e50;'>%s</b><br/>
  #         <span style='color:#7f8c8d;'>PM<sub>2.5</sub> Concentration:</span><br/>
  #         <b style='color:#e67e22;'>%s µg/m³</b>
  #       </div>
  #       ",
  #           paste(name1, name2, sep = " :: "),
  #           input$year_gis,
  #           ifelse(is.na(dis_pm), "No data", sprintf("%.1f", dis_pm))
  #         )
  #       )
  #     
  #     sensor_data_ac <- sensor_data %>%
  #       dplyr::filter(name0 == input$country_gis)
  #     
  #     pal_state <- leaflet::colorBin(
  #       palette = pm_palette,
  #       domain = data_df_state$dis_pm,
  #       bins = pm_bins,
  #       na.color = "#d9d9d9"
  #     )
  #     
  #     pal_district <- leaflet::colorBin(
  #       palette = pm_palette,
  #       domain = data_df_district$dis_pm,
  #       bins = pm_bins,
  #       na.color = "#d9d9d9"
  #     )
  #     
  #     leaflet() %>%
  #       addProviderTiles(providers$CartoDB.Positron) %>%
  #       
  #       addPolygons(
  #         data = data_df_state,
  #         group = "State",
  #         layerId = ~name1,
  #         fillColor = ~pal_state(dis_pm),
  #         weight = 0.8,
  #         opacity = 1,
  #         color = "#ffffff",
  #         fillOpacity = 0.8,
  #         label = lapply(data_df_state$label_text_state, HTML),
  #         labelOptions = labelOptions(
  #           style = list(
  #             "font-weight" = "normal",
  #             "padding" = "4px 8px",
  #             "border-radius" = "4px"
  #           ),
  #           textsize = "13px",
  #           direction = "auto"
  #         ),
  #         highlightOptions = highlightOptions(
  #           weight = 2,
  #           color = "#333333",
  #           fillOpacity = 0.9,
  #           bringToFront = TRUE
  #         )
  #       ) %>%
  #       
  #       addPolygons(
  #         data = data_df_district,
  #         group = "District",
  #         layerId = ~name2,
  #         fillColor = ~pal_district(dis_pm),
  #         weight = 0.6,
  #         opacity = 1,
  #         color = "#ffffff",
  #         fillOpacity = 0.75,
  #         label = lapply(data_df_district$label_text_district, HTML),
  #         labelOptions = labelOptions(
  #           style = list(
  #             "font-weight" = "normal",
  #             "padding" = "4px 8px",
  #             "border-radius" = "4px"
  #           ),
  #           textsize = "13px",
  #           direction = "auto"
  #         ),
  #         highlightOptions = highlightOptions(
  #           weight = 2,
  #           color = "#333333",
  #           fillOpacity = 0.9,
  #           bringToFront = TRUE
  #         )
  #       ) %>%
  #       
  #       addCircleMarkers(
  #         data = sensor_data_ac,
  #         group = "Sensors",
  #         lng = ~lng,
  #         lat = ~lat,
  #         radius = 3.5,
  #         color = "#e74c3c",
  #         fillColor = "#e74c3c",
  #         fillOpacity = 0.9,
  #         stroke = TRUE,
  #         weight = 0.7,
  #         popup = ~paste0(
  #           "<b>Sensor:</b> ", owner, "<br/>",
  #           "<b>PM<sub>2.5</sub>:</b> ", round(pm25, 1), " µg/m³<br/>",
  #           "<b>Latitude:</b> ", round(lat, 4), "<br/>",
  #           "<b>Longitude:</b> ", round(lng, 4)
  #         )
  #       ) %>%
  #       
  #       addLayersControl(overlayGroups =
  #                          c("State", "District", "Sensors"),
  #                        options = layersControlOptions(collapsed = FALSE)
  #       ) %>%
  #       hideGroup(c("District", "Sensors")) %>% 
  #       
  #       addControl(
  #         html = HTML("
  # <div style='
  #   padding:10px 12px;
  #   background:rgba(255,255,255,0.92);
  #   font-family:Arial, sans-serif;
  #   font-size:12px;
  #   border-radius:6px;
  #   box-shadow:0 1px 5px rgba(0,0,0,0.25);
  # '>
  #   <b>PM<sub>2.5</sub> (µg/m³)</b>
  # 
  #   <div style='display:flex; flex-wrap:wrap; gap:6px 10px; margin-top:6px;'>
  # 
  #     <div><span style='display:inline-block;width:18px;height:10px;background:#e0f7fa;'></span> 0–15</div>
  #     <div><span style='display:inline-block;width:18px;height:10px;background:#9adbe8;'></span> 15–30</div>
  #     <div><span style='display:inline-block;width:18px;height:10px;background:#5cc4d6;'></span> 30–45</div>
  #     <div><span style='display:inline-block;width:18px;height:10px;background:#2f9bb8;'></span> 45–60</div>
  #     <div><span style='display:inline-block;width:18px;height:10px;background:#1f6f9a;'></span> 60–90</div>
  #     <div><span style='display:inline-block;width:18px;height:10px;background:#0f3d73;'></span> 90+</div>
  #     <div><span style='display:inline-block;width:18px;height:10px;background:#d9d9d9;'></span> No data</div>
  # 
  #   </div>
  # </div>
  # "),
  #         position = "bottomleft"
  #       ) %>% 
  #       addFullscreenControl(
  #         position = "topleft",
  #         pseudoFullscreen = FALSE
  #       )
  #     
  #     
  #   }
  #   
  #   
  #   else {
  #     
  #     data_df_state <- gadm1_shp_pm %>%
  #       filter(name0 == input$country_gis) %>%
  #       select(name0, name1, !!input$year_gis_aqli, population) 
  #     
  #     print("name1")
  #     print(data_df_state)
  #     
  #   
  #     
  #     data_df_district <- gadm2_shp_pm %>%
  #       filter(name0 == input$country_gis) %>%
  #       select(name1,name2, !!input$year_gis_aqli, population) 
  #     
  #     print("name2")
  #     print(data_df_district)
  #     
  #     data_df_state$label_text <- sprintf(
  #       "<div style='font-family:sans-serif;font-size:13px;line-height:1.5;'>
  #        <b style='font-size:14px;color:#2c3e50;'>%s</b><br/>
  #        <span style='color:#7f8c8d;'>Population: </span>
  #        <b style='color:#2c3e50;'>%s</b><br/>
  #        
  #        <span style='color:#7f8c8d;'><b>PM<sub>2.5</sub> Concentration</b><br/></span>
  #        <b style='color:#e67e22;'>%.1f µg/m³</b>
  #      </div>",
  #       data_df_state$name1,
  #       formatC(data_df_state$population, format = "f", big.mark = ",", digits = 0),
  #       data_df_state[[input$year_gis_aqli]]
  #     )
  #     
  #     pal_state <- colorBin(
  #       palette = c("#b7ebf1", "#8fd8e4", "#3db1c8", "#3f8dac", "#416891", "#434475", "#451f59"),
  #       domain = data_df_state[[input$year_gis_aqli]],
  #       bins = c(0, 10, 25, 35, 50, 60, Inf),
  #       na.color = "grey"
  #     )
  #     
  # 
  #     
  #     data_df_district$label_text <- sprintf(
  #       "<div style='font-family:sans-serif;font-size:13px;line-height:1.5;'>
  #    <b style='font-size:14px;color:#2c3e50;'>%s</b><br/>
  #    <span style='color:#7f8c8d;'>Population: </span>
  #    <b style='color:#2c3e50;'>%s</b><br/>
  #    <span style='color:#7f8c8d;'>
  #      <b>PM<sub>2.5</sub> Concentration</b>
  #    </span><br/>
  #    <b style='color:#e67e22;'>%.1f µg/m³</b>
  #  </div>",
  #       paste0(data_df_district$name1, " :: ", data_df_district$name2),
  #       formatC(data_df_district$population,
  #               format = "f",
  #               big.mark = ",",
  #               digits = 0),
  #       data_df_district[[input$year_gis_aqli]]
  #     )
  #     
  #     pal_dis <- colorBin(
  #       palette = c("#b7ebf1", "#8fd8e4", "#3db1c8", "#3f8dac", "#416891", "#434475", "#451f59"),
  #       domain = data_df_state[[input$year_gis_aqli]],
  #       bins = c(0, 10, 25, 35, 50, 60, Inf),
  #       na.color = "grey"
  #     )
  #     
  #     leaflet(data_df_state) %>%
  #       addProviderTiles(providers$CartoDB.Positron) %>%
  #       
  #       addPolygons(data = data_df_state,
  #         layerId = ~name1,group = "State",
  #         fillColor = ~pal_state(data_df_state[[input$year_gis_aqli]]),
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
  #         label = lapply(data_df_state$label_text, HTML),
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal", padding = "3px 8px"),
  #           textsize = "13px",
  #           direction = "auto"
  #         )
  #       ) %>%
  #       addPolygons(data = data_df_district,
  #         
  #         layerId = ~name2,
  #         group = "District",
  #         fillColor = ~pal_dis(data_df_district[[input$year_gis_aqli]]),
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
  #         label = lapply(data_df_district$label_text, HTML),
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal", padding = "3px 8px"),
  #           textsize = "13px",
  #           direction = "auto"
  #         )
  #       ) %>%
  #       addLayersControl(overlayGroups = 
  #                          c("State", "District"),
  #                        options = layersControlOptions(collapsed = FALSE)
  #       ) %>%
  #       hideGroup(c("District")) %>% 
  #       
  #       addControl(
  #         html = HTML("
  #   <div style='padding: 8px; background: rgba(255,255,255,0.8); font-size: 12px; border-radius: 4px;'>
  #     <b>PM<sub>2.5</sub> Concentration (µg/m³)</b><br/>
  #     <div style='display: flex; flex-wrap: wrap; gap: 6px 10px; align-items: center; margin-top: 6px;'>
  #       <div style='display: flex; align-items: center; gap: 4px;'>
  #         <div style='background:#b7ebf1; width:20px; height:12px;'></div><span>0–10</span>
  #       </div>
  #       <div style='display: flex; align-items: center; gap: 4px;'>
  #         <div style='background:#8fd8e4; width:20px; height:12px;'></div><span>10–25</span>
  #       </div>
  #       <div style='display: flex; align-items: center; gap: 4px;'>
  #         <div style='background:#3db1c8; width:20px; height:12px;'></div><span>25–35</span>
  #       </div>
  #       <div style='display: flex; align-items: center; gap: 4px;'>
  #         <div style='background:#3f8dac; width:20px; height:12px;'></div><span>35–50</span>
  #       </div>
  #       <div style='display: flex; align-items: center; gap: 4px;'>
  #         <div style='background:#416891; width:20px; height:12px;'></div><span>50–60</span>
  #       </div>
  #       <div style='display: flex; align-items: center; gap: 4px;'>
  #         <div style='background:#434475; width:20px; height:12px;'></div><span>60+</span>
  #       </div>
  #     </div>
  #   </div>
  # "),
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
  #   }
  #   
  #   ########## click events
  #   
  #   
  #   
  # })
  
#})



#############################################################################################
 
# output$country_wise_pm <- renderLeaflet({
# 
#   req(input$country_gis, input$year_gis)
# 
# 
#   pm_palette <- c(
#     "#b7ebf1", "#8fd8e4", "#3db1c8",
#     "#3f8dac", "#416891", "#434475"
#   )
# 
#   pm_bins <- c(0, 10, 25, 35, 50, 60, Inf)
# 
#   data_df_state <- gadm1_shp %>%
#     dplyr::filter(name0 == input$country_gis) %>%
#     dplyr::left_join(
#       gis_data_state(),
#       by = c("name0", "name1")
#     ) %>%
#     dplyr::mutate(
#       label_text_state = sprintf(
#         "
#         <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
#           <b style='font-size:14px; color:#2c3e50;'>%s</b><br/>
#           <span style='color:#7f8c8d;'>Year: </span>
#           <b style='color:#2c3e50;'>%s</b><br/>
#           <span style='color:#7f8c8d;'>PM<sub>2.5</sub> Concentration:</span><br/>
#           <b style='color:#e67e22;'>%s µg/m³</b>
#         </div>
#         ",
#         name1,
#         input$year_gis,
#         ifelse(is.na(dis_pm), "No data", sprintf("%.1f", dis_pm))
#       )
#     )
# 
#   data_df_district <- gadm2_shp %>%
#     dplyr::filter(name0 == input$country_gis) %>%
#     dplyr::left_join(
#       gis_data_district(),
#       by = c("name0", "name1", "name2")
#     ) %>%
#     dplyr::mutate(
#       label_text_district = sprintf(
#         "
#         <div style='font-family:Arial, sans-serif; font-size:13px; line-height:1.5;'>
#           <b style='font-size:14px; color:#2c3e50;'>%s</b><br/>
#           <span style='color:#7f8c8d;'>Year: </span>
#           <b style='color:#2c3e50;'>%s</b><br/>
#           <span style='color:#7f8c8d;'>PM<sub>2.5</sub> Concentration:</span><br/>
#           <b style='color:#e67e22;'>%s µg/m³</b>
#         </div>
#         ",
#         paste(name1, name2, sep = " :: "),
#         input$year_gis,
#         ifelse(is.na(dis_pm), "No data", sprintf("%.1f", dis_pm))
#       )
#     )
# 
#   sensor_data_ac <- sensor_data %>%
#     dplyr::filter(name0 == input$country_gis)
# 
#   pal_state <- leaflet::colorBin(
#     palette = pm_palette,
#     domain = data_df_state$dis_pm,
#     bins = pm_bins,
#     na.color = "#d9d9d9"
#   )
# 
#   pal_district <- leaflet::colorBin(
#     palette = pm_palette,
#     domain = data_df_district$dis_pm,
#     bins = pm_bins,
#     na.color = "#d9d9d9"
#   )
# 
#   leaflet() %>%
#     addProviderTiles(providers$CartoDB.Positron) %>%
# 
#     addPolygons(
#       data = data_df_state,
#       group = "State",
#       layerId = ~name1,
#       fillColor = ~pal_state(dis_pm),
#       weight = 0.8,
#       opacity = 1,
#       color = "#ffffff",
#       fillOpacity = 0.8,
#       label = lapply(data_df_state$label_text_state, HTML),
#       labelOptions = labelOptions(
#         style = list(
#           "font-weight" = "normal",
#           "padding" = "4px 8px",
#           "border-radius" = "4px"
#         ),
#         textsize = "13px",
#         direction = "auto"
#       ),
#       highlightOptions = highlightOptions(
#         weight = 2,
#         color = "#333333",
#         fillOpacity = 0.9,
#         bringToFront = TRUE
#       )
#     ) %>%
# 
#     addPolygons(
#       data = data_df_district,
#       group = "District",
#       layerId = ~name2,
#       fillColor = ~pal_district(dis_pm),
#       weight = 0.6,
#       opacity = 1,
#       color = "#ffffff",
#       fillOpacity = 0.75,
#       label = lapply(data_df_district$label_text_district, HTML),
#       labelOptions = labelOptions(
#         style = list(
#           "font-weight" = "normal",
#           "padding" = "4px 8px",
#           "border-radius" = "4px"
#         ),
#         textsize = "13px",
#         direction = "auto"
#       ),
#       highlightOptions = highlightOptions(
#         weight = 2,
#         color = "#333333",
#         fillOpacity = 0.9,
#         bringToFront = TRUE
#       )
#     ) %>%
# 
#     addCircleMarkers(
#       data = sensor_data_ac,
#       group = "Sensors",
#       lng = ~lng,
#       lat = ~lat,
#       radius = 3.5,
#       color = "#e74c3c",
#       fillColor = "#e74c3c",
#       fillOpacity = 0.9,
#       stroke = TRUE,
#       weight = 0.7,
#       popup = ~paste0(
#         "<b>Sensor:</b> ", owner, "<br/>",
#         "<b>PM<sub>2.5</sub>:</b> ", round(pm25, 1), " µg/m³<br/>",
#         "<b>Latitude:</b> ", round(lat, 4), "<br/>",
#         "<b>Longitude:</b> ", round(lng, 4)
#       )
#     ) %>%
# 
#     addLayersControl(overlayGroups =
#         c("State", "District", "Sensors"),
#       options = layersControlOptions(collapsed = FALSE)
#     ) %>%
# 
#     addControl(
#       html = HTML("
#         <div style='
#           padding:10px 12px;
#           background:rgba(255,255,255,0.92);
#           font-family:Arial, sans-serif;
#           font-size:12px;
#           border-radius:6px;
#           box-shadow:0 1px 5px rgba(0,0,0,0.25);
#         '>
#           <b>PM<sub>2.5</sub> Concentration (µg/m³)</b>
#           <div style='display:flex; flex-wrap:wrap; gap:8px 12px; align-items:center; margin-top:8px;'>
# 
#             <div style='display:flex; align-items:center; gap:4px;'>
#               <div style='background:#b7ebf1; width:22px; height:12px;'></div><span>0–10</span>
#             </div>
# 
#             <div style='display:flex; align-items:center; gap:4px;'>
#               <div style='background:#8fd8e4; width:22px; height:12px;'></div><span>10–25</span>
#             </div>
# 
#             <div style='display:flex; align-items:center; gap:4px;'>
#               <div style='background:#3db1c8; width:22px; height:12px;'></div><span>25–35</span>
#             </div>
# 
#             <div style='display:flex; align-items:center; gap:4px;'>
#               <div style='background:#3f8dac; width:22px; height:12px;'></div><span>35–50</span>
#             </div>
# 
#             <div style='display:flex; align-items:center; gap:4px;'>
#               <div style='background:#416891; width:22px; height:12px;'></div><span>50–60</span>
#             </div>
# 
#             <div style='display:flex; align-items:center; gap:4px;'>
#               <div style='background:#434475; width:22px; height:12px;'></div><span>60+</span>
#             </div>
# 
#             <div style='display:flex; align-items:center; gap:4px;'>
#               <div style='background:#d9d9d9; width:22px; height:12px;'></div><span>No data</span>
#             </div>
# 
#           </div>
#         </div>
#       "),
#       position = "bottomleft"
#     ) %>%
# 
#     addFullscreenControl(
#       position = "topleft",
#       pseudoFullscreen = FALSE
#     )
# })
# output$gis_table <- reactable::renderReactable({
#   
#   clicked <- input$country_wise_pm_shape_click$id
#   req(clicked)
#   
#   df <- sensor_data %>%
#     dplyr::group_by(name0, name1, name2, year, owner) %>%
#     dplyr::summarise(
#       tot_sensor = dplyr::n_distinct(locations_id),
#       pm25_avg = round(mean(pm25, na.rm = TRUE), 2),
#       .groups = "drop"
#     ) %>%
#     dplyr::filter(name1 %in% clicked) %>%
#     dplyr::arrange(dplyr::desc(year), dplyr::desc(pm25_avg)) %>%
#     dplyr::rename(
#       Country = name0,
#       State = name1,
#       District = name2,
#       Year = year,
#       Owner = owner,
#       Sensors = tot_sensor,
#       `PM2.5` = pm25_avg
#     )
#   
#   shiny::validate(
#     shiny::need(nrow(df) > 0, "There is no data to show!")
#   )
#   
#   reactable::reactable(
#     df,
#     searchable = TRUE,
#     highlight = TRUE,
#     striped = TRUE,
#     compact = TRUE,
#     defaultPageSize = 10,
#     showPageSizeOptions = TRUE,
#     pageSizeOptions = c(10, 25, 50),
#     
#     columns = list(
#       Country = reactable::colDef(minWidth = 120),
#       State = reactable::colDef(minWidth = 140),
#       District = reactable::colDef(minWidth = 150),
#       Year = reactable::colDef(width = 80, align = "center"),
#       Owner = reactable::colDef(minWidth = 160),
#       Sensors = reactable::colDef(width = 95, align = "center"),
#       `PM2.5` = reactable::colDef(
#         width = 100,
#         align = "center",
#         format = reactable::colFormat(digits = 2),
#         style = list(
#           fontWeight = "700",
#           color = "#1f2937"
#         )
#       )
#     ),
#     
#     theme = reactable::reactableTheme(
#       color = "#374151",
#       backgroundColor = "#ffffff",
#       borderColor = "#e5e7eb",
#       stripedColor = "#f9fafb",
#       highlightColor = "#eef6f8",
#       cellPadding = "10px 12px",
#       
#       style = list(
#         fontFamily = "Inter, Arial, sans-serif",
#         fontSize = "13px"
#       ),
#       
#       tableStyle = list(
#         border = "1px solid #e5e7eb",
#         borderRadius = "12px",
#         overflow = "hidden"
#       ),
#       
#       headerStyle = list(
#         background = "#f8fafc",
#         color = "#111827",
#         fontWeight = "700",
#         borderBottom = "1px solid #e5e7eb"
#       )
#     ),
#     
#     language = reactable::reactableLang(
#       searchPlaceholder = "Search..."
#     )
#   )
# })
# 
# 
# # 
# # 

output$gis_table <- reactable::renderReactable({
  
  shiny::validate(
    shiny::need(nrow(country_df) > 0, "There is no data to show!")
  )
  
  reactable::reactable(
    country_df,
    searchable = TRUE,
    highlight = TRUE,
    striped = TRUE,
    compact = TRUE,
    bordered = FALSE,
    defaultPageSize = 14,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50),
    defaultSorted = list(PM25 = "desc"),
    onClick = "expand",
    
    columns = list(
      
      Country = reactable::colDef(
        name = "Country",
        width = 200,
        style = list(
          fontWeight = "800",
          color = "#111827"
        )
      ),
      
      Sensors = reactable::colDef(
        width = 75,
        align = "center"
      ),
      
      Awardees = reactable::colDef(
        width = 85,
        align = "center"
      ),
      
      Months = reactable::colDef(
        width = 75,
        align = "center"
      ),
      
      Coverage = reactable::colDef(
        name = "Data Availability",
        width = 180,
        cell = function(value) {
          bar_cell(
            value = value,
            max_value = 100,
            color = coverage_color(value),
            suffix = "%"
          )
        }
      ),
      
      PM25 = reactable::colDef(
        name = pm_header,
        width = 180,
        cell = function(value) {
          bar_cell(
            value = value,
            max_value = max_country_pm,
            color = aqli_pm_color(value)
          )
        }
      )
    ),
    
    details = function(index) {
      
      selected_country <- country_df$Country[index]
      
      state_tbl <- state_df[
        Country == selected_country,
        .(State, Sensors, Awardees, Months, Coverage, PM25)
      ]
      
      htmltools::div(
        style = "
          padding:10px 14px 14px 42px;
          background:#fcfcfd;
          border-top:1px solid #eef2f7;
        ",
        
        htmltools::div(
          style = "
            font-weight:800;
            color:#111827;
            margin-bottom:8px;
            font-size:13px;
          ",
          paste("States / provinces in", selected_country)
        ),
        
        reactable::reactable(
          state_tbl,
          compact = TRUE,
          highlight = TRUE,
          striped = TRUE,
          bordered = FALSE,
          searchable = FALSE,
          defaultPageSize = 5,
          showPageSizeOptions = FALSE,
          onClick = "expand",
          
          columns = list(
            
            State = reactable::colDef(
              width = 200,
              style = list(
                fontWeight = "700",
                color = "#1f2937"
              )
            ),
            
            Sensors = reactable::colDef(
              width = 75,
              align = "center"
            ),
            
            Awardees = reactable::colDef(
              width = 85,
              align = "center"
            ),
            
            Months = reactable::colDef(
              width = 75,
              align = "center"
            ),
            
            Coverage = reactable::colDef(
              name = "Data Availability",
              width = 180,
              align = "center",
              cell = function(value) {
                bar_cell(
                  value = value,
                  max_value = 100,
                  color = coverage_color(value),
                  suffix = "%"
                )
              }
            ),
            
            PM25 = reactable::colDef(
              name = pm_header,
              width = 180,
              align = "center",
              cell = function(value) {
                bar_cell(
                  value = value,
                  max_value = max_state_pm,
                  color = aqli_pm_color(value)
                )
              }
            )
          ),
          
          details = function(state_index) {
            
            selected_state <- state_tbl$State[state_index]
            
            district_tbl <- district_df[
              Country == selected_country & State == selected_state,
              .(District, Sensors, Awardees, Months, Coverage, PM25)
            ]
            
            htmltools::div(
              style = "
                padding:10px 12px 14px 38px;
                background:#ffffff;
                border-top:1px solid #eef2f7;
              ",
              
              htmltools::div(
                style = "
                  font-weight:800;
                  color:#111827;
                  margin-bottom:8px;
                  font-size:13px;
                ",
                paste("Districts in", selected_state)
              ),
              
              reactable::reactable(
                district_tbl,
                compact = TRUE,
                highlight = TRUE,
                striped = TRUE,
                bordered = FALSE,
                searchable = FALSE,
                defaultPageSize = 5,
                showPageSizeOptions = FALSE,
                
                columns = list(
                  
                  District = reactable::colDef(
                    width = 200,
                    style = list(
                      fontWeight = "700",
                      color = "#374151"
                    )
                  ),
                  
                  Sensors = reactable::colDef(
                    width = 75,
                    align = "center"
                  ),
                  
                  Awardees = reactable::colDef(
                    width = 85,
                    align = "center"
                  ),
                  
                  Months = reactable::colDef(
                    width = 75,
                    align = "center"
                  ),
                  
                  Coverage = reactable::colDef(
                    name = "Data Availability",
                    width = 180,
                    align = "center",
                    cell = function(value) {
                      bar_cell(
                        value = value,
                        max_value = 100,
                        color = coverage_color(value),
                        suffix = "%"
                      )
                    }
                  ),
                  
                  PM25 = reactable::colDef(
                    name = pm_header,
                    width = 180,
                    align = "center",
                    cell = function(value) {
                      bar_cell(
                        value = value,
                        max_value = max_district_pm,
                        color = aqli_pm_color(value)
                      )
                    }
                  )
                ),
                
                theme = simple_theme
              )
            )
          },
          
          theme = simple_theme
        )
      )
    },
    
    theme = simple_theme,
    
    language = reactable::reactableLang(
      searchPlaceholder = "Search country..."
    )
  )
})
# Handle polygon click
# observeEvent(input$country_wise_pm_shape_click, {
#   print("Map shape clicked!")
#   click_id <- input$country_wise_pm_shape_click$id
#   if (is.null(click_id)) {
#     print("No click ID detected")
#     return()
#   }
# 
#   state_id <- click_id
#  # if (input$switch_btn == "pm25") {
# 
# 
#     # clicked_data <- gadm2_pm25 %>% filter(name0 == input$country_gis) %>%
#     #   select(name0, name1,name2,population, !!input$year_gis) %>% filter() %>% filter(name1 == state_id) %>%
#     #   rename(Value = !!sym(input$year_gis)) %>% arrange(Value)
# 
# 
#     clicked_data <- gadm2_pm25[
#       name0 == input$country_gis & name1 == state_id,
#       .(name0, name1, name2, population, Value = get(input$year_gis))
#     ][order(Value)]
# 
# 
# 
# 
# 
# 
# 
# 
#   if (nrow(clicked_data) == 0) {
#     print("No data found for clicked state")
#     showModal(modalDialog(
#       title = paste("State", state_id, "Details"),
#       "No data available for this state.",
#       easyClose = TRUE,
#       size = "l"
#     ))
#     return()
#   }
# 
# 
#   showModal(
#     modalDialog(
#       title = div(
#         style = "font-size: 20px; font-weight: bold; color: #2c3e50;",
#         paste0("Country : ",input$country_gis," → " ,"State : ", state_id)
#       ),
#       uiOutput("modal_content"),
#       size = "l",
#       easyClose = TRUE,
#       fade = TRUE,
#       footer = modalButton("Close"),
#       class = "modal-balanced"
#     )
#   )
# 
# 
# 
#   output$modal_content <- renderUI({
# 
# 
#     tabsetPanel(type = "tabs",
#                 tabPanel("Top 10 Most Polluted Region", highchartOutput("chart")%>% withSpinner(color="#0dc5c1")),
#                 # tabPanel("Formation", highchartOutput("cboformation1")%>% withSpinner(color="#0dc5c1")),
#                 tabPanel("Tabular Data", dataTableOutput("table")%>% withSpinner(color="#0dc5c1"))
#     )
# 
#   })
# 
#   # color_map <- c(
#   #   "0 - < 0.1 years" = "#FFFFFF",
#   #   "0.1 - 0.5" = "#FFE6B3",
#   #   "> 0.5 - 1" = "#FFD25D",
#   #   "> 1 - 2" = "#FFBA00",
#   #   "> 2 - 3" = "#FF9600",
#   #   "> 3 - 4" = "#FF6908",
#   #   "> 4 - 5" = "#E63D23",
#   #   "> 5 - < 6" = "#BD251C",
#   #   ">= 6" = "#8C130E"
#   # )
# 
#   color_map = c("0 to < 0.1" = "#fff8f0",
#                 "0.1 to < 0.5" = "#FFF2E1",
#                 "0.5 to < 1" = "#FFEDD3",
#                 "1 to < 2"   = "#FFC97A",
#                 "2 to < 3"   = "#FFA521",
#                 "3 to < 4"   = "#FF9600",
#                 "4 to < 5"   = "#EB6C2A",
#                 "5 to < 6"   = "#D63333",
#                 "6 to < 7"   = "#8E2946",
#                 ">= 7"       = "#451F59")
# 
#   # Render Highcharts chart
#   output$chart <- renderHighchart({
# 
#     #  custom_colors <- c("#fff2e1", "#ffedd3","#FFD25D", "#ffa521", "#eb6c2a", "#d63333", "#8e2946", "#451f59", "#2a1333")
#     # custom_colors <- c("#FFFFFF", "#FFF2E1","#FFEDD3", "#FFC97A", "#FFA521", "#EB6C2A", "#D63333", "#8E2946", "#451F59")
#     custom_colors <- c("#fff8f0", "#FFF2E1","#FFEDD3", "#FFC97A", "#FFA521", "#FF9600", "#EB6C2A", "#D63333", "#8E2946", "#451F59")
# 
#     data_df <- clicked_data %>% head(10) %>% mutate(color = custom_colors[1:n()])
# 
# 
#     tooltip_format <- "<b>{point.category}</b><br>PM2.5: {point.y} μg/m³"
# 
# 
#     axis_y <- list(text = "PM2.5 Concentration (μg/m³)")
# 
# 
# 
#     # Highchart Column Chart
#     highchart() %>%
#       hc_chart(type = "column") %>%
#       hc_title(text = "Top 10 Most Polluted District(s)") %>%
#       hc_xAxis(categories = data_df$name2, title = list(text = "District(s)")) %>%
#       hc_yAxis(title = axis_y) %>%
#       hc_add_series(
#         data = data_df$Value,
#         name = "PM2.5",
#         colorByPoint = TRUE,
#         colors = data_df$color
#       ) %>%
#       hc_tooltip(pointFormat = tooltip_format) %>%
# 
#       hc_tooltip(
#         pointFormat = tooltip_format
#       ) %>%
#       hc_plotOptions(
#         column = list(
#           borderRadius = 3,
#           dataLabels = list(enabled = TRUE, format = "{point.y}")
#         )
#       ) %>%
#       hc_exporting(enabled = TRUE) %>%
#       hc_legend(enabled = FALSE)
#     #  hc_add_theme(hc_theme_flat())
# 
#   })
# 
#   # Render data table
#   output$table <- DT::renderDataTable({
# 
#     req(clicked_data)  # ensures clicked_data is not NULL
# 
#     if (nrow(clicked_data) == 0) {
#       return(NULL)
#     }
# 
#     shiny::validate(
#       need(nrow(clicked_data) > 0, "There is no data to show!")
#     )
# 
# 
#     clicked_data <- clicked_data %>% rename("Country" = name0,
#                                             "State" = name1,
#                                             "Subnationa Units" = name2,
#                                             "Population" = population
#     )
# 
#     DT::datatable(
#       clicked_data,
#       rownames = FALSE,
#       options = opts1,  # ensure opts1 includes scrollY
#       selection = 'single',
#       extensions = 'Buttons',
#       class = 'cell-border stripe compact nowrap'
#     )
# 
#   })
# 
# })
# 
# 
# 
# # output$line_pm_llppwho <- renderHighchart({
# # 
# # 
# # 
# #       text = paste0(
# #         "Annual <span style='color:maroon;'>", unique(data_df$country),
# #         "</span> PM<sub>2.5</sub> Concentration"
# #       )
# # 
# # 
# # 
# #     # --- Highchart Code
# #     highchart() %>%
# #       hc_chart(type = "line") %>%
# # 
# #       # X-Axis: Year
# #       hc_xAxis(categories = data_df$year,
# #                title = list(text = "Year")) %>%
# # 
# #       # First Y-Axis: PM2.5
# #       hc_yAxis(
# #         title = list(text = "PM2.5 (µg/m³)")
# #       ) %>%
# # 
# #       # PM2.5 series
# #       hc_add_series(name = "PM2.5",
# #                     data = data_df$pm,
# #                     yAxis = 0,
# #                     type = "spline",
# #                     color = "#1f77b4") %>%
# # 
# #       # PM2.5 National Standard Line
# #       hc_add_series(name = "PM2.5 National Standard",
# #                     data = data_df$natstandard,
# #                     yAxis = 0,
# #                     type = "line",
# #                     dashStyle = "Dash",
# #                     color = "grey",
# #                     showInLegend = TRUE) %>%
# # 
# #       # Titles
# #       hc_title(
# #         text = text,
# #         useHTML = TRUE
# #       ) %>%
# # 
# #       hc_subtitle(text = paste0("National Avg. PM2.5 Standard ", "(", unique(data_df$natstandard), " µg/m³)")) %>%
# # 
# #       # Tooltip
# #       hc_tooltip(shared = TRUE, crosshairs = TRUE)
# # 
# #   
# # 
# # 
# # 
# # })
# 
# 
# #################
# 
# 
# 
# 
# 
# 
# 
