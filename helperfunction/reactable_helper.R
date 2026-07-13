library(data.table)
library(reactable)
library(htmltools)

# =========================================================
# Helper Functions
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

coverage_color <- function(x) {
  if (is.na(x)) {
    "#d9d9d9"
  } else if (x >= 80) {
    "#7cb342"
  } else if (x >= 50) {
    "#fdd835"
  } else {
    "#e53935"
  }
}

bar_cell <- function(value, max_value, color, suffix = "") {
  
  width <- ifelse(
    is.na(value) || is.na(max_value) || max_value == 0 || is.infinite(max_value),
    0,
    value / max_value * 100
  )
  
  htmltools::div(
    style = "display:flex; align-items:center; gap:7px; width:100%;",
    
    htmltools::div(
      style = "
        flex:1;
        height:7px;
        background:#eef2f7;
        border-radius:999px;
        overflow:hidden;
        min-width:42px;
      ",
      htmltools::div(
        style = sprintf(
          "
          width:%s%%;
          height:100%%;
          background:%s;
          border-radius:999px;
          ",
          width,
          color
        )
      )
    ),
    
    htmltools::div(
      style = "
        min-width:52px;
        text-align:right;
        font-weight:700;
        color:#111827;
        font-size:12px;
      ",
      ifelse(is.na(value), "NA", paste0(value, suffix))
    )
  )
}

pm_header <- "PM₂₅"

simple_theme <- reactable::reactableTheme(
  color = "#374151",
  backgroundColor = "#ffffff",
  borderColor = "#e5e7eb",
  stripedColor = "#f9fafb",
  highlightColor = "#eef6f8",
  cellPadding = "8px 10px",
  
  style = list(
    fontFamily = "Inter, Arial, sans-serif",
    fontSize = "12.5px"
  ),
  
  tableStyle = list(
    border = "1px solid #e5e7eb",
    borderRadius = "0px",
    overflow = "hidden"
  ),
  
  headerStyle = list(
    background = "#f8fafc",
    color = "#111827",
    fontWeight = "800",
    borderBottom = "1px solid #e5e7eb"
  )
)



table_base <- data.table::as.data.table(sensor_data)

table_base <- table_base[year %in% c(2025, 2026)]

table_base[is.na(name1) | name1 == "", name1 := "Unknown state"]

table_base[is.na(name2) | name2 == "",name2 := "Unknown district"]

table_base[,month_year := paste(year, month)]

country_df <- table_base[, .(
  Sensors = uniqueN(sensors_id),
  Awardees = uniqueN(owner),
  Months = uniqueN(month_year),
  Coverage = round(mean(coverage.percentComplete, na.rm = TRUE), 1),
  PM25 = round(mean(pm25, na.rm = TRUE), 2)
),
by = .(Country = name0)
][order(-PM25)]

state_df <- table_base[
  ,
  .(
    Sensors = uniqueN(sensors_id),
    Awardees = uniqueN(owner),
    Months = uniqueN(month_year),
    Coverage = round(mean(coverage.percentComplete, na.rm = TRUE), 1),
    PM25 = round(mean(pm25, na.rm = TRUE), 2)
  ),
  by = .(
    Country = name0,
    State = name1
  )
][
  order(Country, -PM25)
]

district_df <- table_base[
  ,
  .(
    Sensors = uniqueN(sensors_id),
    Awardees = uniqueN(owner),
    Months = uniqueN(month_year),
    Coverage = round(mean(coverage.percentComplete, na.rm = TRUE), 1),
    PM25 = round(mean(pm25, na.rm = TRUE), 2)
  ),
  by = .(
    Country = name0,
    State = name1,
    District = name2
  )
][
  order(Country, State, -PM25)
]

max_country_pm <- max(country_df$PM25, na.rm = TRUE)
max_state_pm <- max(state_df$PM25, na.rm = TRUE)
max_district_pm <- max(district_df$PM25, na.rm = TRUE)

