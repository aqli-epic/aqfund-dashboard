library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(tidyverse)
library(sf)
library(usethis)
library(devtools)
library(data.table)
library(svglite)
library(here)
library(shiny)
library(shinyjs)
library(bslib)
library(shinyWidgets)
library(reactable)
#library(reactablefmtr)
library(highr)
library(highcharter)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(shinycssloaders)
library(DT)
library(writexl)
library(arrow)
library(plotly)
library(qs2)
library(waiter)
library(shinyBS)
library(shinymanager)
library(reactable)
library(leafgl)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearth)
library(htmltools)
library(plotly)

###############################################################################

map_qs_cache <- new.env(parent = emptyenv())
map_qs_cache_keys <- character()

read_map_qs <- function(file, max_entries = 16) {
  cache_key <- normalizePath(file, winslash = "/", mustWork = FALSE)

  if (exists(cache_key, envir = map_qs_cache, inherits = FALSE)) {
    return(get(cache_key, envir = map_qs_cache, inherits = FALSE))
  }

  map_data <- qs2::qs_read(file)
  assign(cache_key, map_data, envir = map_qs_cache)

  map_qs_cache_keys <<- unique(c(map_qs_cache_keys, cache_key))

  if (length(map_qs_cache_keys) > max_entries) {
    remove_keys <- head(map_qs_cache_keys, length(map_qs_cache_keys) - max_entries)
    remove_keys <- remove_keys[vapply(
      remove_keys,
      exists,
      logical(1),
      envir = map_qs_cache,
      inherits = FALSE
    )]

    if (length(remove_keys) > 0) {
      rm(list = remove_keys, envir = map_qs_cache)
    }

    map_qs_cache_keys <<- tail(map_qs_cache_keys, max_entries)
    gc(FALSE)
  }

  map_data
}



sensor_data <- as.data.table(read_csv("./data/AQfund_data.csv"))
sensor_data <- sensor_data %>% filter(name0!="Nicaragua")
sensor_data$name1[sensor_data$name0 == "Nepal" & sensor_data$name1 == "Province 1"] <- "Koshi"

location_sensor <- read_csv("./data/sensor_location.csv")
sensor_data <- left_join(sensor_data, location_sensor %>% select(location_id, location_name), by = c( "locations_id" = "location_id"))
sensor_data <- as.data.table(sensor_data)

source(file.path("helperfunction", "reactable_helper.R"))

openaq_data <- as.data.table(read_csv("./data/AQfund_data_distt.csv"))
openaq_data <- openaq_data %>% filter(name0!="Nicaragua")
openaq_data$name1[openaq_data$name0 == "Nepal" & openaq_data$name1 == "Province 1"] <- "Koshi"

openaq_data <- openaq_data %>% filter(year %in% c(2025,2026))

openaq_data_state <- openaq_data %>% group_by(name0, name1, year) %>% summarise(dis_pm = mean(pm25))

openaq_data_state <- as.data.table(openaq_data_state)

openaq_data_district <- openaq_data %>% group_by(name0,name1, name2, year) %>% summarise(dis_pm = mean(pm25, na.rm=T))
openaq_data_district <- as.data.table(openaq_data_district)

# gadm1_shp <- st_read("./data/shapefile/gadm1_aqfund_shapefile.shp")
# gadm1_shp <- gadm1_shp %>% filter(name0 %in% unique(openaq_data$name0))
# st_write(gadm1_shp, "./data/shapefile/gadm1_aqfund_shapefile.shp", delete_layer = TRUE)

# gadm2_shp <- st_read("./data/shapefile/gadm2_aqfund_shapefile.shp")

# gadm1_shp <- qs2::qs_read("./data/shapefile/gadm1_shp.qs2")
# gadm2_shp <- qs2::qs_read("./data/shapefile/gadm2_shp.qs2")
# gadm2_shp <- gadm2_shp %>% filter(name0 %in% unique(openaq_data$name0))
# gadm2_shp$name1[gadm2_shp$name0 == "Nepal" & gadm2_shp$name1 == "Province 1"] <- "Koshi"
# 
# st_write(gadm2_shp, "./data/shapefile/gadm2_aqfund_shapefile.shp", delete_layer = TRUE)

target_countries <- unique(openaq_data$name0)
#source("~/Desktop/My AQLI Work/AQLI 2026/Updated plots code/helper_function_2026.R")

target_countries <- c( "Uganda", "Cameroon","Nigeria", "Nepal", "Ghana", "Bhutan", "Burkina Faso", "Pakistan", "Zambia", "Liberia",
                       "Cote d'Ivoire", "Malawi", "Mozambique", "Lebanon", "Botswana", "Honduras",
                       "Democratic Republic of the Congo", "Gambia", "Argentina")


aqli_gadm1   <- read_csv("./data/gadm1_2024_wide.csv")
aqli_gadm1 <- aqli_gadm1 %>% filter(country %in% target_countries)

#aqli_gadm1$name_1[aqli_gadm1$country == "Nepal" & aqli_gadm1$name_1 == "Province 1"] <- "Koshi"

aqli_gadm2   <- read_csv("./data/gadm2_2024_wide.csv")
aqli_gadm2 <- aqli_gadm2 %>% filter(country %in% target_countries)

#aqli_gadm2$name_1[aqli_gadm2$country == "Nepal" & aqli_gadm2$name_1 == "Province 1"] <- "Koshi"

# Only run once locally — this replaces the heavy st_read every time
# gadm1_shp_pm <- gadm1_shp %>%
#   left_join(aqli_gadm1, by = c("name0" = "country", "name1" = "name_1")) %>%
#   select(starts_with("pm"), name0, name1, population, whostandard, natstandard, geometry)
# 
# gadm1_shp_pm <- gadm1_shp_pm %>%
#   select(starts_with("pm"), name0, name1, population, whostandard, natstandard, geometry) %>%
#  # name2 mutate(name1_id = paste0(name1, " (", obidgadm1, ")")) %>%
#   setNames(c(as.character(1998:2024), "name0", "name1", "population", "whostandard", "natstandard", "geometry"))
# 
# gadm2_shp_pm <- gadm2_shp %>%
#   left_join(aqli_gadm2, by =  c("name0" = "country", "name1" = "name_1", "name2" = "name_2")) %>%
#   select(starts_with("pm"), name0, name1, name2, population, whostandard, natstandard, geometry)
# 
# gadm2_shp_pm <- gadm2_shp_pm %>%
#   select(starts_with("pm"), name0, name1, name2, population, whostandard, natstandard, geometry) %>%
#   # mutate(name1_id = paste0(name1, " (", obidgadm1, ")")) %>%
#   setNames(c(as.character(1998:2024), "name0", "name1", "name2", "population", "whostandard", "natstandard", "geometry"))


# gadm1_shp_pm <- qs2::qs_read("./data/shapefile/gadm1_shp_pm.qs2")
# gadm2_shp_pm <- qs2::qs_read("./data/shapefile/gadm2_shp_pm.qs2")

table_new <- sensor_data %>%
  dplyr::filter(year %in% c(2025, 2026)) %>%
  dplyr::group_by(name0, name1, name2, year) %>%
  dplyr::summarise(
    tot_sensor = dplyr::n_distinct(sensors_id),
    tot_awardees = dplyr::n_distinct(owner),
    data_comp = round(mean(coverage.percentComplete, na.rm = TRUE), 1),
    report_period = dplyr::n_distinct(month),
    avg_pm = round(mean(pm25, na.rm = TRUE), 2),
    .groups = "drop"
  )

table_new <- table_new %>%
  dplyr::rename(
    Country = name0,
    State = name1,
    District = name2,
    Year = year,
    `Total Sensors` = tot_sensor,
    `Total Awardees` = tot_awardees,
    `Data Completeness (%)` = data_comp,
    `Reporting Months` = report_period,
    `Average PM2.5` = avg_pm
  )


##############################################################################################


openaq_month_trend_d <- sensor_data %>% group_by(name0,name1, name2, month, year) %>% summarise(pm25 = round(mean(pm25, na.rm=T),2)) %>% 
                                      filter(year %in% c(2025, 2026)) %>%
                                      mutate(month_name = month.abb[month])
openaq_month_trend_d <- as.data.table(openaq_month_trend_d)

openaq_month_trend_s <- sensor_data %>% group_by(name0,name1, month, year) %>% summarise(pm25 = round(mean(pm25, na.rm=T),2)) %>% 
  filter(year %in% c(2025, 2026)) %>%
  mutate(month_name = month.abb[month])

openaq_month_trend_s <- as.data.table(openaq_month_trend_s)

openaq_month_trend_c <- sensor_data %>% group_by(name0, month, year) %>% summarise(pm25 = round(mean(pm25, na.rm=T),2)) %>% 
  filter(year %in% c(2025, 2026)) %>%
  mutate(month_name = month.abb[month])

openaq_month_trend_c <- as.data.table(openaq_month_trend_c)

district_data_heatmap <- openaq_month_trend_c

district_data_heatmap$name0[district_data_heatmap$name0 == "Democratic Republic of the Congo" ]<-"DRC"

# =========================================================
# Pre-compute Summary Tables
# Put this inside server, before output$gis_table
# =========================================================
openaq_trend_d <- sensor_data %>% group_by(name0,name1, name2, year) %>% summarise(pm25 = round(mean(pm25, na.rm=T),2)) %>% 
  filter(year %in% c(2025, 2026)) 

aqli_gadm2_narrow <- read_csv("./data/gadm2_2024_narrow_aqf.csv")
# 
# 
# data_long <- aqli_gadm2_narrow %>%
#   
#   # Remove objectid_gadm2 and rename columns
#   select(-objectid_gadm2) %>%
#   rename(
#     name0 = country,
#     name1 = name_1,
#     name2 = name_2
#   ) %>%
#   
#   # Pivot PM2.5 columns to long format
#   pivot_longer(
#     cols = starts_with("pm"),
#     names_to = "year",
#     values_to = "pm25"
#   ) %>%
#   
#   # Extract year from column names (pm1998 -> 1998)
#   mutate(
#     year = as.integer(gsub("pm", "", year))
#   ) %>%
#   
#   # Keep columns in desired order
#   select(
#     name0,
#     name1,
#     name2,
#     population,
#     whostandard,
#     natstandard,
#     year,
#     pm25
#   )
# library(stringi)
# aqli_gadm2_narrow <- data_long %>% mutate(
#   name0 = stri_trans_general(name0, "Latin-ASCII"),
#   name1 = stri_trans_general(name1, "Latin-ASCII"),
#   name2 = stri_trans_general(name2, "Latin-ASCII")
# 
# )
# 
# aqli_gadm2_narrow <- aqli_gadm2_narrow %>% 
#                                  rename(
#                                         pm_aqli = pm25) %>% select("name0", "name1", "name2", "population", "whostandard", "natstandard", "year",
#                                                                  "pm_aqli"   )
#   
# write_csv(aqli_gadm2_narrow, "./data/gadm2_2024_narrow_aqf.csv")
who_std <- aqli_gadm2_narrow %>% select(name0, name1,name2, population,whostandard, natstandard) %>% unique()


openaq_trend_bal <- left_join(openaq_trend_d, who_std, by = c("name0" = "name0", "name1" = "name1", "name2" = "name2"))

openaq_trend_bal <- openaq_trend_bal %>% select("name0", "name1", "name2", "population", "whostandard", "natstandard", "year", 
       "pm25"   )

final_data <- bind_rows(openaq_trend_bal,aqli_gadm2_narrow)

# final_data_country_lvl <-final_data %>% group_by(name0,whostandard,natstandard, year) %>% summarise(tot = sum(population, na.rm = T),
#                                                                                         pm25 = round(mean(pm25, na.rm=T),2),
#                                                                                         pm_aqli = round(mean(pm_aqli, na.rm=T),2))



final_data_country_lvl <- final_data %>%
  group_by(name0, whostandard, natstandard, year) %>%
  summarise(
    tot = sum(population, na.rm = TRUE),
    pm25 = round(
      weighted.mean(pm25, w = population, na.rm = TRUE),
      2
    ),
    pm_aqli = round(mean(pm_aqli, na.rm=T),2),
    .groups = "drop"
  ) %>% filter(!is.na(whostandard))

final_data_country_lvl <- as.data.table(final_data_country_lvl)


# =========================================================

final_data_country_name2_lvl <- final_data %>%
  group_by(name0, name1, name2, whostandard, natstandard, year) %>%
  summarise(
    tot = sum(population, na.rm = TRUE),
    pm25 = round(
      weighted.mean(pm25, w = population, na.rm = TRUE),
      2
    ),
    pm_aqli = round(mean(pm_aqli, na.rm=T),2),
    .groups = "drop"
  ) %>% filter(!is.na(whostandard))

final_data_country_name2_lvl <- as.data.table(final_data_country_name2_lvl)


# Convert NaN to NA
final_data_country_name2_lvl[is.nan(pm25), pm25 := NA_real_]
final_data_country_name2_lvl[is.nan(pm_aqli), pm_aqli := NA_real_]
final_data_country_name2_lvl[is.nan(natstandard), natstandard := NA_real_]

# Fill national standard by same location
final_data_country_name2_lvl[
  ,
  natstandard := natstandard[which(!is.na(natstandard))[1]],
  by = .(name0, name1, name2)
]

# Combine PM columns: use pm25 where available, otherwise pm_aqli
final_data_country_name2_lvl[, pm25_final := fcoalesce(pm25, pm_aqli)]

# Remove old PM columns
final_data_country_name2_lvl[, c("pm25", "pm_aqli") := NULL]

# Rename final PM column
setnames(final_data_country_name2_lvl, "pm25_final", "pm25")

# Clean column order
setcolorder(
  final_data_country_name2_lvl,
  c("name0", "name1", "name2", "whostandard", "natstandard", "year", "tot", "pm25")
)
# =========================================================
# Reactable Drilldown
# Country -> State -> District
# # =========================================================
# 
# gadm1_shp <- gadm1_shp %>%
#   sf::st_make_valid() %>%
#   sf::st_transform(4326)
# 
# gadm2_shp <- gadm2_shp %>%
#   sf::st_make_valid() %>%
#   sf::st_transform(4326)
# 
# gadm1_shp_pm <- gadm1_shp_pm %>%
#   sf::st_make_valid() %>%
#   sf::st_transform(4326)
# 
# gadm2_shp_pm <- gadm2_shp_pm %>%
#   sf::st_make_valid() %>%
#   sf::st_transform(4326)
# 
# # Save as qs2
# qs2::qs_save(gadm1_shp, "data/shapefile/gadm1_shp.qs2")
# qs2::qs_save(gadm2_shp, "data/shapefile/gadm2_shp.qs2")
# 
# qs2::qs_save(gadm1_shp_pm, "data/shapefile/gadm1_shp_pm.qs2")
# qs2::qs_save(gadm2_shp_pm, "data/shapefile/gadm2_shp_pm.qs2")
