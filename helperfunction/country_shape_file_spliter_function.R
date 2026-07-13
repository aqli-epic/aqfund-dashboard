library(sf)
library(dplyr)
library(qs2)

dir.create("data/map_cache", recursive = TRUE, showWarnings = FALSE)

safe_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

target_countries <- sort(unique(openaq_data_state$name0))
target_years <- sort(unique(openaq_data_state$year))

# Use your current objects
gadm1_base <- gadm1_shp_pm %>%
  dplyr::filter(name0 %in% target_countries) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  sf::st_simplify(dTolerance = 0.01, preserveTopology = TRUE)

gadm2_base <- gadm2_shp %>%
  dplyr::filter(name0 %in% target_countries) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  sf::st_simplify(dTolerance = 0.01, preserveTopology = TRUE)

for (ctry in target_countries) {
  
  for (yr in target_years) {
    
    message("Saving PM2.5 cache: ", ctry, " - ", yr)
    
    state_df <- gadm1_base %>%
      dplyr::filter(name0 == ctry) %>%
      dplyr::left_join(
        openaq_data_state %>%
          dplyr::filter(name0 == ctry, year == yr),
        by = c("name0", "name1")
      )
    
    district_df <- gadm2_base %>%
      dplyr::filter(name0 == ctry) %>%
      dplyr::left_join(
        openaq_data_district %>%
          dplyr::filter(name0 == ctry, year == yr),
        by = c("name0", "name1", "name2")
      )
    
    qs2::qs_save(
      state_df,
      paste0("data/map_cache/", safe_name(ctry), "_", yr, "_state_pm25.qs2")
    )
    
    qs2::qs_save(
      district_df,
      paste0("data/map_cache/", safe_name(ctry), "_", yr, "_district_pm25.qs2")
    )
    
    rm(state_df, district_df)
    gc()
  }
}

rm(gadm1_base, gadm2_base)
gc()


library(sf)
library(dplyr)
library(qs2)

dir.create("data/map_cache_satellite", recursive = TRUE, showWarnings = FALSE)

safe_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

target_countries <- sort(unique(gadm1_shp_pm$name0))

gadm1_sat_base <- gadm1_shp_pm %>%
  dplyr::filter(name0 %in% target_countries) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  sf::st_simplify(dTolerance = 0.01, preserveTopology = TRUE)

gadm2_sat_base <- gadm2_shp_pm %>%
  dplyr::filter(name0 %in% target_countries) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  sf::st_simplify(dTolerance = 0.01, preserveTopology = TRUE)

for (ctry in target_countries) {
  
  message("Saving satellite cache: ", ctry)
  
  state_df <- gadm1_sat_base %>%
    dplyr::filter(name0 == ctry)
  
  district_df <- gadm2_sat_base %>%
    dplyr::filter(name0 == ctry)
  
  qs2::qs_save(
    state_df,
    paste0("data/map_cache_satellite/", safe_name(ctry), "_state_satellite.qs2")
  )
  
  qs2::qs_save(
    district_df,
    paste0("data/map_cache_satellite/", safe_name(ctry), "_district_satellite.qs2")
  )
  
  rm(state_df, district_df)
  gc()
}

rm(gadm1_sat_base, gadm2_sat_base)
gc()