library(dplyr)
library(readr)




dt <- read_excel("./download/GADM2_with_dust_seasalt.xlsx")


dt$country[dt$country=="CÃ´te d'Ivoire"] <- "Cote d'Ivoire"
# PM2.5 columns

dt <- left_join(dt, aqli_continent_region, by = c("country"))
write_csv(dt, "./data/gadm2_weighted_pm25.csv")

pm_cols <- paste0("pm", 1998:2024)

#-----------------------------
# GADM2 -> GADM1
#-----------------------------
gadm1_pm25 <- dt %>%
  group_by(continent, region,country, name_1, whostandard, natstandard) %>%
  summarise(
    across(
      all_of(pm_cols),
      ~ sum(.x * population, na.rm = TRUE) /
        sum(population[!is.na(.x)], na.rm = TRUE)
    ),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(gadm1_pm25, "./data/gadm1_weighted_pm25.csv")

#-----------------------------
# GADM2 -> GADM0
#-----------------------------
gadm0_pm25 <- dt %>%
  group_by(continent,region, country,whostandard, natstandard) %>%
  summarise(
    across(
      all_of(pm_cols),
      ~ sum(.x * population, na.rm = TRUE) /
        sum(population[!is.na(.x)], na.rm = TRUE)
    ),
    population = sum(population, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(gadm0_pm25, "./data/gadm0_weighted_pm25.csv")


#################


dt_long <- dt %>%
  pivot_longer(
    cols = starts_with("pm"),
    names_to = "year",
    values_to = "pm_aqli"
  ) %>%
  mutate(
    year = as.integer(sub("pm", "", year))
  ) %>%
  select(
    name0 = country,
    name1 = name_1,
    name2 = name_2,
    population,
    whostandard,
    natstandard,
    year,
    pm_aqli
  )
dt_long$pm_aqli <- round(dt_long$pm_aqli,2)

write_csv(dt_long, "./data/gadm2_2024_narrow_pm25.csv")
