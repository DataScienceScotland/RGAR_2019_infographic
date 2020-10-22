library(dplyr)
library(readxl)
library(tidyr)
library(here)
library(purrr)
library(lubridate)
library(forcats)
library(stringr)

library(ggplot2)
library(scales)

path <- "NRS - RGAR 2019 - Infographic - 10 - data - 06.xlsx"

datasets <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)

# 1 COVID -----------------------------------------------------------------
covid_deaths <- datasets[["covid_deaths"]] %>%
  mutate(week_beg = as.Date(week_beg))

covid_deaths_points <- covid_deaths %>%
  filter(week_beg %in% c(min(week_beg), max(week_beg)) | deaths == max(deaths))

covid_location <- datasets[["covid_location"]] %>%
  select(-source) %>%
  gather(key = "location", value = "count", -week_num, -week_start) %>%
  mutate(points = ifelse(week_num %in% c(max(week_num), min(week_num)),
                         count,
                         NA),
         week_start = as.Date(week_start))

simd_urban_rural <- datasets[["simd_urban_rural"]] %>%
  select(-source) %>%
  mutate(label = as_factor(label) %>% fct_rev())

# 2 POPULATION ------------------------------------------------------------
pop_time_breaks <- c(1801, 1855, 1900, 1939, 1974, 2000, 2019, 2043)

pop_time_estimate <- datasets[["pop_time"]] %>%
  filter(measure == "estimate")

pop_time_projection = datasets[["pop_time"]] %>%
  filter(source == "MYP")

pop_time_variants = datasets[["pop_time"]] %>%
  filter(measure == "projection")

pop_time_variants_wide = datasets[["pop_time"]] %>%
  filter(source %in% c("variant_high", "variant_low")) %>%
  select(year, count, source) %>%
  spread(key = source, value = count)

pop_time_principle <- datasets[["pop_time"]] %>%
  filter(measure == "estimate" | source == "MYP")

pop_time_points <- pop_time_estimate %>%
  filter(year %in% c(pop_time_breaks, 1851))

pop_time_text <- datasets[["pop_time"]] %>%
  filter(year %in% c(pop_time_breaks, 2043))

nat_change_migration <- datasets[["nat_change_migration"]] %>%
  gather(key = "measure", value = "net_count",-year,-real_period) %>%
  mutate(
    point = case_when(
      year %in% c(min(year), max(year)) ~ net_count,
      year %in% c(1967, 1974) &
        measure == "nat_change" ~ net_count,
      year %in% c(2004, 2016) &
        measure == "net_migration" ~ net_count,
      TRUE ~ NA_real_
    )
  )


pop_age <- datasets[["pop_age"]] %>%
  select(-source_url) %>%
  gather(key = "age_group", value = "rate", -c(year, measure)) %>%
  mutate(age_group = as_factor(age_group) %>% fct_rev())


# 4 NATIONALITY, FERTILITY, HOUSEHOLDS ------------------------------------
# NATIONALITY -------------------------------------------------------------
nationalities <- datasets[["nationalities"]] %>%
  mutate(region = as_factor(region),
         country = as_factor(country) %>% fct_rev())

non_british <- datasets[["non-british"]] %>%
  arrange(`EU_non-EU`, -estimate) %>%
  mutate(
    region = as_factor(region),
    fraction = estimate / sum(estimate),
    ymax = cumsum(fraction),
    ymin = c(lag(ymax, 1, default = 0)),
    ymid = (ymin + ymax) / 2
  )

# FERTILITY ---------------------------------------------------------------
fertility <- datasets[["fertility"]] %>%
  rename(N_Ireland = `N. Ireland`) %>%
  gather(key = "country", value = "rate", -Year) %>%
  drop_na() %>%
  group_by(country) %>% 
  mutate(points = ifelse(Year %in% c(max(Year), min(Year)), rate, NA))

# HOUSEHOLD ---------------------------------------------------------------
household <- datasets[["household"]] %>%
  select(-publication) %>%
  gather(key = "size", value = "rate", -year, -source) %>%
  filter(!(year == 2011 & source == "census")) %>%
  group_by(source, size) %>% 
  mutate(points = ifelse(year %in% c(max(year), min(year)), rate, NA),
         source_size = paste0(source, "_", size)) %>%
  ungroup()

# 5 LIFE EXPECTANCY -------------------------------------------------------
# TIME SERIES -------------------------------------------------------------
le <- datasets[["le"]] %>%
  select(-source) %>%
  gather(key = "sex", value = "le", male, female) %>%
  mutate(points = ifelse(year %in% c(min(year), max(year)), le, NA))

# EUROPEAN COUNTRIES ------------------------------------------------------
le_EU <- datasets[["le_EU"]] %>%
  gather(key = "country", value = "le",-c(year, year_label, sex)) %>%
  mutate(region = case_when(
    country %in% c("Scotland", "UK") ~ "EU_UK",
    country %in% c(
      "Slovenia",
      "Lithuania",
      "Czechia",
      "Slovakia",
      "Estonia",
      "Poland",
      "Bulgaria",
      "Hungary",
      "Romania",
      "Croatia",
      "Latvia"
    ) ~ "EU_East",
    country %in% c(
      "Finland",
      "Sweden",
      "Spain",
      "Greece",
      "Denmark",
      "France",
      "Italy",
      "Ireland",
      "Luxembourg",
      "Cyprus",
      "Austria",
      "Netherlands",
      "Belgium",
      "Germany",
      "Malta",
      "Portugal"
    ) ~ "EU_West"
  )) %>%
  group_by(region, year, sex) %>%
  mutate(region_min = min(le, na.rm = TRUE),
         region_max = max(le, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(points = ifelse(year %in% c(max(year), min(year)), le, NA))

le_EU_UK <- le_EU %>%
  filter(region == "EU_UK")

le_EU_West <- le_EU %>%
  filter(region == "EU_West")

le_EU_East <- le_EU %>%
  filter(region == "EU_East")

# SIMD --------------------------------------------------------------------
le_simd <- datasets[["le_simd"]] %>%
  mutate(simd_label = as_factor(simd_label) %>% fct_rev(),
         simd_sex = paste0(simd, "_", sex) %>% as_factor())

le_simd_extrema <- le_simd %>%
  filter(simd %in% c(1, 10))
