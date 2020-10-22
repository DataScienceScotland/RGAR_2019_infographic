# 1 COVID -----------------------------------------------------------------
# DEATHS ------------------------------------------------------------------
ggplot(data = covid_deaths,
       mapping = aes(x = Date)) +
  geom_col(mapping = aes(y = deaths_excess),
           width = 1,
           fill = col_neut_silver) +
  geom_line(mapping = aes(y = deaths_nrs),
            size = info_line_size,
            colour = col_nrs_purple,
            lineend = "round") +
  geom_point(data = covid_deaths_points,
             mapping = aes(y = count),
             size = info_point_size,
             colour = col_nrs_purple) +
  geom_line(mapping = aes(y = deaths_hps),
            size = info_line_size,
            colour = col_neut_tundora,
            lineend = "round") +
  geom_point(data = covid_deaths_points,
             mapping = aes(y = count),
             colour = col_neut_tundora,
             size = info_point_size) +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x) format(x, "%b")
  ) +
  theme_info() +
  theme(
    axis.text.y = element_blank()
  )

ggplot(data = covid_deaths,
       mapping = aes(x = Date)) +
  geom_hline(yintercept = 0,
             colour = col_neut_tundora,
             linetype = linetype_annotation_small) +
  geom_step(mapping = aes(y = deaths_excess_avg),
            size = info_line_size,
            colour = col_neut_grey,
            lineend = "round") +
  geom_line(mapping = aes(y = deaths_nrs_7day_avg),
            size = info_line_size,
            colour = col_nrs_purple,
            lineend = "round") +
  geom_line(mapping = aes(y = deaths_hps_7day_avg),
            size = info_line_size,
            colour = col_neut_tundora,
            lineend = "round") +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x) format(x, "%b")
  ) +
  theme_info()

ggsave(
  filename = "1.1_covid_deaths.svg",
  width = 145,
  height = 80,
  units = "mm")

ggplot(data = covid_deaths,
       mapping = aes(x = week_beg,
                     y = deaths)) +
  geom_line(size = info_line_size,
            colour = col_nrs_purple) +
  geom_point(data = covid_deaths_points,
             colour = col_nrs_purple,
             size = info_point_size) +
  geom_text(
    data = covid_deaths_points,
    mapping = aes(label = deaths),
    family = "Segoe UI",
    size = info_text_size,
    colour = col_nrs_purple
  ) +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x)
      format(x, "%b")
  ) +
  theme_info() +
  theme(
    axis.text.y = element_blank()
  )

ggsave(
  filename = "1.1_covid_deaths.svg",
  width = 152,
  height = 80,
  units = "mm")

# LOCATION ----------------------------------------------------------------
ggplot() +
  geom_line(
    data = covid_location,
    mapping = aes(
      x = week_start,
      y = count,
      colour = location,
      linetype = location
    ),
    size = info_line_size,
    lineend = "round"
  ) +
  geom_point(data = data.frame(x = as.Date("2020-08-01"),
                               y = 16256 / 52),
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/2018/section-6-death-causes
             mapping = aes(x = x,
                           y = y)) +
  scale_x_date(
    date_breaks = "1 month",
    labels = function(x)
      format(x, "%b")
  ) +
  scale_linetype_manual(
    values = c(
      "care_home" = linetype_annotation_small,
      "hospital" = "solid",
      "home_non-institution" = "solid",
      "other_institution" = linetype_annotation_small
    )
  ) +
  scale_colour_manual(
    values = c(
      "care_home" = col_nrs_purple,
      "hospital" = col_neut_grey,
      "home_non-institution" = col_nrs_purple,
      "other_institution" = col_neut_grey
    )
  ) +
  theme_info()

ggsave(
  filename = "1.2_covid_location.svg",
  width = 160,
  height = 80,
  units = "mm")

# SIMD URBAN RURAL --------------------------------------------------------
ggplot(data = simd_urban_rural,
       mapping = aes(x = label,
                     y = rate)) +
  geom_col(width = 0.66,
           fill = c(rep(c(col_neut_silver, col_nrs_purple), 5), col_neut_silver)) +
  geom_text(
    mapping = aes(
      y = max(rate) / 100,
      label = label
    ),
    family = "Segoe UI",
    size = info_text_size,
    hjust = 0,
    colour = c(rep(c("black", "white"), 5), "black")
  ) +
  geom_text(
    mapping = aes(y = rate,
                  label = format(rate, digits = 3)),
    colour = c(rep(c("black", "white"), 5), "black"),
    family = "Segoe UI",
    size = info_text_size,
    hjust = 1,
    nudge_y = -1
  ) +
  coord_flip() +
  theme_info() +
  theme(
    axis.text = element_blank()
  )

ggsave(
  filename = "1.3_simd_urban_rural.svg",
  width = 167,
  height = 70,
  units = "mm")

# 2 POPULATION ------------------------------------------------------------
# TIME SERIES -------------------------------------------------------------
ggplot(mapping = aes(x = year)) +
  geom_ribbon(
    data = pop_time_variants_wide,
    mapping = aes(ymin = variant_low,
                  ymax = variant_high),
    fill = col_neut_silver
  ) +
  geom_line(
    data = pop_time_estimate,
    mapping = aes(y = count,
                  group = source),
    size = info_line_size,
    colour = col_nrs_purple
  ) +
  geom_line(
    data = pop_time_variants,
    mapping = aes(y = count,
                  group = source),
    colour = col_nrs_purple,
    size = info_line_size / 2,
    linetype = linetype_annotation_small,
    lineend = "round"
  ) +
  geom_point(data = pop_time_points,
             mapping = aes(y = count),
             size = info_point_size,
             colour = col_nrs_purple) +
  geom_text(data = pop_time_text,
            mapping = aes(y = count,
                          label = round(count / 1000000, digits = 2)),
            family = "Segoe UI",
            size = info_text_size) +
  scale_x_continuous(breaks = pop_time_breaks) +
  theme_info() +
  theme(axis.text.y = element_blank())

ggsave(
  filename = "2.1_pop_time.svg",
  width = 150,
  height = 85,
  units = "mm")

# AGE GROUP ---------------------------------------------------------------
ggplot(mapping = aes(x = year,
                     y = rate)) +
  geom_area(data = filter(pop_age, measure == "estimate"),
            mapping = aes(fill = age_group),
            colour = "white",
            size = info_line_size) +
  geom_area(data = filter(pop_age, measure == "projection"),
            mapping = aes(fill = age_group),
            colour = "white",
            size = info_line_size) +
  scale_fill_manual(values = c(col_neut_tundora,
                               col_neut_grey,
                               col_neut_silver)) +
  scale_x_continuous(breaks = c(1911, 2019, 2043)) +
  scale_y_continuous(labels = scales::percent) +
  theme_info()

ggsave(
  filename = "2.2_pop_age.svg",
  width = 160,
  height = 80,
  units = "mm")

# NATURAL CHANGE NET MIGRATION --------------------------------------------
ggplot(data = nat_change_migration,
       mapping = aes(x = year)) +
  geom_line(mapping = aes(y = net_count,
                          colour = measure),
            size = info_line_size,
            lineend = "round") +
  geom_point(mapping = aes(y = point,
                           colour = measure),
             size = info_point_size) +
  geom_text(mapping = aes(y = point,
                          label = point,
                          colour = measure),
            hjust = c(1, rep(0, 60), 1, rep(0, 60)),
            nudge_x = c(-1, rep(1, 60), -1, rep(1, 60)),
            family = "Segoe UI") +
  geom_hline(yintercept = 0,
             colour = col_neut_tundora,
             linetype = linetype_annotation_small) +
  scale_x_continuous(breaks = c(1959, 1967, 1974, 2004, 2016, 2019)) +
  scale_y_continuous(breaks = c(0)) +
  scale_colour_manual(values = c(col_neut_tundora, col_nrs_purple)) +
  theme_info()

ggsave(
  filename = "2.3_nat_change_migration.svg",
  width = 150,
  height = 80,
  units = "mm")

# 4 NATIONALITY, FERTILITY, HOUSEHOLDS ------------------------------------
# NATIONALITY -------------------------------------------------------------
ggplot(data = nationalities,
       mapping = aes(x = country,
                     y = estimate * 1000)) +
  geom_col(width = info_col_width,
           fill = col_neut_grey) +
  geom_text(
    mapping = aes(label = scales::comma(estimate * 1000)),
    hjust = 1,
    family = "Segoe UI",
    colour = "white"
  ) +
  coord_flip() +
  scale_x_discrete(
    labels = function(x) {
      stringr::str_wrap(x, width = 15)
    }
  ) +
  theme_info() +
  theme(axis.text.y = element_text(hjust = 0),
        axis.text.x = element_blank())

ggsave(
  filename = "4.1_nationality.svg",
  width = 152,
  height = 70,
  units = "mm")

ggplot(data = non_british) +
  geom_rect(
    mapping = aes(
      ymax = ymax,
      ymin = ymin,
      xmax = 4,
      xmin = 3,
      fill = region
    ),
    colour = "white",
    size = 0.3
  ) +
  geom_rect(mapping = aes(
    ymax = ymax,
    ymin = ymin,
    xmax = 2.5,
    xmin = 2,
    fill = `EU_non-EU`
  ),
  colour = "white",
  size = 0.3) +
  geom_text(mapping = aes(
    label = stringr::str_wrap(paste0(region, " (", scales::comma(estimate), ")"), width = 10),
    x = 3.5,
    y = ymid
  ),
  family = "Segoe UI",
  size = info_text_size,
  colour = c("white", "white", "white", "black", "black", "black", "black")) +
  scale_fill_manual(
    values = c(
      col_neut_silver,
      col_nrs_purple,
      col_nrs_purple,
      col_nrs_purple,
      col_nrs_purple,
      col_nrs_purple,
      col_neut_silver,
      col_neut_silver,
      col_neut_silver
    )
  ) +
  xlim(c(2, 4)) +
  coord_polar(theta = "y") +
  theme_info() +
  theme(axis.text = element_blank())

ggsave(
  filename = "4.1_non-british.svg",
  width = 122,
  height = 95,
  units = "mm")

# FERTILITY ---------------------------------------------------------------
ggplot(data = fertility,
       mapping = aes(x = Year,
                     y = rate,
                     colour = country,
                     linetype = country)) +
  geom_line(size = info_line_size,
            lineend = "round") +
  geom_point(mapping = aes(y = points),
             size = info_point_size) +
  geom_text(mapping = aes(y = points,
                          label = rate)) +
  scale_x_continuous(breaks = c(1971, 2000, 2019)) +
  scale_colour_manual(values = c(
    "England" = col_nrs_purple,
    "N_Ireland" = col_neut_grey,
    "Wales" = col_neut_grey,
    "Scotland" = col_nrs_purple
  )) +
  scale_linetype_manual(values = c(
    "England" = linetype_annotation_small,
    "N_Ireland" = linetype_annotation_small,
    "Wales" = "solid",
    "Scotland" = "solid"
  )) +
  theme_info() +
  theme(
    axis.text.y = element_blank()
  )

ggsave(
  filename = "4.2_fertility.svg",
  width = 122,
  height = 82,
  units = "mm")

# HOUSEHOLD ---------------------------------------------------------------
ggplot(data = household,
       mapping = aes(x = year,
                     y = rate,
                     group = source_size,
                     linetype = size)) +
  geom_line(mapping = aes(colour = size),
            size = info_line_size,
            lineend = "round") +
  geom_point(mapping = aes(y = points,
                           colour = size),
             size = info_point_size) +
  scale_y_continuous(labels = function(x) {scales::percent(x, accuracy = 1)}) +
  scale_x_continuous(breaks = c(1961, 2002, 2018)) +
  scale_colour_manual(values = c(
    "1_person" = col_nrs_purple,
    "2_person" = col_neut_grey,
    "3+_person" = col_neut_tundora
  )) +
  scale_linetype_manual(values = c(
    "1_person" = "solid",
    "2_person" = linetype_annotation_small,
    "3+_person" = linetype_annotation_small
  )) +
  theme_info()

ggsave(
  filename = "4.3_household.svg",
  width = 138,
  height = 80,
  units = "mm")

# 5 - LIFE EXPECTANCY -----------------------------------------------------
# TIME SERIES -------------------------------------------------------------
ggplot(
  data = le,
  mapping = aes(
    x = year,
    y = le,
    group = sex
  )
) +
  geom_line(size = info_line_size,
            lineend = "round",
            colour = col_nrs_purple) +
  geom_point(mapping = aes(y = points),
             size = info_point_size,
             colour = col_nrs_purple) +
  geom_text(
    mapping = aes(
      label = round(points, digits = 1)
    ),
    family = "Segoe UI",
    size = info_text_size,
    colour = col_nrs_purple
  ) +
  scale_x_continuous(breaks = c(1981, 2017),
                     labels = c("1980-1982", "2017-2019")) +
  scale_y_continuous(breaks = seq(from = 70, to = 86, by = 2)) +
  scale_linetype_manual(values = c("estimate" = "solid",
                                   "projection" = linetype_annotation_small)) +
  theme_info() +
  theme(
    axis.text.y = element_blank()
  )
  
ggsave(
  filename = "5.1_le.svg",
  width = 130,
  height = 85,
  units = "mm")


# EUROPEAN COUNTRIES ------------------------------------------------------
ggplot(mapping = aes(x = year,
                     y = le)) +
  geom_ribbon(
    data = le_EU_West,
    mapping = aes(ymin = region_min,
                  ymax = region_max),
    fill = col_nrs_purple,
    alpha = 0.5
  ) +
  geom_ribbon(
    data = le_EU_East,
    mapping = aes(ymin = region_min,
                  ymax = region_max),
    fill = col_neut_grey,
    alpha = 0.5
  ) +
  geom_line(
    data = le_EU_UK,
    mapping = aes(linetype = country),
    size = info_line_size,
    colour = col_neut_tundora,
    lineend = "round"
  ) +
  geom_point(
    data = le_EU_UK,
    mapping = aes(y = points),
    size = info_point_size,
    colour = col_neut_tundora
  ) +
  scale_linetype_manual(values = c("UK" = linetype_annotation_small,
                                   "Scotland" = "solid")) +
  scale_x_continuous(breaks = c(1981, 2018),
                     labels = c("1980-1982", "2017-2019")) +
  scale_y_continuous(breaks = seq(from = 60, to = 85, by = 5)) +
  facet_grid(cols = vars(sex)) +
  theme_info()

ggsave(
  filename = "5.2_le_EU.svg",
  width = 145,
  height = 80,
  units = "mm")


# SIMD --------------------------------------------------------------------
ggplot(data = le_simd,
       mapping = aes(x = le,
                     y = simd,
                     shape = sex,
                     colour = sex)) +
  geom_point(size = info_point_size) +
  scale_y_continuous(breaks = 1:10, trans = "reverse") +
  scale_colour_manual(values = c(female = col_nrs_purple,
                                 male = col_neut_tundora)) +
  scale_shape_manual(values = c(female = "circle",
                                male = "diamond")) +
  theme_info()


ggplot(data = le_simd_extrema,
         mapping = aes(x = simd_sex,
                       y = le,
                       label = round(le, digits = 1))) +
  geom_col(size = info_col_size,
           width = info_col_width) +
  geom_text(
    family = "Segoe UI",
    size = info_text_size,
    hjust = 1,
    colour = "white"
  ) +
  coord_flip() +
  theme_info() +
  theme(
    axis.text = element_blank()
  )

ggsave(
  filename = "5.3_le_simd.svg",
  width = 135,
  height = 72,
  units = "mm")
