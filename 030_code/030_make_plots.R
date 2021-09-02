# make plots of:
# Chicks per year
# Average of chicks per nested box per year
# For each state

# Setup ------------------------------------------------------------------------

v = "030"
rfile = "make_plots"

set.seed(100)

# The data
library(here)
library(readxl)
library(readr)
library(tidyverse)

nj_nestboxes <- read_csv(here::here(paste0(v, "_data"),
                                    "2020_NJ.csv"))  %>%
  head(-3) %>%
  bind_rows(read_excel("010_data/2020_NJ_smallwood.xlsx") %>%
                     dplyr::rename(Year = YEAR,
                                   chicks_banded = `BANDED CHICKS`,
                                   chicks_per_box = `AVG BANDED CHICKS/ATTEMPT`) %>%
              dplyr::mutate(Org = "Smallwood")) %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year)) %>%
  dplyr::select(org, year, chicks_per_box, chicks_banded)

pa_nestboxes <- read_excel(here::here(paste0(v, "_data"),
                                      "2020_PA.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year)) %>%
  dplyr::rename(chicks_per_box = chicks_nested_box)


# colors
library(viridis)
scales::show_col(watlington(16)) #look at colors
watlington(16)
# CRAN version
library(pals)
pal.bands(coolwarm, parula, ocean.haline, brewer.blues, cubicl, kovesi.rainbow, ocean.phase, brewer.paired(12), stepped)
labs=c('alphabet','alphabet2', 'glasbey','kelly','polychrome', 'stepped', 'stepped2', 'stepped3', 'tol', 'watlington')
op=par(mar=c(0,5,3,1))
pal.bands(alphabet(), alphabet2(), glasbey(), kelly(),
          polychrome(), stepped(), stepped2(), stepped3(), tol(), watlington(),
          labels=labs, show.names=FALSE)
pal10 <- watlington(11)[c(1:7,9:11)]
scales::show_col(pal10)
gg_color <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
# RColorBrewer
library(RColorBrewer)
RColorBrewer::display.brewer.all()
display.brewer.pal(10, "Paired")
pal_paired_10 <- brewer.pal(10, "Paired")

# other libraries
library(gridExtra)
library(grid)
library(directlabels) 

# files to source
source(here::here(paste0(v, "_code"),paste0("general_functions.R"))) 
source(here::here(paste0(v, "_code"),paste0(v, "_functions.R"))) 

new_rfiles(rfile, v)

# height and width of plots
w = 7
h = 5
h_cum = 10



# ***************************************************************************
# *** Just NJ Land Trust and PA Conservancy ------------------------------------------------------------
# ***************************************************************************


# combine data ------------------------------------------------------------

nestboxes_clean <- pa_nestboxes %>% 
  dplyr::bind_rows(nj_nestboxes) %>% 
  dplyr::filter(org %in% c("Central PA Conservancy",
                           "Natural Lands"))

# chicks per year -----------------------------------------------------

# by organization
nestboxes_clean %>%
  dplyr::filter(chicks_banded!=0) %>%
  kestrel_plot_chicks_per_year(region = "Pennsylvania and New Jersey", 
                               combined = FALSE, 
                               labels_as_points = FALSE, 
                               label_col = chicks_banded) +
  scale_color_manual(values = c("#FF8F99", "#3A8E4E"))
save_ggplot("pa_nj_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")


# chicks per box ------------------------------------------------------

# by org
nestboxes_clean %>%
  dplyr::filter(chicks_per_box != 0) %>%
  drop_na(chicks_per_box) %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Pennsylvania and New Jersey") +
  scale_color_manual(values = c("#FF8F99", "#3A8E4E")) +
  expand_limits(y = 2.9)
save_ggplot("pa_nj_chicks_per_nested_box.png", rfile, v, width = w, height = h, units = "in")


# ***************************************************************************
# *** Calculate standard errors -----------------------------------------------
# ***************************************************************************

# Start by loading data ---------------------------------------------------

pa_2018 <- read_excel(here::here(paste0("020_data"),
                                    "2018_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(pa_year, nestbox_id, number_young_banded, 
              banding_date, .direction = "down") %>%
  dplyr::rename(year = pa_year) %>% 
  rowwise() %>% 
  dplyr::mutate(total_young = number_young_banded,
                state = "PA") %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
nj_2018 <- read_excel(here::here(paste0("020_data"),
                                 "2018_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(year, nestbox_id, number_young_banded, 
              banding_date, .direction = "down") %>%
  rowwise() %>% 
  dplyr::mutate(total_young = number_young_banded,
                state = "NJ") %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
nj_2019 <- read_excel(here::here(paste0("020_data"),
                                 "2019_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(date_1st_observed, nestbox_id, number_young_banded, 
              banding_date, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1892) %>% 
  rowwise() %>% 
  dplyr::mutate(total_young = number_young_banded,
                year = 2019,
                state = "NJ") %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
pa_2019 <- read_excel(here::here(paste0("020_data"),
                                 "2019_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(year, date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, status, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1892) %>%
  dplyr::select(-c("x14","x15")) %>%
  rowwise() %>% 
  dplyr::mutate(state = "PA",
                total_young = sum(number_young_banded, number_unbanded_young_to_12_day_banding_age, na.rm=TRUE)) %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
nj_2020 <- read_excel(here::here(paste0("020_data"),
                                 "2020_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(nestbox_id = x2020_nestbox_id,
                band_number = band_number_1893) %>%
  tidyr::fill(date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, status, .direction = "down") %>%
  rowwise() %>% 
  dplyr::mutate(year = 2020,
                total_young = sum(number_young_banded, number_unbanded_young_to_12_day_banding_age, na.rm=TRUE)) %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
pa_2020 <- read_excel(here::here(paste0("020_data"),
                                 "2020_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(band_number = band_number_1893,
                date_1st_observed = date_1st_observed_as_active) %>%
  dplyr::select(-c("x15")) %>%
  tidyr::fill(nestbox_id, number_young_banded, 
              banding_date, status, year, .direction = "down") %>%
  rowwise() %>% 
  dplyr::mutate(state = "PA",
                band_number = as.numeric(band_number),
                total_young = sum(number_young_banded, number_unbanded_young_to_12_day_banding_age, na.rm=TRUE)) %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
pa_2021 <- read_excel(here::here(paste0("020_data"),
                                 "2021_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(band_number = band_number_1893,
                date_1st_observed = date_1st_observed_as_active) %>%
  tidyr::fill(nestbox_id, 
              status, .direction = "down") %>%
  rowwise() %>% 
  dplyr::mutate(state = "PA",
                band_number = as.numeric(band_number),
                number_young_banded = sum(
                  number_young_banded_s_mount,
                  number_young_banded_u_mount,
                  na.rm=TRUE
                ),
                total_young = sum(
                  number_young_banded_s_mount,
                  number_young_banded_u_mount, 
                  number_unbanded_young_to_12_day_banding_age_u, 
                  na.rm=TRUE
                )) %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
nj_2021 <- read_excel(here::here(paste0("020_data"),
                                 "2021_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(weight_in_grams = weightin_grams) %>%
  tidyr::fill(nestbox_id, number_young_banded,
              status, .direction = "down") %>%
  rowwise() %>% 
  dplyr::mutate(state = "NJ",
                band_number = as.numeric(band_number),
                total_young = sum(number_young_banded, unbanded_documented_to_banding_age, na.rm=TRUE)) %>% 
  # distinct will take only the first row with each unique combination
  dplyr::distinct(nestbox_id, state, year, .keep_all = TRUE) %>% 
  dplyr::select(nestbox_id, state, year, total_young, number_young_banded)
# combine
combined <- dplyr::bind_rows(nj_2018, nj_2019, nj_2020, nj_2021, pa_2018, pa_2019, pa_2020, pa_2021) %>% 
  arrange(state, year)
combined_summary <- combined %>% 
  dplyr::group_by(state, year) %>% 
  dplyr::summarise(n_boxes = n(),
                   mean_chicks_per_box = mean(total_young),
                   sum_chicks_banded = sum(number_young_banded),
                   sd_chicks_per_box = sd(total_young),
                   se_chicks_per_box = sd(total_young)/sqrt(n()),
                   med_cpb = median(total_young),
                   q1_cpb = quantile(total_young, 0.25),
                   q3_cpb = quantile(total_young, 0.75))
# ***************************************************************************
# *** Plots with SE ---------------------------------------------------------
# ***************************************************************************

# clean up summary data with SEs
se_clean <- combined_summary %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year)) %>%
  dplyr::mutate(chicks_per_box = round(mean_chicks_per_box, digits = 2),
                org = ifelse(
                  state == "PA",
                  "Central PA Conservancy",
                  "Natural Lands"
                ))

# chicks per box ------------------------------------------------------

# all years
nestboxes_clean %>%
  dplyr::filter(chicks_per_box != 0) %>%
  drop_na(chicks_per_box) %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1),
                year_value = lubridate::year(year)) %>%
  dplyr::full_join(dplyr::select(se_clean, se_chicks_per_box, year, org),
                   by = c("year","org")) %>% 
  kestrel_plot_chicks_per_box(region = "Pennsylvania and New Jersey", se = TRUE) +
  scale_color_manual(values = c("#FF8F99", "#3A8E4E")) +
  expand_limits(y = 2.9) +
  labs(caption = paste0("Error bars from 2018 to 2021 indicate ±1 standard error."))
save_ggplot("pa_nj_chicks_per_nested_box_se_allyears.png", rfile, v, width = w, height = h, units = "in")


# by org with SE
se_clean %>%
  kestrel_plot_chicks_per_box(region = "Pennsylvania and New Jersey",
                              se = TRUE) +
  scale_color_manual(values = c("#FF8F99", "#3A8E4E")) +
  expand_limits(y = 2.9, x = as.Date("2021-02-05")) +
  labs(caption = paste0("Error bars indicate ±1 standard error."))
save_ggplot("pa_nj_chicks_per_nested_box_se.png", rfile, v, width = w, height = h, units = "in")


# ***************************************************************************
# *** Try boxplots ---------------------------------------------------------
# ***************************************************************************

# chicks per box with boxplots ------------------------------------------------------

# by org
combined %>%
  dplyr::mutate(year = as.factor(year)) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(aes(x = year, y = total_young, fill = state))

