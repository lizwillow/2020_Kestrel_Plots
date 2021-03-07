# make plots of:
# Weight vs age
# Possibly broken up by:  
# - M/F
# - PA/NJ
# - 2018/2019/2020

# Setup ------------------------------------------------------------------------

v = "020"
rfile = "more_plots"

set.seed(100)

# packages
library(here)
library(readxl)
library(readr)
library(tidyverse)

# files to source
source(here::here(paste0(v, "_code"),paste0("general_functions.R"))) 
source(here::here(paste0(v, "_code"),paste0(v, "_functions.R"))) 

new_rfiles(rfile, v)

# load data ---------------------------------------------------------------

pa_nj_2018 <- read_excel(here::here(paste0(v, "_data"),
                                      "2018_PA_NJ.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(year, nestbox_id, number_young_banded, 
              number_unbanded_young_to_12_day_banding_age,
              state, banding_date, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1783)
nj_2019 <- read_excel(here::here(paste0(v, "_data"),
                                    "2019_NJ.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(year, date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1892)
pa_2019 <- read_excel(here::here(paste0(v, "_data"),
                                 "2019_PA.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(year, date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, status, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1892) %>%
  dplyr::select(-c("x14","x15")) %>%
  dplyr::mutate(state = "PA")
nj_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_NJ.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(nestbox_id = x2020_nestbox_id,
                band_number = band_number_1893) %>%
  tidyr::fill(date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, status, .direction = "down") %>%
  dplyr::mutate(year = 2020)
pa_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_PA.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(band_number = band_number_1893,
                date_1st_observed = date_1st_observed_as_active) %>%
  dplyr::select(-c("x14","x15")) %>%
  tidyr::fill(date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, status, .direction = "down") %>%
  dplyr::mutate(year = 2020,
                state = "PA")

# combine data ------------------------------------------------------------
# remove unknown sex

big_df <- dplyr::bind_rows(
  pa_nj_2018, nj_2019, pa_2019, nj_2020, pa_2020
) %>%
  dplyr::filter(sex != "u")

# plot weight vs age by sex -------------------------------------------------------

# individual chicks (better than box means I think)
big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = sex)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  theme_bw() +
  scale_color_manual(values = c("chocolate1", "darkblue")) +
  geom_hline(yintercept = 111, linetype='dashed', color = "darkblue") +
  geom_hline(yintercept = 120, linetype='dashed', color = "chocolate1") +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = "Kestrel chicks in PA and NJ, 2018-2020") +
  theme(plot.title = element_text(hjust = 0.5))
save_ggplot("wva_by_gender.png", rfile=rfile, v=v)
  
# box means
big_df %>%
  dplyr::group_by(nestbox_id, year, sex) %>%
  dplyr::summarise(mean_weight = mean(weight_in_grams),
                   mean_age = mean(age_in_days)) %>%
  ggplot(aes(x = mean_age,
             y = mean_weight,
             col = sex)) +
  geom_point() +
  geom_smooth() +
  theme_bw()


# plot weight vs age by state and sex ---------------------------------------------

# individual chicks (better than box means I think)
big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = state)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  facet_wrap(~sex)

# box means
big_df %>%
  dplyr::group_by(nestbox_id, year, sex, state) %>%
  dplyr::summarise(mean_weight = mean(weight_in_grams),
                   mean_age = mean(age_in_days)) %>%
  ggplot(aes(x = mean_age,
             y = mean_weight,
             col = state)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex) +
  theme_bw()


# plot age vs weight by sex and year --------------------------------------

# individual chicks (better than box means I think)
big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = as.factor(year))) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  facet_wrap(~sex)

# box means
big_df %>%
  dplyr::group_by(nestbox_id, sex, state, year) %>%
  dplyr::summarise(mean_weight = mean(weight_in_grams),
                   mean_age = mean(age_in_days)) %>%
  ggplot(aes(x = mean_age,
             y = mean_weight,
             col = as.factor(year))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sex) +
  theme_bw()








