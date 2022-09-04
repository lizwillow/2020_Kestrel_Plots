# make plots of:
# fledge dates for
# - PA/NJ
# - 2018/2019/2020/2021/2022

# Setup ------------------------------------------------------------------------

v = "020"
rfile = "plot_fledge_dates"

set.seed(100)

# packages
library(here)
library(readxl)
library(readr)
library(tidyverse)
library(moderndive)
library(lme4)
library(merTools)

# files to source
source(here::here(paste0(v, "_code"),paste0("general_functions.R"))) 
source(here::here(paste0(v, "_code"),paste0(v, "_functions.R"))) 

new_rfiles(rfile, v)

# set ggplot theme
theme_liz <- theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
theme_set(theme_liz)

# load data ---------------------------------------------------------------

## NOTE: need to watch out when using tidyr::fill.
## number_young_banded, number_unbanded_young_to_12_day_banding_age, and 
## banding_date might be filling in the next nest incorrectly!!
## Still need to fix that.

pa_nj_2018 <- read_excel(here::here(paste0(v, "_data"),
                                    "2018_PA_NJ_boxes.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(year, nestbox_id, number_young_banded, 
              number_unbanded_young_to_12_day_banding_age,
              state, banding_date, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1783) %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  dplyr::count(state, year, fledge_date, name = "count")
nj_2019 <- read_excel(here::here(paste0(v, "_data"),
                                 "2019_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(year, date_1st_observed, nestbox_id, number_young_banded, 
              banding_date, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1892) %>%
  dplyr::mutate(state = "NJ") %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  dplyr::count(state, year, fledge_date, name = "count")
pa_2019 <- read_excel(here::here(paste0(v, "_data"),
                                 "2019_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(year, date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, status, .direction = "down") %>%
  dplyr::rename(band_number = band_number_1892) %>%
  dplyr::select(-c("x14","x15")) %>%
  dplyr::mutate(state = "PA") %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  dplyr::count(state, year, fledge_date, name = "count")
nj_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(nestbox_id = x2020_nestbox_id,
                band_number = band_number_1893) %>%
  tidyr::fill(date_1st_observed, nestbox_id, number_young_banded, 
              state, banding_date, status, .direction = "down") %>%
  dplyr::mutate(year = 2020) %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  dplyr::count(state, year, fledge_date, name = "count")
pa_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(band_number = band_number_1893,
                date_1st_observed = date_1st_observed_as_active) %>%
  dplyr::select(-c("x15")) %>%
  tidyr::fill(nestbox_id, number_young_banded, 
              banding_date, status, .direction = "down") %>%
  dplyr::mutate(state = "PA",
                band_number = as.numeric(band_number),
                fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  dplyr::count(state, year, fledge_date, name = "count")
pa_2021 <- read_excel(here::here(paste0(v, "_data"),
                                 "2021_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(band_number = band_number_1893,
                date_1st_observed = date_1st_observed_as_active,
                banding_date = banding_date_15) %>%
  tidyr::fill(nestbox_id, 
              status,
              banding_date, .direction = "down") %>%
  dplyr::mutate(state = "PA",
                band_number = as.numeric(band_number),
                fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  dplyr::count(state, year, fledge_date, name = "count")
                
nj_2021 <- read_excel(here::here(paste0(v, "_data"),
                                 "2021_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weightin_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(weight_in_grams = weightin_grams) %>%
  tidyr::fill(nestbox_id, number_young_banded,
              status, banding_date, .direction = "down") %>%
  dplyr::mutate(state = "NJ",
                band_number = as.numeric(band_number),
                fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  dplyr::count(state, year, fledge_date, name = "count")
                

# no weights, just numbers per box for pa&nj 2022
pa_2022_nowt <- read_excel(here::here(paste0(v, "_data"),
                                      "2022_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(number_young_banded_s_mount=number_young_banded_steel_pole_mount,
                number_young_banded_u_mount=young_banded_utility_pole_mount,
                date_1st_observed = date_1st_observed_as_active) %>%
  replace_na(list(number_young_banded_s_mount=0,
                  number_young_banded_u_mount=0,
                  number_unbanded_young_to_12_day_banding_age=0)) %>% 
  dplyr::mutate(state = "PA",
                number_young_banded = number_young_banded_s_mount +
                  number_young_banded_u_mount + 
                  number_unbanded_young_to_12_day_banding_age,
                year = 2022,
                fledge_date = fledge_date %>% 
                  lubridate::round_date(unit = "days")) %>% 
  drop_na(fledge_date) %>% 
  dplyr::group_by(year, state, fledge_date) %>% 
  dplyr::summarise(count = sum(number_young_banded)) %>% 
  dplyr::ungroup()
nj_2022_nowt <- read_excel(here::here(paste0(v, "_data"),
                                      "2022_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  replace_na(list(number_young_banded_s_mount=0,
                  number_young_banded_u_mount=0,
                  number_unbanded_young_to_12_day_banding_age_u=0)) %>% 
  dplyr::mutate(state = "NJ",
                number_young_banded = number_young_banded_s_mount +
                  number_young_banded_u_mount + 
                  number_unbanded_young_to_12_day_banding_age_u,
                fledge_date = fledge_date %>% 
                  lubridate::round_date(unit = "days")) %>% 
  drop_na(fledge_date) %>% 
  dplyr::group_by(year, state, fledge_date) %>% 
  dplyr::summarise(count = sum(number_young_banded)) %>% 
  dplyr::ungroup()

# weights only for pa&nj
pa_nj_2022 <- read_excel(here::here(paste0(v,"_data/2022_NJ_PA_chicks.xlsx"))) %>% 
  pivot_longer(-AGE) %>% 
  dplyr::rename(age = AGE,
                weight_in_grams = value) %>% 
  dplyr::select(-name) %>% 
  dplyr::mutate(sex = str_sub(age, -1),
                age_in_days = parse_number(age),
                year = 2022) %>% 
  drop_na()

# combine data ------------------------------------------------------------
# remove unknown sex

big_df_dates <- dplyr::bind_rows(
  pa_nj_2018, nj_2019, pa_2019, nj_2020, pa_2020, pa_2021, nj_2021, pa_2022_nowt, nj_2022_nowt
)

# all same year
lubridate::year(big_df_dates$fledge_date) = 2000

big_df_counts <- big_df_dates %>% 
  dplyr::group_by(year, state) %>% 
  complete(fledge_date = seq(min(fledge_date), 
                                 max(fledge_date), by="day"),
           fill = list(count = 0)) %>% 
  # get moving average
  dplyr::mutate(rolling_ave = zoo::rollmean(count, k = 7, fill = NA))

big_df_counts %>% 
  ggplot(aes(x = fledge_date, y = rolling_ave)) +
  geom_line(aes(color = as.factor(year))) +
  facet_wrap(~ state) +
  labs(y = "7 day rolling average of fledgelings per day",
       title = "Estimated 30-day-old fledge dates") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank()) +
  scale_color_manual(values = c('#AD2323', '#3cb44b', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#9A6324', '#a9a9a9', '#000000')
)
save_ggplot("fledge_dates.png", rfile=rfile, v=v,
            width = 7, height = 4)  
  
  
  
  
