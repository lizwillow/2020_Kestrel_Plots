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


nj_2018 <- read_excel(here::here(paste0(v, "_data"),
                                 "2018_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  # calculate indiv fledge dates
  dplyr::mutate(year = 2018,
                state = "NJ",
                fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
pa_2018 <- read_excel(here::here(paste0(v, "_data"),
                                 "2018_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2018,
                state = "PA") %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
nj_2019 <- read_excel(here::here(paste0(v, "_data"),
                                 "2019_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2019,
                state = "NJ") %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
pa_2019 <- read_excel(here::here(paste0(v, "_data"),
                                 "2019_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2019,
                state = "PA") %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
nj_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(nestbox_id = x2020_nestbox_id) %>%
  tidyr::fill(nestbox_id, 
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2020,
                state = "NJ") %>% 
  dplyr::mutate(fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
pa_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(nestbox_id, 
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2020,
                state = "PA",
                fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
pa_2021 <- read_excel(here::here(paste0(v, "_data"),
                                 "2021_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  dplyr::rename(date_1st_observed = date_1st_observed_as_active) %>%
  tidyr::fill(nestbox_id, 
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(state = "PA",
                fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
                
nj_2021 <- read_excel(here::here(paste0(v, "_data"),
                                 "2021_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  tidyr::fill(nestbox_id,
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(state = "NJ",
                fledge_date = (banding_date + lubridate::days(30 - age_in_days)) %>% 
                  lubridate::round_date(unit = "days")) %>% 
  # get mean fledge dates
  dplyr::group_by(nestbox_id, year, state) %>%
  dplyr::summarise(ave_fledge_date = mean(fledge_date) %>%
                     lubridate::round_date(unit = "days")) %>%
  dplyr::ungroup() %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
                
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
                ave_fledge_date = fledge_date %>% 
                  lubridate::round_date(unit = "days")) %>% 
  drop_na(ave_fledge_date) %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")
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
                ave_fledge_date = fledge_date %>% 
                  lubridate::round_date(unit = "days")) %>% 
  drop_na(ave_fledge_date) %>%
  # count up mean fledge dates
  dplyr::count(state, year, ave_fledge_date, name = "count")

# # weights only for pa&nj
# pa_nj_2022 <- read_excel(here::here(paste0(v,"_data/2022_NJ_PA_chicks.xlsx"))) %>% 
#   pivot_longer(-AGE) %>% 
#   dplyr::rename(age = AGE,
#                 weight_in_grams = value) %>% 
#   dplyr::select(-name) %>% 
#   dplyr::mutate(sex = str_sub(age, -1),
#                 age_in_days = parse_number(age),
#                 year = 2022) %>% 
#   drop_na()

# combine data ------------------------------------------------------------
# remove unknown sex

big_df_dates <- dplyr::bind_rows(
  pa_2018, nj_2018, nj_2019, pa_2019, nj_2020, pa_2020, pa_2021, nj_2021, pa_2022_nowt, nj_2022_nowt
)
# create data product
readr::write_csv(big_df_dates %>% 
                   drop_na(ave_fledge_date), 
                 here::here(paste0(v, "_data"), rfile, "panj_upto2022_dates.csv"))

# make them all same year for plotting
lubridate::year(big_df_dates$ave_fledge_date) = 2000

big_df_counts <- big_df_dates %>% 
  drop_na(ave_fledge_date) %>% 
  dplyr::group_by(year, state) %>% 
  complete(ave_fledge_date = seq(min(ave_fledge_date), 
                                 max(ave_fledge_date), by="day"),
           fill = list(count = 0)) %>% 
  # get moving average
  dplyr::mutate(rolling_ave = zoo::rollmean(count, k = 7, fill = NA))

p1 <- big_df_counts %>% 
  ggplot(aes(x = ave_fledge_date, y = rolling_ave)) +
  geom_line(aes(color = as.factor(year))) +
  facet_wrap(~ state) +
  labs(y = "Nestboxes per day (7-day rolling average)",
       title = "Estimated mean 30-day-old fledge dates") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank()) +
  scale_color_manual(values = c('#AD2323', '#3cb44b', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#9A6324', '#a9a9a9', '#000000')
) 
p1
save_ggplot("ave_fledge_dates.png", rfile=rfile, v=v,
            width = 7, height = 4)  

# Add medians --------------------------------------------

big_df_wmed <- as.data.frame(lapply(big_df_counts, 
                                    rep, 
                                    big_df_counts$count)) %>% 
  dplyr::group_by(state, year) %>% 
  dplyr::summarise(median_date = median(ave_fledge_date))

# add annotations
dat_text <- data.frame(
  label = c(
  "Median fledge dates:
   2018: 7/5
   2019: 6/22
   2020: 6/18
   2021: 6/23
   2022: 6/25", 
  "Median fledge dates:
   2018: 7/1
   2019: 6/22
   2020: 6/16
   2021: 6/19
   2022: 6/19"),
  state   = c("NJ", "PA"))
p1 + geom_text(
  data    = dat_text,
  mapping = aes(x = max(big_df_counts$ave_fledge_date)-lubridate::days(60), 
                y = 4, label = label),
  hjust = 0,
  size = 3
)
save_ggplot("ave_fledge_dates_w_medians.png", rfile=rfile, v=v,
            width = 7, height = 4) 

# p2 <- big_df_wmed %>% 
#   ggplot(aes(x = median_date, y = 0)) +
#   geom_hline(aes(yintercept = 0), color = "grey") +
#   geom_point(aes(color = as.factor(year)),
#              ) +
#   facet_wrap(~ state) +
#   theme(legend.title = element_blank(),
#         axis.title = element_blank()) +
#   scale_color_manual(values = c('#AD2323', '#3cb44b', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#9A6324', '#a9a9a9', '#000000')
#   ) +
#   theme_void() +
#   theme(legend.position = "none",
#         strip.background = element_blank(),
#         strip.text.x = element_blank()) +
#   scale_x_continuous(limits = c(min(big_df_counts$ave_fledge_date), 
#                                 max(big_df_counts$ave_fledge_date)))
# p2
#   
# library(patchwork)
# p1/p2   + 
#   plot_layout(heights = c(10, 1))
