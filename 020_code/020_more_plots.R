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
  dplyr::filter(sex != "u") %>%
  dplyr::mutate(yday_born = lubridate::yday(banding_date) - age_in_days)


# plot weight vs age by sex -------------------------------------------------------

# individual chicks (better than box means I think)
big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = sex)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  scale_color_manual(values = c("chocolate1", "darkblue")) +
  geom_hline(yintercept = 111, linetype='dashed', color = "darkblue") +
  geom_hline(yintercept = 120, linetype='dashed', color = "chocolate1") +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = "Kestrel chicks in PA and NJ, 2018-2020")
save_ggplot("wva_by_gender.png", rfile=rfile, v=v)
  
# box means
# big_df %>%
#   dplyr::group_by(nestbox_id, year, sex) %>%
#   dplyr::summarise(mean_weight = mean(weight_in_grams),
#                    mean_age = mean(age_in_days)) %>%
#   ggplot(aes(x = mean_age,
#              y = mean_weight,
#              col = sex)) +
#   geom_point() +
#   geom_smooth() +
#   theme_bw()


# plot weight vs age by state and sex ---------------------------------------------

# individual chicks (better than box means I think)
big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = state)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~sex) +
  scale_color_manual(values = c("cornflowerblue", "brown1")) +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = "Kestrel chicks in PA and NJ, 2018-2020")
save_ggplot("wva_by_state_and_sex.png", rfile=rfile, v=v)

# box means
# big_df %>%
#   dplyr::group_by(nestbox_id, year, sex, state) %>%
#   dplyr::summarise(mean_weight = mean(weight_in_grams),
#                    mean_age = mean(age_in_days)) %>%
#   ggplot(aes(x = mean_age,
#              y = mean_weight,
#              col = state)) +
#   geom_point() +
#   geom_smooth() +
#   facet_wrap(~sex) +
#   theme_bw()


# plot age vs weight by sex and year --------------------------------------

# individual chicks (better than box means I think)
big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = as.factor(year))) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  facet_wrap(~sex) +
  scale_color_manual(values = c("cornflowerblue", "brown1", "gold")) +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = "Kestrel chicks in PA and NJ, 2018-2020") + 
  guides(col = guide_legend(title=element_blank()))
save_ggplot("wva_by_year_and_sex.png", v=v, rfile=rfile)

# box means
# big_df %>%
#   dplyr::group_by(nestbox_id, sex, state, year) %>%
#   dplyr::summarise(mean_weight = mean(weight_in_grams),
#                    mean_age = mean(age_in_days)) %>%
#   ggplot(aes(x = mean_age,
#              y = mean_weight,
#              col = as.factor(year))) +
#   geom_point() +
#   geom_smooth() +
#   facet_wrap(~sex) +
#   theme_bw()


# plot both year and sex and state --------------------------------------------------

# individual chicks (better than box means I think)
big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = state)) +
  geom_point(alpha = 0.4) +
  geom_smooth() +
  facet_wrap(sex~year, labeller = label_wrap_gen(multi_line=FALSE)) +
  scale_color_manual(values = c("cornflowerblue", "brown1")) +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = "Kestrel chicks in PA and NJ, 2018-2020") + 
  guides(col = guide_legend(title=element_blank()))
save_ggplot("wva_by_year_sex_state.png", v=v, rfile=rfile)


# Plot bday vs weight -----------------------------------------------------

# plot of yday_born vs weight
big_df %>%
  ggplot(aes(x = yday_born, y = weight_in_grams, col = age_in_days)) +
  geom_point() +
  geom_smooth(col = "black") +
  labs(x = "Birthday (day banded minus age in days)",
       y = "Weight when banded (grams)",
       title = "Kestrel chicks in PA and NJ, 2018-2020") +
  facet_wrap(~sex) +
  guides(col = guide_legend(title = "Age(days)")) +
  scale_color_continuous(low = "red", high = "blue")
  
save_ggplot("wv_bday_by_sex.png", v=v, rfile=rfile)

# Linear regression --------------------------------------------------------------

big_df_lm <- big_df %>%
  dplyr::mutate(year = as.factor(year))

fit <- lm(weight_in_grams ~ age_in_days + sqrt(age_in_days) +
            year + sex + number_young_banded + yday_born, data = big_df_lm)

summary(fit)
get_regression_table(fit)
get_regression_summaries(fit)

plot(fit)

# conditional plots
library(visreg)
visreg(fit, "age_in_days", gg = TRUE) 
visreg(fit, "year", gg = TRUE) 
visreg(fit, "sex", gg = TRUE) 
visreg(fit, "yday_born", gg = TRUE) 

# number_young_banded was not significant so remove it

fit2 <- lm(weight_in_grams ~ age_in_days + sqrt(age_in_days) +
            year + sex + yday_born, data = big_df_lm)

summary(fit2)
get_regression_table(fit2)
get_regression_summaries(fit2)

par(mfrow = c(2,2))
plot(fit2)

# conditional plots
library(visreg)
visreg(fit2, "age_in_days", gg = TRUE) 
visreg(fit2, "year", gg = TRUE) 
visreg(fit2, "sex", gg = TRUE) 
visreg(fit2, "yday_born", gg = TRUE) 

# add random effect for nestbox ---------------------------------------------

fit3 <- lmer(weight_in_grams ~ age_in_days + sqrt(age_in_days) +
            year + sex + yday_born + (1 | nestbox_id), data = big_df_lm)

summary(fit3)
plot(fit3)
confint(fit3)

# estimated of random effects
predictInterval(fit3) %>% head()   # for various model predictions, possibly with new data

REsim(fit3) %>% head()            # mean, median and sd of the random effect estimates

plotREsim(REsim(fit3))  # plot the interval estimates

# conditional plots
visreg(fit3, "age_in_days", gg = TRUE) 
visreg(fit3, "year", gg = TRUE) 
visreg(fit3, "sex", gg = TRUE) 
visreg(fit3, "yday_born", gg = TRUE) 





