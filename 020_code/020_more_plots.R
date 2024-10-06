# make plots of:
# Weight vs age
# Possibly broken up by:  
# - M/F
# - PA/NJ
# - 2018/2019/2020/2021/2022

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

## NOTE: need to watch out when using tidyr::fill.


nj_2018 <- read_excel(here::here(paste0(v, "_data"),
                                 "2018_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2018,
                state = "NJ") %>% 
  dplyr::select(year, nestbox_id, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )
pa_2018 <- read_excel(here::here(paste0(v, "_data"),
                                 "2018_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2018,
                state = "PA") %>% 
  dplyr::select(year, nestbox_id, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )
nj_2019 <- read_excel(here::here(paste0(v, "_data"),
                                    "2019_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2019,
                state = "NJ") %>% 
  dplyr::select(year, nestbox_id, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )
pa_2019 <- read_excel(here::here(paste0(v, "_data"),
                                 "2019_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(nestbox_id, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2019,
                state = "PA") %>% 
  dplyr::select(year, nestbox_id, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )
nj_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(nestbox_id = x2020_nestbox_id) %>%
  tidyr::fill(nestbox_id, 
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2020,
                state = "NJ") %>% 
  dplyr::select(year, nestbox_id, status, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )
pa_2020 <- read_excel(here::here(paste0(v, "_data"),
                                 "2020_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  tidyr::fill(nestbox_id, 
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(year = 2020,
                state = "PA") %>% 
  dplyr::select(year, nestbox_id, status, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )
pa_2021 <- read_excel(here::here(paste0(v, "_data"),
                                 "2021_PA_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weight_in_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(date_1st_observed = date_1st_observed_as_active) %>%
  tidyr::fill(nestbox_id, 
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(year, nestbox_id, status, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )
nj_2021 <- read_excel(here::here(paste0(v, "_data"),
                                 "2021_NJ_chicks.xlsx")) %>%
  janitor::clean_names() %>%
  drop_na(weightin_grams) %>% # remove those whose weight was not recorded
  dplyr::rename(weight_in_grams = weightin_grams) %>%
  tidyr::fill(nestbox_id,
              status, .direction = "down") %>%
  # fill banding dates within nestbox IDs
  dplyr::group_by(nestbox_id) %>% 
  tidyr::fill(banding_date) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(state = "NJ") %>% 
  dplyr::select(year, nestbox_id, status, 
                state, sex, weight_in_grams, age_in_days,
                banding_date
  )

# # no weights, just numbers per box for pa&nj 2022
# pa_2022_nowt <- read_excel(here::here(paste0(v, "_data"),
#                                  "2022_PA_chicks.xlsx")) %>%
#   janitor::clean_names() %>%
#   drop_na(fledge_date) %>% 
#   dplyr::mutate(year = "2022") %>% 
#   dplyr::rename(number_young_banded_s_mount=number_young_banded_steel_pole_mount,
#                 number_young_banded_u_mount=young_banded_utility_pole_mount) %>% 
#   replace_na(list(number_young_banded_s_mount=0,
#                   number_young_banded_u_mount=0,
#                   number_unbanded_young_to_12_day_banding_age=0)) %>% 
#   dplyr::mutate(state = "PA",
#                 number_young_banded = number_young_banded_s_mount +
#                   number_young_banded_u_mount + 
#                   number_unbanded_young_to_12_day_banding_age) %>%
#   dplyr::select(year, nestbox_id, status, 
#                 state, fledge_date
#   )
# nj_2022_nowt <- read_excel(here::here(paste0(v, "_data"),
#                                  "2022_NJ_chicks.xlsx")) %>%
#   janitor::clean_names() %>%
#   janitor::clean_names() %>%
#   drop_na(fledge_date) %>% 
#   dplyr::mutate(year = "2022") %>% 
#   replace_na(list(number_young_banded_s_mount=0,
#                   number_young_banded_u_mount=0,
#                   number_unbanded_young_to_12_day_banding_age_u=0)) %>% 
#   dplyr::mutate(state = "PA",
#                 number_young_banded = number_young_banded_s_mount +
#                   number_young_banded_u_mount + 
#                   number_unbanded_young_to_12_day_banding_age_u) %>%
#   dplyr::select(year, nestbox_id, status, 
#                 state, fledge_date
#   )

# weights for pa&nj
pa_nj_2022 <- read_excel(here::here(paste0(v,"_data/2022_NJ_PA_chicks.xlsx"))) %>% 
  pivot_longer(-AGE) %>% 
  dplyr::rename(age = AGE,
                weight_in_grams = value) %>% 
  dplyr::select(-name) %>% 
  dplyr::mutate(sex = str_sub(age, -1),
                age_in_days = parse_number(age),
                year = 2022) %>% 
  drop_na() %>% 
  dplyr::select(sex, year, weight_in_grams,
                age_in_days)

# combine data ------------------------------------------------------------
# remove unknown sex

bind_df <- dplyr::bind_rows(
  pa_2018, nj_2018, nj_2019, pa_2019, nj_2020, pa_2020, pa_2021, nj_2021, pa_nj_2022
)

big_df <- bind_df %>% 
  dplyr::select(sex, year, weight_in_grams,
                age_in_days) %>%
  dplyr::filter(sex %in% c("m","f")) %>%
  dplyr::mutate(sex_full = case_when(sex == "f" ~ "female",
                                     sex == "m" ~ "male"))
save_rds(big_df, v=v, rfile = rfile)

# how many unidentified sex? None in the dataset in 2022
dplyr::bind_rows(
  pa_2018, nj_2018, nj_2019, pa_2019, nj_2020, pa_2020, pa_2021, nj_2021, pa_nj_2022
) %>% 
  dplyr::select(sex, year, weight_in_grams,
                age_in_days) %>%
  dplyr::filter(!(sex %in% c("m","f"))) %>% 
  glimpse()


# plot weight vs age by sex -------------------------------------------------------

# individual chicks
p <- big_df %>%
  ggplot() +
  geom_point(aes(x = age_in_days,
                 y = weight_in_grams,
                 col = sex_full),
             alpha = 0.4) +
  geom_smooth(aes(x = age_in_days,
                  y = weight_in_grams,
                  col = sex_full),
              se = FALSE, level = 0.95,
              method = "gam", formula = y ~ s(x, bs = "cs"),
              size = 0.7) +
  scale_color_manual(values = c("chocolate1", "darkblue")) +
  geom_hline(yintercept = 108, linetype='dashed', color = "darkblue", size = 0.7) +
  geom_hline(yintercept = 116, linetype='dashed', color = "chocolate1", size = 0.7) +
  geom_text(data = data.frame(annotateText = "Dashed lines depict average male and female weight from data provided by BBL for banded adults in PA and NJ from 2018 to 2020.
                Solid lines depict trends in male and female kestrel chicks using locally weighted smoothing."),
            mapping = aes(x = 30, y = 64, label = annotateText),
            inherit.aes = FALSE,
            size = 3, hjust = 1) +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = paste0(nrow(big_df), " kestrel chicks in PA and NJ, 2018-2022"),
       color = "Sex",
       caption = "394 chicks whose sex was not recorded or could not be clearly determined were excluded.") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks=seq(14,28,2)) + 
  scale_y_continuous(breaks = sort(c(seq(60, 165, by = 20), 116, 108)))
p
save_ggplot("w_by_gender.png", rfile=rfile, v=v,
            height = 6, width = 8)

# calculate percent above adult weight at 15
big_df %>% 
  dplyr::filter(age_in_days == 15, sex == "f") %>% 
  dplyr::summarise(sum_big_116 = sum(weight_in_grams>116),
                   n=n(),
                   prop=sum_big_116/n)
big_df %>% 
  dplyr::filter(age_in_days == 15, sex == "m") %>% 
  dplyr::summarise(sum_big_116 = sum(weight_in_grams>108),
                   n=n(),
                   prop = sum_big_116/n)

# calculate percent above adult weight above 17
big_df %>% 
  dplyr::filter(age_in_days > 17, sex == "f") %>% 
  dplyr::summarise(sum_big_116 = sum(weight_in_grams>116),
                   n=n(),
                   prop=sum_big_116/n)
big_df %>% 
  dplyr::filter(age_in_days > 17, sex == "m") %>% 
  dplyr::summarise(sum_big_116 = sum(weight_in_grams>108),
                   n=n(),
                   prop = sum_big_116/n)

# marginals
ggExtra::ggMarginal(p,type="boxplot", groupFill = TRUE)
ggExtra::ggMarginal(p,type="density", groupFill = TRUE)

# individual chicks faceted by year
p <- big_df %>%
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = sex)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = TRUE, level = 0.95,
              method = "gam", formula = y ~ s(x, bs = "cs")) +
  scale_color_manual(values = c("chocolate1", "darkblue")) +
  geom_hline(yintercept = 108, linetype='dashed', color = "darkblue") +
  geom_hline(yintercept = 116, linetype='dashed', color = "chocolate1") +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = paste0(nrow(big_df), " kestrel chicks in PA and NJ, 2018-2021"),
       caption = "Dotted lines represent average weight from data provided by BBL for banded adults in PA and NJ from 2018 to 2020",
       color = "Sex") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(14,28,2)) +
  facet_wrap(~ year)
p
save_ggplot("w_by_gender_by_year.png", rfile=rfile, v=v)
  
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


# Frequentist piecewise regression ----------------------------------------

# females
library(segmented)
fit_lm = lm(weight_in_grams ~ 1 + age_in_days, 
            data = big_df %>% dplyr::filter(sex == "f"))  # intercept-only model
fit_segmented = segmented(fit_lm, seg.Z = ~age_in_days, npsi = 1)  # One change point along x
summary(fit_segmented)
confint(fit_segmented)
slope(fit_segmented)
BF = exp((BIC(fit_segmented) - BIC(fit_lm))/2)  # From Wagenmakers (2007)
BF

# males
fit_lm = lm(weight_in_grams ~ 1 + age_in_days, 
            data = big_df %>% dplyr::filter(sex == "m"))  # intercept-only model
fit_segmented = segmented(fit_lm, seg.Z = ~age_in_days, npsi = 1)  # One change point along x
summary(fit_segmented)
confint(fit_segmented)
slope(fit_segmented)
BF = exp((BIC(fit_segmented) - BIC(fit_lm))/2)  # From Wagenmakers (2007)
BF

# Bayesian Piecewise regression ----------------------------------------------------

# female changepoint model
model = list(
  weight_in_grams ~ age_in_days,  # linear segment
  ~ age_in_days  # another linear segment
)
library(mcp)
fit = mcp(model, big_df %>% dplyr::filter(sex == "f"))
plot(fit)
plot_pars(fit, regex_pars = "cp_")

# male changepoint model
model_male = list(
  weight_in_grams ~ age_in_days,  # linear segment
  ~ age_in_days  # another linear segment
)
fit_male = mcp::mcp(model_wage, big_df %>% dplyr::filter(sex == "m"))
plot(fit_male)
plot_pars(fit_male, regex_pars = "cp_")

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


# plot weight vs age by sex FOR REGRESSION LESSON -------------------------------------------------------

sub_df <- big_df %>%
  dplyr::filter(age_in_days < 24) %>% 
  dplyr::group_by(nestbox_id, banding_date) %>% 
  dplyr::sample_n(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(age_in_days, weight_in_grams, sex) %>% 
  dplyr::mutate(age_sq = age_in_days^2)

save_rds(sub_df, v, rfile)

p <- sub_df %>% 
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = sex)) +
  geom_point(alpha = 0.4) + 
  scale_color_manual(values = c("chocolate1", "darkblue")) +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = paste0(nrow(sub_df), " kestrel chicks in PA and NJ, 2018-2021"),
       color = "Sex") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(13,28,1))
p
save_ggplot("regression_example_1.png", rfile=rfile, v=v)

# add regression lines
fit <- lm(weight_in_grams~age_in_days+sex+age_sq, sub_df)
new_df <- data.frame(age_in_days = 
                   rep(seq(min(sub_df$age_in_days), 
                           max(sub_df$age_in_days),
                           length.out = 20),2),
                 sex = rep(c("m","f"), each = 20),
                 weight_in_grams = predict(fit,
              newdata = data.frame(age_in_days = 
                                     rep(seq(min(sub_df$age_in_days), 
                                         max(sub_df$age_in_days),
                                         length.out = 20),2),
                                   sex = rep(c("m","f"), each = 20)) %>% 
                dplyr::mutate(age_sq = age_in_days^2)))
p <- sub_df %>% 
  dplyr::rename(Sex = sex) %>% 
  ggplot(aes(x = age_in_days,
             y = weight_in_grams,
             col = Sex)) +
  geom_point(alpha = 0.4) + 
  geom_line(aes(x = age_in_days,
                y = weight_in_grams,
                col = sex), data = new_df) +
  #geom_smooth(method = "lm", alpha = .15, aes(fill = Sex)) +
  scale_color_manual(values = c("chocolate1", "darkblue")) +
  scale_fill_manual(values = c("chocolate1", "darkblue")) +
  labs(x= "Age (days)",
       y = "Weight (grams)",
       title = paste0(nrow(sub_df), " kestrel chicks in PA and NJ, 2018-2021")) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks=seq(13,28,1))
p
save_ggplot("regression_example_w_sq_lines.png", rfile=rfile, v=v)

plot(fit)
