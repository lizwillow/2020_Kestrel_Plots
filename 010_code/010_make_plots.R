# make plots of:
# Chicks per year
# Average of chicks per nested box per year
# For each state

# Setup ------------------------------------------------------------------------

v = "010"
rfile = "make_plots"

set.seed(100)

# The data
library(here)
library(readxl)
library(readr)
library(tidyverse)
ny_nestboxes <- read_excel(here::here(paste0(v, "_data"),
                                      "NY Manske kestrel nest box history w-percentages.xlsx"))
nj_nestboxes <- read_csv(here::here(paste0(v, "_data"),
                                    "2020_NJ.csv"))  %>%
  head(-3) %>%
  bind_rows(read_excel("010_data/2020_NJ_smallwood.xlsx") %>%
                     dplyr::rename(Year = YEAR,
                                   chicks_banded = `BANDED CHICKS`,
                                   chicks_per_box = `AVG BANDED CHICKS/ATTEMPT`) %>%
              dplyr::mutate(Org = "Smallwood"))

pa_nestboxes <- read_excel(here::here(paste0(v, "_data"),
                                      "2020_PA.xlsx"))
pa_mckelvie <- read_excel(here::here(paste0(v, "_data"),"2020_PA_mckelvie.xlsx"))

ct_nestboxes <- read_excel(here::here(paste0(v, "_data"),
                                      "2020_CT.xlsx"))

me_nestboxes <- read_csv(here::here(paste0(v, "_data"),
                                    "2020_ME.csv"), 
                     col_types = cols(X5 = col_skip(), X6 = col_skip())) %>%
                       tidyr::drop_na()

# libraries
library(viridis)
scales::show_col(viridis_pal()(20)) #look at colors
viridis_pal()(20)
library(gridExtra)
library(grid)

# files to source
source(here::here(paste0(v, "_code"),paste0("general_functions.R"))) 
source(here::here(paste0(v, "_code"),paste0(v, "_functions.R"))) 

new_rfiles(rfile, v)

# height and width of plots
w = 9
h = 6
h_cum = 10

# *************************************************************************
# *** New York ------------------------------------------------------------
# *************************************************************************

# clean data --------------------------------------------------------------

ny_nestboxes_clean <- ny_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year),
                org = "Manske") %>%
  dplyr::rename(chicks_per_box = chicks_per_nested_box_failures_entered_as_zeros, 
                chicks_banded = chicks) %>%
  dplyr::slice(1:(n()-2)) #remove totals row and empty bottom row

# chicks per year -----------------------------------------------------

# combined
ny_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "New York", combined = TRUE)
save_ggplot("ny_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")

# by organization
ny_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "New York", combined = FALSE)
save_ggplot("ny_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# cumulative plot using geom_area
ny_nestboxes_clean %>%
  expand(year, org) %>%
  left_join(ny_nestboxes_clean) %>%
  replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "New York")
save_ggplot("ny_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, height = h_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
ny_nestboxes_clean %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "New York")
save_ggplot("ny_chicks_per_nested_box.png", rfile, v, width = 7, height = h, units = "in")

# by gender
ny_nestboxes_clean %>%
  pivot_longer(c("males",
                 "females",
                 "unknown")) %>%
  ggplot() +
  theme_minimal() +
  geom_area(aes(x=year, y=value, fill=name),
            alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Number of chicks per year") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               minor_breaks = NULL) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.title = element_blank(),
        legend.title = element_blank()) + 
  scale_y_continuous(breaks = c(0,100,200,300),
                     labels = c("0","100","200","300"),
                     limits = c(0, NA))
save_ggplot("ny_number_of_chicks_by_sex.png", rfile, v)


# gender proportions ------------------------------------------------------

# by gender
ny_nestboxes_clean %>%
  pivot_longer(c("percent_males",
                 "percent_females",
                 "percent_unk")) %>%
  ggplot() +
  theme_minimal() +
  geom_area(aes(x=year, y=value, fill=name),
            alpha=0.6 , size=.5, colour="white") +
  scale_fill_viridis(discrete = T,
                     labels = c("females", "males", "unknown")) +
  ggtitle("Percentage of chicks each year") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               minor_breaks = NULL) +
  theme(axis.text.x=element_text(angle=60, hjust=1),
        axis.title = element_blank(),
        legend.title = element_blank()) + 
  scale_y_continuous(breaks = c(0,50,100),
                     labels = c("0%","50%","100%"),
                     limits = c(0, NA))
save_ggplot("ny_percentage_of_chicks_by_sex.png", rfile, v)


# Average of chicks per nested box per year -------------------------------


ny_nestboxes_clean %>%
  ggplot() +
  geom_line(aes(x = year, y = chicks_per_box), 
            color="#453781FF", alpha = 0.5,
            size = 1) +
  geom_point(aes(x = year, y = chicks_per_box), 
             color="#453781FF", size = 3) +
  geom_text(aes(x = year, y = chicks_per_box,
                label=chicks_per_box),
            hjust=.5, vjust=2, size = 3) +
  theme_minimal() + 
  theme(axis.title = element_blank()) +
  ggtitle("Average number of chicks per nested box") +
  labs(subtitle = "(Failures are entered as zeroes)",
       caption = "Data provided by Mike Manske in northern NY") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               minor_breaks = NULL) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  scale_y_continuous(limits = c(0, NA))
save_ggplot("ny_chicks_per_nested_box.png", rfile, v)


# ***************************************************************************
# *** New Jersey ------------------------------------------------------------
# ***************************************************************************

# clean data --------------------------------------------------------------

nj_nestboxes_clean <- nj_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year)) %>%
  dplyr::select(org, year, chicks_per_box, chicks_banded)

# chicks per year -----------------------------------------------------

# combined
nj_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "New Jersey", combined = TRUE)
save_ggplot("nj_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")


# by organization
nj_nestboxes_clean$org = sub("Friends of Hopewell", "Friends of Hopewell \n", 
                             nj_nestboxes_clean$org)
nj_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "New Jersey", combined = FALSE)
save_ggplot("nj_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# cumulative plot using geom_area
nj_nestboxes_clean %>%
  expand(year, org) %>%
  left_join(nj_nestboxes_clean) %>%
  replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "New Jersey")
save_ggplot("nj_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, height = h_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
nj_nestboxes_clean %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "New Jersey")
save_ggplot("nj_chicks_per_nested_box.png", rfile, v, width = 7, height = h, units = "in")



# ***************************************************************************
# *** Pennsylvania ------------------------------------------------------------
# ***************************************************************************

# clean data --------------------------------------------------------------

pa_mckelvie_clean <- pa_mckelvie %>%
  janitor::clean_names() %>%
  tidyr::drop_na() %>% 
  dplyr::mutate(year = zoo::as.Date.yearmon(year),
                org = "McKelvie") %>%
  dplyr::rename(chicks_per_box = number_chicks_per_box_with_failures_counting_as_zero, 
                chicks_banded = total_number_chicks) %>%
  dplyr::select(org,year,chicks_banded,chicks_per_box)

pa_nestboxes_clean <- pa_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year),
                number_chicks = parse_number(number_chicks)) %>%
  dplyr::rename(org = x1,
                chicks_per_box = chicks_nested_box, 
                chicks_banded = number_chicks) %>%
  dplyr::bind_rows(pa_mckelvie_clean) %>%
  dplyr::mutate(org = replace(
    org,
    org == "Devich Farbotnik in Bucks County",
    "Farbotnik"
  )) %>%
  dplyr::mutate(org = replace(
    org,
    org == "Jere Schade and Steve Benningfield in Bucks County",
    "Schade and Benningfield"
  ))

pa_nestboxes_clean$org <- sub("PA ", "", pa_nestboxes_clean$org)
  

# chicks per year -----------------------------------------------------

# combined
pa_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "Pennsylvania", combined = TRUE)
save_ggplot("pa_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")

# by organization
pa_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "Pennsylvania", combined = FALSE) +
  guides(col = guide_legend(nrow = 3))
save_ggplot("pa_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# cumulative plot using geom_area
pa_nestboxes_clean %>%
  expand(year, org) %>%
  left_join(pa_nestboxes_clean) %>%
  replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "Pennsylvania") +
  guides(fill = guide_legend(nrow = 3))
save_ggplot("pa_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, 
            height = h_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
pa_nestboxes_clean %>%
  na.omit() %>%
  dplyr::mutate(org = replace(
    org,
    org == "Devich Farbotnik in Bucks County, PA",
    "Farbotnik"
  )) %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Pennsylvania") +
  guides(col = guide_legend(nrow = 3))
save_ggplot("pa_chicks_per_nested_box.png", rfile, v, width = 7, height = h, units = "in")


# McKelvie separately -----------------------------------------------------

# chicks per box
wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {   
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
}
pa_nestboxes_clean %>%
  dplyr::filter(org == "McKelvie") %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "McKelvie") +
  labs(caption = wrapper("Note: Prior to 2018 we were not putting enough chips on the bottom of the boxes, or were putting chips in too early in the year. Sometimes this resulted in starlings removing much of the chip layer before kestrels took up residence, and the kestrel eggs were laid on a very thin chip layer, or even on bare wood. Beginning in 2018 we added a 3-4 inch layer of fresh chips to each box immediately prior to nesting season. This greatly reduced the number of box failures, as you can see from our data.",
                         dev_scaler = 13)) +
  theme(legend.position = "none") +
  ggtitle("McKelvie kestrel nest box program")
save_ggplot("pa_mckelvie_chicks_per_nested_box.png", rfile, v, width = 7, height = h, units = "in")

# total chicks from McKelvie
pa_nestboxes_clean %>%
  dplyr::filter(org == "McKelvie") %>%
  kestrel_plot_chicks_per_year(region = "McKelvie") +
  theme(legend.position = "none") +
  ggtitle("McKelvie kestrel nest box program")
save_ggplot("pa_mckelvie_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")

# Farbotnik separately -----------------------------------------------------


# chicks per box
pa_nestboxes_clean %>%
  dplyr::filter(org == "Farbotnik") %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Devich Farbotnik in Bucks County, PA",
                              text_repel_size = 4) +
  theme(legend.position = "none") +
  ggtitle("Devich Farbotnik in Bucks County, PA")
save_ggplot("pa_farbotnik_chicks_per_nested_box.png", rfile, v, width = w, height = h, units = "in")

# total chicks
pa_nestboxes_clean %>%
  dplyr::filter(org == "Farbotnik") %>%
  kestrel_plot_chicks_per_year(region = "Devich Farbotnik in Bucks County, PA") +
  theme(legend.position = "none") +
  ggtitle("Devich Farbotnik in Bucks County, PA")
save_ggplot("pa_farbotnik_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")

# Schade and Benningfield separately -----------------------------------------------------


# chicks per box
pa_nestboxes_clean %>%
  dplyr::filter(org == "Schade and Benningfield") %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Jere Schade and Steve Benningfield in Bucks County, PA") +
  theme(legend.position = "none") +
  ggtitle("Jere Schade and Steve Benningfield in Bucks County, PA")
save_ggplot("pa_schade_benningfield_chicks_per_nested_box.png", rfile, v, width = w, height = h, units = "in")

# total chicks
pa_nestboxes_clean %>%
  dplyr::filter(org == "Schade and Benningfield") %>%
  kestrel_plot_chicks_per_year(region = "Jere Schade and Steve Benningfield in Bucks County, PA") +
  theme(legend.position = "none") +
  ggtitle("Jere Schade and Steve Benningfield in Bucks County, PA")
save_ggplot("pa_schade_benningfield_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")


# ***************************************************************************
# *** Connecticut ------------------------------------------------------------
# ***************************************************************************

# clean data --------------------------------------------------------------

ct_nestboxes_clean <- ct_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::rename(year = yr,
                chicks_per_box = chicks_per_nested_box, 
                chicks_banded = total_number_of_chicks) %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year),
                chicks_per_box = parse_number(chicks_per_box),
                chicks_banded = parse_number(chicks_banded),
                org = "Sayers")


# chicks per year -----------------------------------------------------

# combined
ct_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "Connecticut", combined = TRUE)
save_ggplot("ct_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")

# by organization
ct_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "Connecticut", combined = FALSE)
save_ggplot("ct_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# cumulative plot using geom_area
ct_nestboxes_clean %>%
  expand(year, org) %>%
  left_join(ct_nestboxes_clean) %>%
  replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "Connecticut")
save_ggplot("ct_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, 
            height = h_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
ct_nestboxes_clean %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Connecticut")
save_ggplot("ct_chicks_per_nested_box.png", rfile, v, width = 7, height = h, units = "in")


# ***************************************************************************
# *** Maine ------------------------------------------------------------
# ***************************************************************************

# clean data --------------------------------------------------------------

me_nestboxes_clean <- me_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::rename(chicks_banded = nestlings_to_banding_age) %>%
  dplyr::mutate(year = zoo::as.Date.yearmon(year))


# chicks per year -----------------------------------------------------

# combined
p1 <- me_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "Maine", combined = TRUE) +
  ggtitle("St. Albans Maine kestrel nest box project")
p2 <- grid.arrange(p1, top = textGrob("Data provided by Marek Plater",
             x = .99,
             just = "right", # left-aligned,
             gp = gpar(fontsize = 8)) # bigger font
)
save_ggplot("me_number_of_chicks.png", rfile, v, width = w, height = h, units = "in",
            plot = p2)

# by organization
me_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "Maine", combined = FALSE)
save_ggplot("me_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# cumulative plot using geom_area
me_nestboxes_clean %>%
  expand(year, org) %>%
  left_join(me_nestboxes_clean) %>%
  replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "Maine")
save_ggplot("me_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, 
            height = h_cum, units = "in")








