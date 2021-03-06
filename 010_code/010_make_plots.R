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
                                      "2020_NY.csv"))
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

ct_nestboxes <- read_excel(here::here(paste0(v, "_data"),
                                      "2020_CT.xlsx"))

me_nestboxes <- read_csv(here::here(paste0(v, "_data"),
                                    "2020_ME.csv"), 
                     col_types = cols(X5 = col_skip(), X6 = col_skip())) %>%
                       tidyr::drop_na()

vt_nestboxes <- read_csv(here::here(paste0(v, "_data"),
                                      "2020_VT.csv"))

va_nestboxes <- read_csv(here::here(paste0(v, "_data"),
                                    "2020_VA.csv"))

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
w = 9
h = 9
h_cum = 10

# *************************************************************************
# *** New York ------------------------------------------------------------
# *************************************************************************

# height and width of plots
w_ny = 9
h_ny = 5
h_ny_cum = 5

# clean data --------------------------------------------------------------

ny_nestboxes_clean <- ny_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year)) %>%
  dplyr::rename(chicks_per_box = chicks_per_nested_box_failures_entered_as_zeros, 
                chicks_banded = chicks)

# chicks per year -----------------------------------------------------

# combined
ny_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "New York", combined = TRUE)
save_ggplot("ny_number_of_chicks.png", rfile, v, width = w_ny, height = h_ny, units = "in")

# by organization
ny_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "New York", combined = FALSE,
                               label_col = label_chicks_banded) +
  labs(caption = paste0("Data provided by Mark Manske and Zach Smith.\n", wrapper("* Smith’s boxes in southern NY around the Shawangunk Grasslands National Wildlife Refuge continue to be maintained but nesting data has not been reported for 2019 or 2020.",
                         dev_scaler = 12))) +
  theme(plot.caption = element_text(size = 10, face = "italic"))
save_ggplot("ny_number_of_chicks_by_org.png", rfile, v, width = w_ny, height = h_ny, units = "in")

# cumulative plot using geom_area
ny_nestboxes_clean %>%
  # expand(year, org) %>%
  # left_join(ny_nestboxes_clean) %>%
  # replace_na(list(chicks_banded = 0, 
  #                 chicks_per_box = 0)) %>%
  kestrel_plot_cumulative(region = "New York")
save_ggplot("ny_number_of_chicks_by_org_cumulative.png", rfile, v, 
            width = w_ny, height = h_ny_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
ny_nestboxes_clean %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "New York") +
  labs(caption = "Data provided by Mike Manske in northern NY",
       title = "New York kestrel nest box program") +
  theme(legend.position = "none")
save_ggplot("ny_chicks_per_nested_box.png", rfile, v, width = w_ny, height = h_ny, units = "in")

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
  kestrel_plot_chicks_per_year(region = "New Jersey", combined = TRUE,
                               label_col = sum_chicks_banded_per_year) +
  expand_limits(y = 100)
save_ggplot("nj_number_of_chicks.png", rfile, v) #, width = w, height = h, units = "in"


# by organization
nj_nestboxes_clean$org = sub("Friends of Hopewell", "Friends of Hopewell \n", 
                             nj_nestboxes_clean$org)
nj_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "New Jersey", combined = FALSE) +
  theme(legend.position = "right")
save_ggplot("nj_number_of_chicks_by_org.png", rfile, v) #, width = w, height = h, units = "in"

# cumulative plot using geom_area
nj_nestboxes_clean %>%
  expand(year, org) %>%
  left_join(nj_nestboxes_clean) %>%
  replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "New Jersey") +
  theme(legend.position = "right")
save_ggplot("nj_number_of_chicks_by_org_cumulative.png", rfile, v)#, width = w, height = h_cum, units = "in"


# chicks per box ------------------------------------------------------

# by org
nj_nestboxes_clean %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "New Jersey") +
  theme(legend.position = "right")
save_ggplot("nj_chicks_per_nested_box.png", rfile, v)#, width = 7, height = h, units = "in"



# ***************************************************************************
# *** Pennsylvania ------------------------------------------------------------
# ***************************************************************************

# clean data --------------------------------------------------------------

pa_nestboxes_clean <- pa_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year)) %>%
  dplyr::rename(chicks_per_box = chicks_nested_box) %>%
  # change factor order for org to be ordered by first year of data
  dplyr::group_by(org) %>%
  dplyr::mutate(min_year = min(year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(org = fct_reorder(org, min_year, min, .desc = FALSE),
                org_key = paste0(org, " - ", org_key),
                org_key = fct_reorder(org_key, min_year, min, .desc = FALSE))


# color palette -----------------------------------------------------------

pa_pal <- c('#AD2323', '#3cb44b', '#4363d8', '#f58231', '#ffe119', '#42d4f4', '#f032e6', '#fabed4', '#9A6324', '#a9a9a9', '#000000')
names(pa_pal) <- levels(pa_nestboxes_clean$org)

# chicks per year -----------------------------------------------------

# combined
pa_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(sum_chicks_banded_per_year != 0) %>%
  kestrel_plot_chicks_per_year(region = "Pennsylvania", combined = TRUE,
                               label_col = sum_chicks_banded_per_year)
save_ggplot("pa_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")

# by organization
pa_nestboxes_clean %>%
  dplyr::filter(chicks_banded!=0) %>%
  kestrel_plot_chicks_per_year(region = "Pennsylvania", 
                               combined = FALSE, 
                               labels_as_points = FALSE, 
                               label_col = label_chicks_banded) +
  guides(col = guide_legend(nrow = 3)) +
  labs(caption = "* indicates incomplete data") + 
  theme(plot.caption = element_text(size = 10, face = "italic")) +
  scale_color_manual(values = pa_pal)
save_ggplot("pa_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# no numbers and label end of line
pa_nestboxes_clean %>%
  dplyr::filter(chicks_banded!=0) %>%
  kestrel_plot_chicks_per_year(region = "Pennsylvania", 
                               combined = FALSE, 
                               labels_as_points = TRUE,
                               cols = watlington(11), 
                               label_col = label_chicks_banded) +
  labs(caption = "* indicates incomplete data") + 
  theme(plot.caption = element_text(size = 10, face = "italic"))
save_ggplot("pa_labeled_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# cumulative plot using geom_area - 
pa_nestboxes_clean %>%
  dplyr::filter(year > "2013-01-01") %>%
  # expand(year, org) %>%
  # left_join(pa_nestboxes_clean) %>%
  # replace_na(list(chicks_banded = 0, 
  #                 chicks_per_box = 0)) %>%
  kestrel_plot_cumulative(region = "Pennsylvania") +
  guides(fill = guide_legend(nrow = 3)) +
  labs(caption = "* incomplete data for Karner in 2015, 2017, and 2018 and for PA Game Commission in 2020") + 
  theme(plot.caption = element_text(size = 10, face = "italic")) +
  scale_fill_manual(values = pa_pal)
save_ggplot("pa_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, 
            height = h_cum, units = "in")


# just the legend ---------------------------------------------------------

pa_pal_legend <- pa_pal
names(pa_pal_legend) <- levels(pa_nestboxes_clean$org_key)

# get plot for legend
p_legend <- pa_nestboxes_clean %>%
  ggplot() +
  theme_minimal() +
  geom_area(aes(x=year, y=chicks_banded, fill=org_key),
            alpha=1 , size=.5) +
  scale_fill_manual(values = pa_pal_legend,
                    labels = function(x) str_wrap(x, width = 100)) + 
  guides(fill = guide_legend(ncol = 1)) +
  labs(fill = "Key")

# Using the cowplot package
legend <- cowplot::get_legend(p_legend)

grid.newpage()
grid.draw(legend)

# Save as 3x6.5 pdf using the dropdown menu.
# Otherwise if you use save_ggplot as below it won't be just the legend.
# save_ggplot("key.pdf", rfile=rfile, v=v)

# chicks per box ------------------------------------------------------

# by org
pa_nestboxes_clean %>%
  dplyr::filter(chicks_per_box != 0) %>%
  na.omit(chicks_per_box) %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Pennsylvania") +
  guides(col = guide_legend(nrow = 3)) +
  scale_color_manual(values = pa_pal)
save_ggplot("pa_chicks_per_nested_box.png", rfile, v, width = w, height = h-2, units = "in")

# McKelvie separately -----------------------------------------------------

# chicks per box
pa_nestboxes_clean %>%
  dplyr::filter(org == "McKelvie") %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "McKelvie") +
  labs(caption = wrapper("Note: Prior to 2018 we were not putting enough chips on the bottom of the boxes, or were putting chips in too early in the year. Sometimes this resulted in starlings removing much of the chip layer before kestrels took up residence, and the kestrel eggs were laid on a very thin chip layer, or even on bare wood. Beginning in 2018 we added a 3-4 inch layer of fresh chips to each box immediately prior to nesting season. This greatly reduced the number of box failures, as you can see from our data.",
                         dev_scaler = 12)) +
  theme(legend.position = "none") +
  ggtitle("McKelvie kestrel nest box program")
save_ggplot("pa_mckelvie_chicks_per_nested_box.png", rfile, v, width = w, height = h, units = "in")

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
                org = as.factor(org))

# color palette -----------------------------------------------------------

ct_pal <- c(gg_color(2))
names(ct_pal) <- levels(ct_nestboxes_clean$org)

# chicks per year -----------------------------------------------------

# combined
ct_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "Connecticut", combined = TRUE,
                               label_col = sum_chicks_banded_per_year)
save_ggplot("ct_number_of_chicks.png", rfile, v) #, width = w, height = h, units = "in"

# by organization
ct_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "Connecticut", combined = FALSE) +
  theme(legend.position = "right") +
  scale_color_manual(values = ct_pal)
save_ggplot("ct_number_of_chicks_by_org.png", rfile, v) #, width = w, height = h, units = "in"

# cumulative plot using geom_area
ct_nestboxes_clean %>%
  # expand(year, org) %>%
  # left_join(ct_nestboxes_clean) %>%
  # replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "Connecticut") +
  scale_color_manual(values = ct_pal)
save_ggplot("ct_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, 
            height = h_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
ct_nestboxes_clean %>%
  na.omit(chicks_per_box) %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Connecticut") +
  theme(legend.position = "right") +
  expand_limits(y=1.7) +
  scale_color_manual(values = ct_pal)
save_ggplot("ct_chicks_per_nested_box.png", rfile, v) #, width = 7, height = h, units = "in"


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
  # expand(year, org) %>%
  # left_join(me_nestboxes_clean) %>%
  # replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "Maine")
save_ggplot("me_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, 
            height = h_cum, units = "in")


# ***************************************************************************
# *** Vermont ------------------------------------------------------------
# ***************************************************************************

# clean data --------------------------------------------------------------

# height and width of plots
w_vt = w
h_vt = h
h_vt_cum = h-2

vt_nestboxes_clean <- vt_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year))


# chicks per year -----------------------------------------------------

# combined
vt_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  kestrel_plot_chicks_per_year(region = "Vermont", combined = TRUE,
                               label_col = sum_chicks_banded_per_year) +
  labs(caption = "Data provided by Brian Lowe") +
  expand_limits(y = 70)
save_ggplot("vt_number_of_chicks.png", rfile, v) #, width = w_vt, height = h_vt, units = "in"

# by organization
vt_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "Vermont", combined = FALSE) +
  theme(legend.position = "none") +
  expand_limits(y = 70)
save_ggplot("vt_number_of_chicks_by_org.png", rfile, v) #, width = w_vt, height = h_vt, units = "in"

# cumulative plot using geom_area
vt_nestboxes_clean %>%
  # expand(year, org) %>%
  # left_join(vt_nestboxes_clean) %>%
  # replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "Vermont")
save_ggplot("vt_number_of_chicks_by_org_cumulative.png", rfile, v, width = w_vt, 
            height = h_vt_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
vt_nestboxes_clean %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Vermont") +
  labs(caption = "Data provided by Brian Lowe") +
  theme(legend.position = "none") +
  expand_limits(y = 2.2)
save_ggplot("vt_chicks_per_nested_box.png", rfile, v) #, width = w_vt, height = h_vt, units = "in"


# ***************************************************************************
# *** Virginia ------------------------------------------------------------
# ***************************************************************************

# clean data --------------------------------------------------------------

h = 4
va_nestboxes_clean <- va_nestboxes %>%
  janitor::clean_names() %>%
  dplyr::mutate(year = as.numeric(year),
                year = zoo::as.Date.yearmon(year))


# chicks per year -----------------------------------------------------

# combined
va_nestboxes_clean %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sum_chicks_banded_per_year = sum(chicks_banded)) %>%
  #dplyr::mutate(lab = c("316","393","283*","194")) %>%
  kestrel_plot_chicks_per_year(region = "Virginia", combined = TRUE, label_col = lab) +
  labs(caption = "* Incomplete data for 2019") +
  theme(plot.caption = element_text(size = 10, face = "italic"))
save_ggplot("va_number_of_chicks.png", rfile, v, width = w, height = h, units = "in")

# by organization
va_nestboxes_clean %>%
  kestrel_plot_chicks_per_year(region = "Virginia", combined = FALSE,
                               label_col = label_chicks_banded) +
  labs(caption = "* Reum notes count as a conservative estimate") +
  theme(plot.caption = element_text(size = 10, face = "italic"))
save_ggplot("va_number_of_chicks_by_org.png", rfile, v, width = w, height = h, units = "in")

# cumulative plot using geom_area
va_nestboxes_clean %>%
  # expand(year, org) %>%
  # left_join(va_nestboxes_clean) %>%
  # replace(is.na(.), 0) %>%
  kestrel_plot_cumulative(region = "Virginia")
save_ggplot("va_number_of_chicks_by_org_cumulative.png", rfile, v, width = w, 
            height = h_cum, units = "in")


# chicks per box ------------------------------------------------------

# by org
va_nestboxes_clean %>%
  dplyr::mutate(chicks_per_box = round(chicks_per_box, digits = 1)) %>%
  kestrel_plot_chicks_per_box(region = "Virginia") +
  theme(legend.position = "none")
save_ggplot("va_chicks_per_nested_box.png", rfile, v, width = w, height = h, 
            units = "in")




