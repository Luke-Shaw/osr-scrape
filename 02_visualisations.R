################################################################################
# Some plots. Assumes 01_webscrape_site.R been run
################################################################################

# theme barchart ----------------------------------------------------------

theme_tbl
# 2020-06-19 interesting that we're missing 46% (570) of the total correspondences
# from our breakdown. Seems to be a lot of correspondences aren't themed, like 
# general stats notes.

# barchart - excluding the row which is all themes (values!="")
theme_plot <- ggplot(theme_tbl %>% filter(values!=""), 
                     aes(x = str_wrap(names,10), y = num_letters)) + 
  geom_bar(stat="identity") +
  theme_bw() + 
  labs(title = "Number of correspondences by theme",
       x = "Theme", 
       y = "Frequency",
       caption = "Source: https://www.statisticsauthority.gov.uk/correspondence-list/")
theme_plot

# letters barchart ----------------------------------------------------------

letters_tbl

# Going to plot the yearly correspondences, coloured by where they were as at 
# this point of the year

# what day of the year is it right now (171 on 2020-06-19)
today_in_year <- lubridate::yday(Sys.Date())
# month and day for plot title
m <- lubridate::month(Sys.Date(), abbr=TRUE, label=TRUE)
d <- lubridate::day(Sys.Date()) %>% as.character()

# get the dataframe in format for plotting
yearly_correspondence <- letters_tbl %>%
  # find the year and whether it's before today in each year
  mutate(year = lubridate::year(date),
         before_today_in_year = lubridate::yday(date) <= today_in_year) %>%
  mutate(before_today_in_year = ifelse(before_today_in_year,"Yes","No")) %>% 
  group_by(year, before_today_in_year) %>%
  count()

yearly_plot <- ggplot(yearly_correspondence, aes(x=year, y=n, fill=before_today_in_year)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  scale_fill_manual(values = c("#17e33f","#007818")) +
  labs(title = paste0("OSR correspondences",
                      "\nfilled by whether before ",
                      d, " ", m," in each year"),
       x = "Year",
       y = "Frequency",
       fill = "Before today \nin year",
       caption = "Source: https://www.statisticsauthority.gov.uk/correspondence-list/")
yearly_plot
# annotate the plot - though will go out of date fairly quickly
yearly_plot + 
  annotate("rect", xmin = 2017, xmax = 2021, ymin = 60, ymax = 120, alpha = .2) +
  annotate("text", x=2020.5, y = 145, 
           label = str_wrap("2020 set to be highest, but similar to last 2 years",
                            width=15))
  

# Colour by title ---------------------------------------------------------

# trying to find who was involved in the correspondences - not that successful visuals

# identify when OSR members are in the title
letters_tbl <- letters_tbl %>% 
  mutate(names = case_when(
    # note stops when first true found
    str_detect(title, "David Norgrove") ~ "Sir David Norgrove",
    str_detect(title, "Ed Humpherson") ~ "Ed Humpherson",
    str_detect(title, "UK Statistics Authority") ~ "UKSA",
    str_detect(title, "OSR") ~ "OSR",
    TRUE~ "None"
  ))

# not tidied up
ggplot(letters_tbl, aes(x=date, fill = names)) +
  # roughly 1 a month bin-width
  geom_histogram(binwidth=90)

