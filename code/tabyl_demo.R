library(pacman)

p_load(janitor, readr, lubridate, dplyr, forcats, stringr, tidyr)

raw <- read_csv("data/raw/callsForService_2018_11_08.csv") %>%
  clean_names()

glimpse(raw)

noises <- raw %>%
  mutate(calldate = mdy_hm(calldate),
         mon = month.abb[month(calldate)],
         day = factor(
           weekdays(calldate),
           levels = c("Monday", 
                     "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  filter(
    str_detect(reported_off_des, "DRUNK") |
      str_detect(reported_off_des, "NOISE") |
      str_detect(reported_off_des, "FTA")) %>% # control
  filter(call_source %in% c("911", "PHONE"),
         mon %in% c("Aug", "Sep", "Oct")) %>%
  mutate(mon = factor(mon, levels = c("Aug", "Sep", "Oct"))) %>%
  rename(noise = reported_off_des)

x <- noises %>%
  tabyl(day, noise)

x

# Compare to tidyverse equivalent

y <- noises %>%
  count(day, noise) %>%
  spread(noise, n)

class(y)
class(x)

attributes(y)
attributes(x)

# Now add adornments
x %>%
  adorn_totals("row") %>%
  attributes()

# Three-way

noises %>%
  tabyl(day, noise, mon)
