library(pacman)

p_load(janitor, readr, lubridate, dplyr, forcats, stringr, tidyr)

# The last 365 days of calls for service to Ann Arbor Police Department
raw <- read_csv("data/raw/callsForService_2018_11_08.csv") %>%
  clean_names()

glimpse(raw)

reports <- raw %>%
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
  rename(issue = reported_off_des)

reports$issue[reports$issue == "NUISANCES ORD - NOISE / PROHIBITED HOURS / AREA"] <- "NUISANCE NOISE"

x <- reports %>%
  tabyl(day, issue)

x

# Compare to tidyverse equivalent

y <- reports %>%
  count(day, issue) %>%
  spread(issue, n)

y

class(y)
class(x)

attributes(iris)
attributes(y)
attributes(x) # the core!

# Now add adornments
x %>%
  adorn_totals("row")

x

fancy <- x %>%
  adorn_totals("row") %>%
  adorn_percentages("col") %>%
  adorn_ns()

fancy

# How did it know how to handle the totals?  Where did it get the Ns from?
attributes(fancy)
attr(fancy, "totals")
attr(fancy, "core")


# One-way vs. Two-way
# adorn_pct_formatting() behaves differently
reports %>%
  tabyl(mon) %>%
  adorn_pct_formatting() # column-wise, skips column of Ns

reports %>%
  tabyl(mon, call_source) %>%
  adorn_percentages() %>%
  adorn_pct_formatting() # row-wise

# Three-way

reports %>%
  tabyl(day, issue, mon)

# using stored variable names

x

x %>%
  adorn_title()

## ------- S3 Methods ----------

reports %>%
  tabyl(issue)

# same as
tabyl(reports, issue) # data.frame, col name

# also works on a vector
tabyl(reports$issue)

# It's a hack ... but I'm not ashamed.
# <go look at source code>

