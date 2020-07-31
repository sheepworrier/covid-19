library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(purrr)
library(lubridate)
library(plotly)
library(wbstats)

options(scipen = 99999)

cases <-
  read_csv(
    paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",
           "csse_covid_19_data/csse_covid_19_time_series/",
           "time_series_covid19_confirmed_global.csv"))

filter_in <- data.frame(`Country/Region` = c("United Kingdom", "Spain",
                                             "Portugal", "France"),
                        country = c("GBR", "ESP", "PRT", "FRA"),
                        stringsAsFactors = FALSE)

population <- wb(country = filter_in$country, indicator = c("SP.POP.TOTL"),
                 mrv = 1) %>%
  select(iso3c, population = value)

get_area_cases <- function(area_name, area_code) {
  print(paste("Processing", area_name))
  area_cases <- cases %>%
    filter(`Country/Region` == area_name,
           is.na(`Province/State`)) %>%
    select(-c(`Province/State`, Lat, Long)) %>%
    pivot_longer(contains("/2"), names_to = "date") %>%
    mutate(date = as.Date(date, format = "%m/%d/%y"),
           value = value - lag(value),
           value = if_else(is.na(value) | value < 0,
                           0,
                           value),
           iso3c = area_code)
  
  area_cases$rolling_14_days <-
    rollsum(area_cases$value, 14, align = "right",
            fill = NA)
  
  area_cases %>%
    inner_join(population, by = c("iso3c")) %>%
    mutate(rolling_14_days_rate = rolling_14_days / population * 1e5) %>%
    select(-iso3c)
}

all_results <- pmap_dfr(unname(filter_in), get_area_cases)

ggplot(all_results) +
  geom_line(aes(x = date, y = rolling_14_days_rate, group = `Country/Region`,
                colour = `Country/Region`)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 14 days per 100k") +
  ggtitle("The number of infectous people per 100k by country") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = paste0("Sources:\n",
                        "Population: https://data.worldbank.org/\n",
                        "Cases: https://github.com/CSSEGISandData/COVID-19/")) +
  ggsave("plot by country.png", height = 12, width = 24, units = "cm")

ggplotly()
