library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(purrr)
library(lubridate)
library(plotly)

cases <- read_csv(paste0("https://coronavirus.data.gov.uk/downloads/csv/",
                         "coronavirus-cases_latest.csv"))

ltlas <- cases %>%
  filter(`Area type` == "Lower tier local authority") %>%
  distinct(`Area name`, `Area code`) %>%
  mutate(type = "Lower tier local authority")

get_local_cases <- function(area_name, ltla, type) {
  print(paste("Processing", ltla))
  local_cases <- cases %>%
    filter(`Area code` == ltla,
           `Area type` == type)
  
  all_dates <- data.frame(
    date = seq.Date(from = min(local_cases$`Specimen date`),
                    to = max(local_cases$`Specimen date`),
                    by = "day"))
  
  local_overall <- all_dates %>%
    left_join(local_cases %>%
                select(date = `Specimen date`, `Daily lab-confirmed cases`)) %>%
    mutate(`Daily lab-confirmed cases` =
             if_else(is.na(`Daily lab-confirmed cases`),
                     0,
                     `Daily lab-confirmed cases`))
  
  local_overall$rolling_14_days <-
    rollsum(local_overall$`Daily lab-confirmed cases`, 14, align = "right",
            fill = NA)
  
  local_overall %>%
    mutate(area_name = !!area_name) %>%
    select(-`Daily lab-confirmed cases`)
}

all_results <- pmap_dfr(unname(ltlas), get_local_cases)

peaks <- all_results %>%
  group_by(area_name) %>%
  top_n(1, rolling_14_days) %>%
  top_n(1, date)

ggplot(peaks, aes(x = date)) +
  geom_histogram() +
  xlab("Date") +
  ylab("Number of LTLAs with their peak infections on this date") +
  ggtitle("Spread of peak infection dates by LTLA") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk")

# After 1st May has there been another peak?
daily_increase <- all_results %>%
  filter(date >= "2020-05-01") %>%
  group_by(area_name) %>%
  arrange(area_name, date) %>%
  mutate(increase = if_else(!is.na(rolling_14_days) &
                              rolling_14_days > lag(rolling_14_days,
                                                    default = 1e6), 1, 0),
         week_commencing = floor_date(date, "week", 1))

weekly_increase <- daily_increase %>%
  group_by(area_name, week_commencing) %>%
  summarise(days_increased = sum(increase)) %>%
  mutate(weekly_increase = if_else(days_increased > 3, 1, 0))

likely_peaks <- weekly_increase %>%
  group_by(area_name) %>%
  summarise(weekly_increases = sum(weekly_increase)) %>%
  filter(weekly_increases >= 2)

results_for_2nd_peaks <- all_results %>%
  semi_join(likely_peaks)

g <- ggplot(results_for_2nd_peaks) +
  geom_line(aes(x = date, y = rolling_14_days, group = area_name,
                colour = area_name)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 14 days") +
  ggtitle("The number of infectious people by UTLA") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk")

ggplotly()

england_cases <- get_local_cases("England", "E92000001", "Nation")

england_and_brighton <- england_cases %>%
  bind_rows(results_for_2nd_peaks %>%
              filter(area_name == "Brighton and Hove"))

population <- cases %>%
  inner_join(england_and_brighton, by = c("Area name" = "area_name")) %>%
  filter(`Area type` %in% c("Nation", "Upper tier local authority")) %>%
  mutate(population = `Cumulative lab-confirmed cases` /
           `Cumulative lab-confirmed cases rate` * 1e5) %>%
  filter(date == max(cases$`Specimen date`),
         date == `Specimen date`) %>%
  select(`Area name`, population)

england_and_brighton <- england_and_brighton %>%
  inner_join(population, by = c("area_name" = "Area name")) %>%
  mutate(rolling_14_days_rate = rolling_14_days / population * 1e5)

ggplot(england_and_brighton) +
  geom_line(aes(x = date, y = rolling_14_days_rate, group = area_name,
                colour = area_name)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 14 days per 100k") +
  ggtitle("The number of infectous people per 100k by UTLA") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk") +
  ggsave("plot 2.png", height = 12, width = 24, units = "cm")

ggplotly()
