library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(purrr)
library(lubridate)
library(plotly)

cases <- read_csv(paste0("https://assets.publishing.service.gov.uk/government/",
                         "uploads/system/uploads/attachment_data/file/893339/",
                         "COVID-19_UK_testing_time_series_18_June.csv"))

antigen_tests_uk <- cases %>%
  filter(Nation == "UK",
         Pillar %in% c("Pillar 1 (plus Pillar 2 for Wales)",
                       "Pillar 2 (excluding Wales)")) %>%
  mutate(`Daily number of people tested` =
           as.numeric(`Daily number of people tested`),
         `Cumulative number of people tested` =
           as.numeric(`Cumulative number of people tested`),
         `Date of activity` = as.Date(`Date of activity`,
                                      format = "%d/%m/%Y")) %>%
  group_by(`Date of activity`) %>%
  select(`Date of activity`, starts_with("Daily")) %>%
  summarise_at(vars(`Daily number of tests`, `Daily number of people tested`,
                 `Daily number of positive cases`,
                 `Daily In-person (tests processed)`,
                 `Daily Delivery (tests sent out)`),
            list(sum)) %>%
  ungroup()

antigen_tests_uk$rolling_14_days <-
  rollsum(antigen_tests_uk$`Daily number of positive cases`, 14,
          align = "right", fill = NA)

antigen_tests_uk$rolling_7_days <-
  rollmean(antigen_tests_uk$`Daily number of positive cases`, 7,
           align = "right", fill = NA)

ggplot(antigen_tests_uk) +
  geom_line(aes(x = `Date of activity`, y = rolling_14_days)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 14 days") +
  ggtitle("The number of infectious people") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public")

ggplotly()

ggplot(antigen_tests_uk) +
  geom_line(aes(x = `Date of activity`, y = rolling_7_days)) +
  xlab("Date") +
  ylab("Moving average of positive tests in previous 7 days") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public")

ggplotly()
