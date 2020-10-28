library(ukcovid19)
# library(readr)
# library(tidyr)
library(dplyr)
library(zoo)
# library(ggplot2)
# library(purrr)
# library(lubridate)
# library(plotly)

query_filters <- c(
  'areaType=nation',
  'areaName=England'
)

cases_and_deaths = list(
  date = "date",
  areaName = "areaName",
  areaCode = "areaCode",
  newCasesByPublishDate = "newCasesByPublishDate",
  cumCasesByPublishDate = "cumCasesByPublishDate",
  newDeaths28DaysByPublishDate = "newDeaths28DaysByPublishDate",
  cumDeaths28DaysByPublishDate = "cumDeaths28DaysByPublishDate"
)

data <- get_data(
  filters = query_filters, 
  structure = cases_and_deaths
)

# Showing the head:
print(head(data))

all_ltlas = c(
  "areaType=ltla"
)

data <- get_data(
  filters = all_ltlas, 
  structure = cases_and_deaths,
  latest_by = "newCasesByPublishDate"
)

brighton_ltla <- c(
  "areaName=Brighton and Hove",
  "areaType=ltla"
)

cases_and_rates = list(
  date = "date",
  areaName = "areaName",
  areaCode = "areaCode",
  newCasesByPublishDate = "newCasesByPublishDate",
  cumCasesByPublishDate = "cumCasesByPublishDate",
  newCasesBySpecimenDate = "newCasesBySpecimenDate",
  cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
  cumCasesByPublishDateRate = "cumCasesByPublishDateRate",
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate"
)

brighton_data <- get_data(
  filters = brighton_ltla, 
  structure = cases_and_rates
) %>%
  arrange(date)

brighton_population <- max(brighton_data$cumCasesBySpecimenDate, na.rm = TRUE) /
  max(brighton_data$cumCasesBySpecimenDateRate, na.rm = TRUE) * 1e5

brighton_data$rolling_7_days_by_publish <-
  rollsum(brighton_data$newCasesByPublishDate, 7, align = "right",
          fill = NA)

brighton_data$rolling_7_days_by_specimen <-
  rollsum(brighton_data$newCasesBySpecimenDate, 7, align = "right",
          fill = NA)

brighton_data <- brighton_data %>%
  mutate(specimen_7_day_rate = rolling_7_days_by_specimen /
           brighton_population * 1e5,
         publish_7_day_rate = rolling_7_days_by_publish /
           brighton_population * 1e5)
