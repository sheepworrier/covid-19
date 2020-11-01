library(ukcovid19)
library(readr)
library(dplyr)
library(zoo)

all_ltlas = c(
  "areaType=ltla"
)

all_uk = c(
  "areaType=overview"
)

cases_and_rates = list(
  date = "date",
  areaName = "areaName",
  areaCode = "areaCode",
  newCasesBySpecimenDate = "newCasesBySpecimenDate",
  cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate"
)

ltla_data <- get_data(
  filters = all_ltlas, 
  structure = cases_and_rates
) %>%
  arrange(areaName, date)

uk_data <- get_data(
  filters = all_uk, 
  structure = cases_and_rates
) %>%
  arrange(areaName, date)

all_data <- ltla_data %>%
  bind_rows(uk_data) %>%
  group_by(areaCode, areaName) %>%
  mutate(population = max(cumCasesBySpecimenDate, na.rm = TRUE) / 
           max(cumCasesBySpecimenDateRate, na.rm = TRUE) * 1e5,
         rolling_7_days_by_specimen =
           rollapplyr(newCasesBySpecimenDate, 7, sum, partial = TRUE,
                      align = "right"),
         specimen_7_day_rate = rolling_7_days_by_specimen /
           population * 1e5) %>%
  ungroup() %>%
  arrange(areaName, date)

write_csv(all_data, "~/Dropbox/Public/COVID-19/uk_rate_data.csv")
