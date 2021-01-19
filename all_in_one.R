library(ukcovid19)
library(readr)
library(dplyr)
# library(zoo)
# library(purrr)

all_uk = c(
  "areaType=overview"
)

cases_and_rates = list(
  date = "date",
  areaName = "areaName",
  areaCode = "areaCode",
  newCasesBySpecimenDate = "newCasesBySpecimenDate",
  cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate",
  newCasesByPublishDate = "newCasesByPublishDate",
  cumCasesByPublishDate = "cumCasesByPublishDate"
)

last_updated_remote <- last_update(
  filters = all_uk, 
  structure = cases_and_rates
) %>%
  as.Date()

last_updated_local <-
  read_csv("https://www.dropbox.com/s/1y03gs95ltrcit2/latest_data.csv?dl=1") %>%
  pull()

if (last_updated_remote > last_updated_local) {
  source("uk_api_eda.R")
  source("plotly_chart_uk_ltlas.R")
  source("msoa_data.R")
}
