library(readr)
library(tidyr)
library(dplyr)
library(zoo)
cases <- read_csv(paste0("https://coronavirus.data.gov.uk/downloads/csv/",
                         "coronavirus-cases_latest.csv"))

brighton_cases <- cases %>%
  filter(`Area code` == "E06000043",
         `Area type` == "Upper tier local authority")

all_dates <- data.frame(
  date = seq.Date(from = min(brighton_cases$`Specimen date`),
                  to = max(brighton_cases$`Specimen date`),
                  by = "day"))

brighton_overall <- all_dates %>%
  left_join(brighton_cases %>%
              select(date = `Specimen date`, `Daily lab-confirmed cases`)) %>%
  mutate(`Daily lab-confirmed cases` =
           if_else(is.na(`Daily lab-confirmed cases`),
                   0,
                   `Daily lab-confirmed cases`))

brighton_overall$rolling_14_days <-
  rollsum(brighton_overall$`Daily lab-confirmed cases`, 14, align = "right",
          fill = NA)

library(ggplot2)

ggplot(brighton_overall, aes(x = date)) +
  geom_bar(aes(y = `Daily lab-confirmed cases`,
               fill = "Daily lab-confirmed cases"),
           stat = "identity") +
  geom_line(aes(y = rolling_14_days, group = 1, colour = "rolling_14_days")) +
  scale_colour_manual(" ", values = c("rolling_14_days" = "blue",
                                      "Daily lab-confirmed cases" = "red")) +
  scale_fill_manual("", values = "red") +
  theme(legend.key = element_blank(),
        legend.title = element_blank(),
        legend.box = "vertical")

# Next compare the cumulative rate Brighton to South East to England
south_east <- cases %>%
  filter(`Area code` == "E12000008",
         `Area type` == "Region")

all_dates <- data.frame(
  date = seq.Date(from = min(south_east$`Specimen date`),
                  to = max(south_east$`Specimen date`),
                  by = "day"))

south_east_overall <- all_dates %>%
  left_join(south_east %>%
              select(date = `Specimen date`,
                     `Cumulative lab-confirmed cases rate`)) %>%
  mutate(`Cumulative lab-confirmed cases rate` =
           na.locf(`Cumulative lab-confirmed cases rate`)) %>%
  rename(south_east_rate = `Cumulative lab-confirmed cases rate`)

all_dates <- data.frame(
  date = seq.Date(from = min(brighton_cases$`Specimen date`),
                  to = max(brighton_cases$`Specimen date`),
                  by = "day"))

brighton_overall <- all_dates %>%
  left_join(brighton_cases %>%
              select(date = `Specimen date`,
                     `Cumulative lab-confirmed cases rate`)) %>%
  mutate(`Cumulative lab-confirmed cases rate` =
           na.locf(`Cumulative lab-confirmed cases rate`)) %>%
  rename(brighton_rate = `Cumulative lab-confirmed cases rate`)

england <- cases %>%
  filter(`Area code` == "E92000001",
         `Area type` == "Nation")

all_dates <- data.frame(
  date = seq.Date(from = min(england$`Specimen date`),
                  to = max(england$`Specimen date`),
                  by = "day"))

england_overall <- all_dates %>%
  left_join(england %>%
              select(date = `Specimen date`,
                     `Cumulative lab-confirmed cases rate`)) %>%
  mutate(`Cumulative lab-confirmed cases rate` =
           na.locf(`Cumulative lab-confirmed cases rate`)) %>%
  rename(england_rate = `Cumulative lab-confirmed cases rate`)

all_data <- england_overall %>%
  left_join(south_east_overall) %>%
  left_join(brighton_overall)

all_data[is.na(all_data)] <- 0

ggplot(all_data) +
  geom_line(aes(x = date, y = england_rate), colour = "red") +
  geom_line(aes(x = date, y = south_east_rate), colour = "green") +
  geom_line(aes(x = date, y = brighton_rate), colour = "blue") +
  theme(legend.position = "right")
