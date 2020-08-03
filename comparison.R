library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(purrr)

cases <- read_csv(paste0("https://coronavirus.data.gov.uk/downloads/csv/",
                         "coronavirus-cases_latest.csv"))

ltlas <- data.frame(
  household = c("Julie & Neil (Brighton & Hove)", "Andrea (West Sussex)",
                "Russell (Bournemouth, Christchurch and Poole)",
                "Gwenda (Enfield)", "Shirley (Wolverhampton)"),
  `Area code` = c("E06000043", "E10000032", "E06000058", "E09000010",
                  "E08000031"),
  stringsAsFactors = FALSE
)

get_local_cases <- function(household, ltla) {
  local_cases <- cases %>%
    filter(`Area code` == ltla,
           `Area type` == "Upper tier local authority")
  
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
    mutate(household = !!household) %>%
    select(-`Daily lab-confirmed cases`)
}

all_results <- pmap_dfr(unname(ltlas), get_local_cases)

ggplot(all_results) +
  geom_line(aes(x = date, y = rolling_14_days, group = household,
                colour = household)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 14 days") +
  ggtitle("The number of infectous people by UTLA") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk") +
  ggsave("plot 1.png", height = 12, width = 24, units = "cm")

population <- cases %>%
  inner_join(ltlas, by = c("Area code" = "Area.code")) %>%
  filter(`Area type` == "Upper tier local authority") %>%
  mutate(population = `Cumulative lab-confirmed cases` /
           `Cumulative lab-confirmed cases rate` * 1e5) %>%
  group_by(household) %>%
  summarise_at(vars(population), list(mean))

population_adjusted_results <- all_results %>%
  inner_join(population) %>%
  mutate(rolling_14_days_rate = rolling_14_days / population * 1e5)

ggplot(population_adjusted_results) +
  geom_line(aes(x = date, y = rolling_14_days_rate, group = household,
                colour = household)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 14 days per 100k") +
  ggtitle("The number of infectous people per 100k by UTLA") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk") +
  ggsave("plot 2.png", height = 12, width = 24, units = "cm")

ggplotly()
