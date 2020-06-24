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

ltlas <- data.frame(name = c("Isle of Wight", "England", "Brighton and Hove"),
                    code = c("E06000046", "E92000001", "E06000043"),
                    type = c("Lower tier local authority", "Nation",
                             "Lower tier local authority"),
                    stringsAsFactors = FALSE)

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

population <- cases %>%
  inner_join(all_results, by = c("Area name" = "area_name")) %>%
  filter(`Area type` %in% c("Nation", "Lower tier local authority")) %>%
  mutate(population = `Cumulative lab-confirmed cases` /
           `Cumulative lab-confirmed cases rate` * 1e5) %>%
  filter(date == max(cases$`Specimen date`),
         date == `Specimen date`) %>%
  select(`Area name`, population)

adj_results <- all_results %>%
  inner_join(population, by = c("area_name" = "Area name")) %>%
  mutate(rolling_14_days_rate = rolling_14_days / population * 1e5)

ggplot(adj_results) +
  geom_line(aes(x = date, y = rolling_14_days_rate, group = area_name,
                colour = area_name)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 14 days per 100k") +
  ggtitle("The number of infectous people per 100k by UTLA") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk") +
  ggsave("plot 2.png", height = 12, width = 24, units = "cm")

ggplotly()
