library(readr)
library(tidyr)
library(dplyr)
library(zoo)
library(ggplot2)
library(purrr)
library(lubridate)
library(plotly)

deaths <- read_csv(paste0("https://coronavirus.data.gov.uk/downloads/csv/",
                          "coronavirus-deaths_latest.csv"))

uk_deaths <- deaths %>%
  filter(`Area code` == "K02000001") %>%
  mutate(dow = weekdays(`Reporting date`))

ggplot(uk_deaths) +
  geom_line(aes(x = `Reporting date`, y = `Daily change in deaths`,
                group = dow, colour = dow)) +
  xlab("Date") +
  ylab("Deaths by day of week") +
  ggtitle("Deaths by day of week") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk")

ggplotly()
