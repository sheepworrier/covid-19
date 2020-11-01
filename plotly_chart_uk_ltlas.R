library(readr)
library(ggplot2)
library(plotly)

all_data <-
  read_csv("https://www.dropbox.com/s/nai0vhgr3tlcfb8/uk_rate_data.csv?dl=1")

latest_data <- max(all_data$date, na.rm = FALSE)

filtered_data <- all_data %>%
  filter(areaName %in% c("Brighton and Hove", "United Kingdom",
                         "Wolverhampton", "Enfield", "Mid Sussex",
                         "Bournemouth, Christchurch and Poole"),
         date <= latest_data - 4,
         date >= latest_data - 90)

p <- ggplot(filtered_data) +
  geom_line(aes(x = date, y = specimen_7_day_rate, group = areaName,
                colour = areaName)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 7 days per 100k") +
  ggtitle("The number of infectous people per 100k by LTLA") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk")

ggplotly(p)

p + ggsave("UK LTLAs.png", height = 12, width = 24, units = "cm")
