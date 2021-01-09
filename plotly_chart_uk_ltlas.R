library(readr)
library(ggplot2)
library(ggrepel)
library(tidyr)

all_data <-
  read_csv("https://www.dropbox.com/s/nai0vhgr3tlcfb8/uk_rate_data.csv?dl=1")

latest_snapshot <- all_data %>%
  filter(snapshot_date == max(all_data$snapshot_date)) %>%
  select(-snapshot_date)

latest_data <- max(all_data$date, na.rm = FALSE)

ltlas_of_interest <-
  data.frame(areaName = c("Brighton and Hove", "United Kingdom",
                          "Wolverhampton", "Enfield", "Mid Sussex",
                          "Bournemouth, Christchurch and Poole"),
             who = c("Dean, Julie & Neil", "UK", "Shirley", "Gwenda", "Andrea",
                     "Russell")
  )

filtered_data <- latest_snapshot %>%
  inner_join(ltlas_of_interest) %>%
  filter(date <= latest_data - 4,
         date >= latest_data - 90) %>%
  mutate(label = if_else(who == "UK",
                         paste(who, round(specimen_7_day_rate, digits = 0),
                               sep = " - "),
                         paste(who, areaName,
                               round(specimen_7_day_rate, digits = 0),
                               sep = " - ")))

p <- ggplot(filtered_data) +
  geom_line(aes(x = date, y = specimen_7_day_rate, group = areaName,
                colour = areaName)) +
  geom_label_repel(data = filtered_data %>%
                     filter(date == latest_data - 4),
                   aes(label = label, x = date, y = specimen_7_day_rate)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 7 days per 100k") +
  ggtitle(paste0("The number of cases in last 7 days per 100k by LTLA.  ",
                 "Last updated: ", max(filtered_data$date))) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk") +
  theme(legend.position = "none")

p + ggsave("~/Dropbox/Public/COVID-19/UK LTLAs.png",
           height = 12, width = 24, units = "cm")

plot_selected_area <- function(area_name) {
  area_data <- latest_snapshot %>%
    filter(areaName == !!area_name,
           date >= latest_data - 250) %>%
    mutate(specimen_7_day_rate = if_else(date > latest_data - 5,
                                         NA_real_,
                                         specimen_7_day_rate)) %>%
    pivot_longer(cols = c(specimen_7_day_rate, publish_7_day_rate),
                 names_to = "series",
                 values_to = "rate") %>%
    mutate(label = paste(if_else(series == "specimen_7_day_rate",
                                 "Rate by specimen date",
                                 "Rate by publish date"),
                         round(rate, digits = 0),
                         sep = " - "))
  
  p2 <- ggplot(area_data) +
    geom_line(aes(x = date, y = rate, group = series, colour = series)) +
    geom_label_repel(data = area_data %>%
                       filter((date == latest_data - 5 &
                                 series == "specimen_7_day_rate") |
                                (date == latest_data &
                                   series == "publish_7_day_rate")),
                     aes(label = label, x = date, y = rate)) +
    geom_label_repel(data = area_data %>%
                       filter(date == "2020-12-24"),
                     aes(label = label, x = date, y = rate)) +
    xlab("Date") +
    ylab("Number of positive tests in previous 7 days per 100k") +
    ggtitle(paste0("The number of cases in last 7 days per 100k in ", area_name,
                   ".  Last updated: ", max(area_data$date))) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    labs(caption = "Source: http://coronavirus.data.gov.uk") +
    theme(legend.position = "none")
  
  p2 + ggsave(paste0("~/Dropbox/Public/COVID-19/", area_name, ".png"),
              height = 12, width = 24, units = "cm")
}

plot_selected_area("Brighton and Hove")
plot_selected_area("Enfield")
