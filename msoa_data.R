library(readr)
library(ggplot2)
library(ggrepel)

msoa_data <- read_csv(paste0("https://coronavirus-staging.data.gov.uk/",
                             "downloads/msoa_data/MSOAs_latest.csv"))

msoas_of_interest <-
  data.frame(areaName = c("North Laine & the Lanes", "Aldrington South",
                          "Hassocks, Keymer & East Hurstpierpoint",
                          "Bradley", "World's End",
                          "Talbot & Branksome Woods"),
             who = c("Dean & Julie", "Neil", "Andrea", "Shirley", "Gwenda",
                     "Russell")
  )

filtered_data <- msoa_data %>%
  inner_join(msoas_of_interest) %>%
  filter(date >= max(msoa_data$date, na.rm = TRUE) - 90) %>%
  mutate(newCasesBySpecimenDateRollingRate =
           if_else(is.na(newCasesBySpecimenDateRollingRate),
                   2,
                   newCasesBySpecimenDateRollingRate),
         label = paste(who, areaName,
                       round(newCasesBySpecimenDateRollingRate, digits = 0),
                       sep = " - "))

p <- ggplot(filtered_data) +
  geom_line(aes(x = date, y = newCasesBySpecimenDateRollingRate,
                group = who, colour = who)) +
  geom_label_repel(data = filtered_data %>%
               filter(date == max(msoa_data$date, na.rm = TRUE)),
             aes(label = label, x = date,
                 y = newCasesBySpecimenDateRollingRate)) +
  xlab("Date") +
  ylab("Number of positive tests in previous 7 days per 100k") +
  ggtitle(paste0("The number of cases in last 7 days per 100k by MSOA.  ",
                 "Last updated: ", max(filtered_data$date))) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  labs(caption = "Source: http://coronavirus.data.gov.uk") +
  theme(legend.position = "none")

p + ggsave("UK MSOAs.png", height = 12, width = 24, units = "cm")
