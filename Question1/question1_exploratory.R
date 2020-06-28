# Question under Investigation: How has the number of bapstims of children of record changed in regard to number of children being born per woman?

library(tidyverse)
library(googlesheets4)
library(lubridate)

url <- "https://docs.google.com/spreadsheets/d/1n_M_kvV2jdVsGqCPkE6twRFb3IoV06lSvkfB7sUC8rE/edit?usp=sharing"
# Create a sheets_id object
ssid <- googlesheets4::as_sheets_id(url)
# have to unclass ssid first to obtain the Sheet ID. ssid currently contains metadata about the sheet.
church_data <- read_sheet(unclass(ssid))

View(church_data)

# Change first entry year to 2020-04-06
church_data$`End of Year` <- as.numeric(church_data$`End of Year`)
church_data[1,1] <- 1830
church_data19$baptisms_children_record <- as.numeric(church_data19$baptisms_children_record)
# class(church_data$`End of Year`)


# Get rid of year 2020 and above.
church_data19 <- church_data[-c(192:257),]

church_data19 <- select(church_data19, c('End of Year', "Baptisms of Children of Record", 'Children per Woman')) %>%
  rename(
    'year' = "End of Year",
    'baptisms_children_record' = "Baptisms of Children of Record",
    "child_per_woman" = "Children per Woman"
  ) 
just2003 <- filter(church_data19, year == 2003)
church_data19 %>% 
  ggplot(aes(x = year, y = baptisms_children_record)) +
  geom_col(color = 'gray') +
  geom_line(aes(x = 2003), color = "red") +
  geom_text(just2003, mapping = aes(label = baptisms_children_record), vjust = -0.5) +
  labs(y = "Baptisms of Children of Record", x = "Year") +
  theme_bw()



