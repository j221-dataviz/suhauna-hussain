# load required package
library(scales)
library(dplyr)

# load required packages
library(ggplot2)
library(readr)

library(lubridate)

# load data and add column for donor "Amount" in millions and extract year
foreign_gifts <- read_csv("foreign_gifts_final.csv") %>%
  mutate(Amount_millions = Amount/10^6) %>%
  mutate(Year = year(Date))


#made new data set, manually added whether was corporation or not
berkeley <- foreign_gifts %>%
  filter(grepl("Berkeley", Name)) %>%
  group_by(Donor,Year) %>%
  summarize(Amount_millions=sum(Amount_millions)) %>%
  ungroup()

berkeley1 <- read_csv("berkeley_corporations.csv") %>%
  group_by(Year) %>%
  summarize(Amount_millions=sum(Amount_millions)) 

berkeley2 <- read_csv("berkeley_corporations.csv") %>%
  filter(Corporation == 1) %>%
  group_by(Year) %>%
  summarize(Amount_millions=sum(Amount_millions, na.rm=TRUE)) 


# bar chart by year, entire state
ggplot(berkeley1, aes(x = year, y = Amount_millions)) + 
  geom_bar() 
#xlab("") +
#ylab("Incomplete") +
#ggtitle("Immunization in California kindergartens, entire state") + 
#theme(panel.grid.minor.x = element_blank())



# bar chart by year, entire state
corp_chart <- ggplot(berkeley2, aes(x = year, y = Amount_millions)) + 
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  theme_minimal(base_size = 12, base_family = "Georgia") + 
  scale_y_continuous(limits = c(0,50)) +
  scale_x_continuous(breaks = c(2010,2011,2012,2013,2014,2015,2016)) 
  #xlab("") +
  #ylab("Incomplete") +
  #ggtitle("Immunization in California kindergartens, entire state") + 
  #theme(panel.grid.minor.x = element_blank())

  


