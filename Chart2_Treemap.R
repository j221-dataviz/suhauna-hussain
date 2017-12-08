# install highcharter, RColorBrewer
install.packages("highcharter","RColorBrewer")

# load required packages
library(highcharter)
library(RColorBrewer)

# load data and add column for donor "Amount" in millions
foreign_gifts <- read_csv("foreign_gifts_final.csv") %>%
  mutate(Amount_millions = Amount/10^6)

tree_country_donations <- foreign_gifts %>% 
  group_by(Country) %>% 
  summarize(Amount_millions = sum(Amount_millions, na.rm=TRUE), CONTROL_2 = sum(CONTROL_2, na.rm=TRUE)) %>%
  arrange(-Amount_millions, -CONTROL_2) %>% 
  glimpse()
## Observations: 15
## Variables: 3
## $ manufacturer <chr> "dodge", "toyota", "volkswagen", "ford", "chevrol...
## $ n            <int> 37, 34, 27, 25, 19, 18, 14, 14, 13, 9, 8, 5, 4, 4, 3
## $ unique       <int> 4, 6, 4, 4, 4, 3, 2, 2, 3, 1, 1, 1, 1, 1, 1

hchart(tree_country_donations, "treemap", hcaes(x = Country, value = Amount_millions, color = CONTROL_2))