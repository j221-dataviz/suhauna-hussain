# load required package
library(scales)
library(dplyr)

# load required packages
library(ggplot2)
library(readr)

# install and load htmlwidgets
#install.packages("htmlwidgets")
library(htmlwidgets)

# install and load ggiraph
#install.packages("ggiraph")
library(ggiraph)

library(RColorBrewer)

# load data and add column for donor "Amount" in millions
foreign_gifts <- read_csv("foreign_gifts_final.csv") %>%
  mutate(Amount_millions = Amount/10^6)


# Amount by donor (n)
amount_donor <- foreign_gifts %>% 
  group_by(Donor, CONTROL) %>% 
  summarize(Amount_millions = sum(Amount_millions, na.rm=TRUE))

# identify top 20 countries with highest GDP
top10 <- foreign_gifts %>%
  filter(Donor != "Anonymous" & Donor != "Unrestricted funds" & Donor != "Corporation") %>%
  group_by(Donor) %>%
  summarize(Amount_millions = sum(Amount_millions, na.rm=TRUE)) %>%
  arrange(desc(Amount_millions)) %>%
  head(10) %>%  
  select(Donor)

# top 20 donors amount
top10_donors <- semi_join(amount_donor, top10)



# column chart
donors_chart <- ggplot(top10_donors, aes(x = reorder(Donor, Amount_millions), y = Amount_millions, fill=CONTROL, order=CONTROL)) + 
  xlab("Donor") +
  ylab("Foreign donations (millions)") +
  theme_minimal(base_size = 10, base_family = "Helvetica") + 
  geom_bar_interactive(stat = "identity", 
                       color = "white", 
                       #aes(fill = CONTROL),
                       position = "stack",
                       alpha = .6,
                       aes(tooltip = paste0("Foreign donations: $", Amount_millions, " million", " Type: ", CONTROL),
                           data_id = Donor)) + 
  coord_flip()

donors_chart + scale_fill_hue("Public"=40, "Private nonprofit"=35)

donors_chart_interactive <- ggiraph(code = print(donors_chart), 
                                      height_svg = 4,
                                      width_svg = 10,
                                      font = "Helvetica",
                                      hover_css = "fill-opacity:1;stroke:red",
                                      tooltip_extra_css = "background-color:#f0f0f0;color:black;padding:5px")

# save as a web page
saveWidget(donors_chart_interactive, "donors_chart.html", selfcontained = TRUE, libdir = NULL, background = "white")