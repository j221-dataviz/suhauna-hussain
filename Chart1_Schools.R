# load required package
library(scales)
library(dplyr)

# load required packages
library(ggplot2)
library(readr)

# install and load htmlwidgets
install.packages("htmlwidgets")
library(htmlwidgets)

# install and load ggiraph
install.packages("ggiraph")
library(ggiraph)

# load data and add column for donor "Amount" in millions
foreign_gifts <- read_csv("foreign_gifts_final.csv") %>%
  mutate(Amount_millions = Amount/10^6)


# Amount by school (n)
amount_schools <- foreign_gifts %>% 
  group_by(Name) %>% 
  summarize(Amount_millions = sum(Amount_millions, na.rm=TRUE))

# identify top 20 countries with highest GDP
top20 <- foreign_gifts %>%
  group_by(Name) %>%
  summarize(Amount_millions = sum(Amount_millions, na.rm=TRUE)) %>%
  arrange(desc(Amount_millions)) %>%
  head(20) %>%  
  select(Name)

# top 20 schools amount
top20_schools <- semi_join(amount_schools, top20)



# column chart
schools_chart <- ggplot(top20_schools, aes(x = reorder(Name, Amount_millions), y = Amount_millions)) + 
  xlab("College") +
  ylab("Foreign donations (millions)") +
  theme_minimal(base_size = 30, base_family = "Helvetica") + 
  geom_bar_interactive(stat = "identity", 
                       color = "white", 
                       fill = "red", 
                       alpha = .6,
                       width = 10,
                       aes(tooltip = paste0("Foreign donations: $", Amount_millions, " million"),
                           data_id = Name)) + 
  coord_flip()

schools_chart_interactive2 <- ggiraph(code = print(schools_chart), 
                                          height_svg = 16,
                                          width_svg = 30,
                                          hover_css = "fill-opacity:1;stroke:red",
                                          tooltip_extra_css = "background-color:#f0f0f0;color:black;padding:5px")

# save as a web page
saveWidget(schools_chart_interactive2, "schools_chart.html", selfcontained = TRUE, libdir = NULL, background = "white")
  

  
  
  