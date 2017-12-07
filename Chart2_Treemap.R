# install highcharter, RColorBrewer
install.packages("highcharter","RColorBrewer")

# load required packages
library(highcharter)
library(RColorBrewer)
library(readr)
library(dplyr)
library(treemap)
library(htmlwidgets)

# load data and add column for donor "Amount" in millions
foreign_gifts <- read_csv("foreign_gifts_final.csv") %>%
  mutate(Amount_millions = Amount/10^6)

# process data for treemap
tree_country_donations <- foreign_gifts %>% 
  group_by(Country, CONTROL) %>% 
  summarize(Amount_millions = sum(Amount_millions, na.rm=TRUE))

# use treemap package to create static treemap chart
tree_chart <- treemap(tree_country_donations, #Your data frame object
        index = c("CONTROL","Country"),  #A list of your categorical variables
        vSize = "Amount_millions",  #This is your quantitative variable
        vColor = "CONTROL", # The categorical variable to be used to set the colors
        type="categorical", #Type sets the organization and color scheme of your treemap
        palette = "Set2",  #Select your color palette from the RColorBrewer presets or make your own.
        title="", #Customize your title
        fontsize.title = 14 #Change the font size of the title
)

# now convert this to a highchart
treemap_interactive <- hctreemap(tree_chart, 
                                 allowDrillToNode = TRUE,
                                 layoutAlgorithm = "squarified", name = "tmdata") %>%
hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
             Amount: ${point.value:,.0f} million<br>") 



# save as a web page
saveWidget(treemap_interactive, "treemap.html", selfcontained = TRUE, libdir = NULL, background = "white")






