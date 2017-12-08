# load required packages
library(readr)
library(dplyr)
library(htmlwidgets)
library(networkD3)


# load data and add column for donor "Amount" in millions
foreign_gifts <- read_csv("foreign_gifts_final.csv") %>%
  mutate(Amount_millions = Amount/10^6)

# prepare the data for Sankey Network visualization

# filter for UC only
uc <- foreign_gifts %>%
  filter(grepl("University of California", Name)) %>%
  group_by(Country,Name) %>%
  summarize(Amount_millions=sum(Amount_millions)) %>%
  ungroup()

#alternative


##### First, all countries to all UC schools

# create data frame with nodes in the network = all the schools and all the countries, give them unqiue id numbers

# network nodes
nodes <- data_frame(name = unique(uc$Name)) %>%
  bind_rows(data_frame(name = unique(uc$Country)))
# sequence to use as ids (needs to start with 0 for this to work!)
id <- c(0:(nrow(nodes)-1))

nodes <- nodes %>%
  mutate(id = id)
# need to convert to plain data frame
nodes <- as.data.frame(nodes) 

# network links
links <- left_join(uc,nodes, by = c("Name" = "name")) %>%
  left_join(nodes, by = c("Country" = "name")) %>%
  select(id.x,id.y,Amount_millions) %>%
  ungroup()
# need to convert to plain data frame
links <- as.data.frame(links)

# plot the network
sankey <- sankeyNetwork(Links = links, 
         Nodes = nodes, 
         Source = "id.y",
         Target = "id.x", 
         Value = "Amount_millions", 
         NodeID = "name",
         fontSize = 12,
         fontFamily = "Helvetica",
         nodeWidth = 15)

saveWidget(sankey, "sankey1.html", selfcontained = TRUE, libdir = NULL, background = "white")

#### Top 10 donors to UC schools (removing Anonymous donations)

uc_top10 <- foreign_gifts %>%
  filter(grepl("University of California", Name) & Donor != "Anonymous") %>%
  group_by(Donor) %>%
  summarize(Amount=sum(Amount_millions)) %>%
  arrange(desc(Amount)) %>%
  head(10) %>%
  select(Donor)

uc_top10_network <- foreign_gifts %>%
  filter(grepl("University of California", Name)) %>%
  semi_join(uc_top10)
           
# network nodes
nodes <- data_frame(name = unique(uc_top10_network$Name)) %>%
  bind_rows(data_frame(name = unique(uc_top10_network$Donor)))
# sequence to use as ids (needs to start with 0 for this to work!)
id <- c(0:(nrow(nodes)-1))

nodes <- nodes %>%
  mutate(id = id)
# need to convert to plain data frame
nodes <- as.data.frame(nodes) 

# network links
links <- left_join(uc_top10_network,nodes, by = c("Name" = "name")) %>%
  left_join(nodes, by = c("Donor" = "name")) %>%
  select(id.x,id.y,Amount_millions) %>%
  ungroup()
# need to convert to plain data frame
links <- as.data.frame(links)

# plot the network
sankey <- sankeyNetwork(Links = links, 
                        Nodes = nodes, 
                        Source = "id.y",
                        Target = "id.x", 
                        Value = "Amount_millions", 
                        NodeID = "name",
                        fontSize = 12,
                        fontFamily = "Georgia",
                        nodeWidth = 30)

saveWidget(sankey, "sankey2.html", selfcontained = TRUE, libdir = NULL, background = "white")


  

