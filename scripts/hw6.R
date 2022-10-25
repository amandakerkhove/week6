
library(tidyverse)

# Using Mongolia fish sampling data provided by Maggie
# Shaw and the Jensen lab, we are looking into how 
# fish length varies among sites and years?
# Pick one species

# First subset the data so that we are considering only
# one species (burbot) and one net type (sequential-5).
fish_data <- read_csv('all-spp_leng-mesh_2009-2022.csv') %>% 
  filter(net_type == "sequential-5") %>% 
  filter(species == "burbot") %>%
  # This site only had one year of data, so remove it
  filter(location != "Hatgal")

unique(x$location) 
unique(x$year)


