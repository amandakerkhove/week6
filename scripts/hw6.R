
library(tidyverse)
library(nlme)

# Using Mongolia fish sampling data provided by Maggie
# Shaw and the Jensen lab, we are looking into how 
# fish length varies among sites and years?
# Pick one species

# First subset the data so that we are considering only
# one species (burbot) and one net type (sequential-5).
fish_data <- read_csv('data/all-spp_leng-mesh_2009-2022.csv') %>% 
  filter(net_type == "sequential-5") %>% 
  filter(species == "burbot") %>%
  # This site only had one year of data, so remove it
  filter(location != "Hatgal")

unique(fish_data$location) 
# I don't think year gives us the nested structure we need for a hierarchical model
unique(fish_data$year) 
# I think that mesh size might be a good random effect
unique(fish_data$mesh_mm)

# Crosswalking to RIKZ data terms:
# Beach = location
# Site = mesh_mm
# Y = length_mm (what are we saying length is related to?)
# X = ?

fish_data %>% 
  group_by(location, mesh_mm) %>% 
  tally()

##### Figure out the appropriate model #####

fish_data_ready <- fish_data %>% 
  # Sample has to have measured mesh size
  filter(!is.na(mesh_mm)) %>% 
  mutate(floc = factor(location),
         fmesh = factor(mesh_mm)) %>% 
  select(year, floc, fmesh, fish_length = length_mm)

# No random effects

m1 <- gls(fish_length ~ 1 + fmesh,
          method = "REML", data = fish_data_ready)

# Add in random effects
m2 <- lme(fish_length ~ 1 + fmesh,
          random = ~ 1 | floc,
          method = "REML", data = fish_data_ready)
m3 <- lme(fish_length ~ 1 + fmesh,
          random = ~ 1 + fmesh | floc,
          method = "REML", data = fish_data_ready)

summary(m1) # AIC = 1972.441
summary(m2) # AIC = 1974.441
summary(m3)

# Choose the appropriate model based on lowest AIC. Then 
# use `summary()` to drop individual terms from the model
# based on their statistical significance using p-val
summary(m1)

# Then write out the optimal model
moptimal <- m1

##### Run simulation #####

# Extract the mean and variance using the actual data


# Generate simulation data using random values from a distributions


##### Evaluate simulation outcomes #####
