# Data Processing/Preparation for Final Project App -----------------------
# Load Packages -----------------------------------------------------------
library(tidyverse)
library(janitor)
library(tigris)

# Load police data -------------------------------------------------------
police <- read_csv("data/police_killings.csv") %>% 
  mutate(
    share_white = as.numeric(share_white),
    share_black = as.numeric(share_black),
    share_hispanic = as.numeric(share_hispanic),
    age = as.numeric(age),
    state_fp = replace(state_fp, state == "NY", 36),
    latitude = replace(latitude, city == "Rochester", 43.14785),
    longitude = replace(longitude, city == "Rochester", -77.63095)
  )

# Get US data from tigris ---------------------------------
us <- states(cb = TRUE) %>% 
  clean_names() %>% 
  rename(state_fp = statefp) %>% 
  mutate(state_fp = as.numeric(state_fp))

# Join tigris data with police data ------------------------------------
police_geometry <- full_join(x = police, y = us, by = "state_fp")



