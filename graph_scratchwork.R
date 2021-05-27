
# Load Necessary Package --------------------------------------------------
library(tidyverse)
library(tigris)
library(maps)
library(sf)
library(janitor)

# Load Data ---------------------------------------------------------------
police <- read_csv("data/police_killings.csv") %>% 
  mutate(
    share_white = as.numeric(share_white),
    share_black = as.numeric(share_black),
    share_hispanic = as.numeric(share_hispanic),
    age = as.numeric(age)
  )

# Create Map --------------------------------------------------------------
# get US data from tigris
us <- states(cb = TRUE) %>% 
  clean_names() %>% 
  rename(state_fp = statefp) %>% 
  mutate(state_fp = as.numeric(state_fp))
  
# join with police data
police_geometry <- left_join(x = police, y = us, by = "state_fp")

# create state_name_widget
state_name_widget <- police_geometry %>% 
  mutate(name.y = as_factor(name.y)) %>% 
  pull(name.y) %>% 
  unique() %>% 
  str_to_title() 

# try one state graph
police_geometry %>% 
  filter(state == "CA") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity)) +
  coord_sf() +
  theme_void()


# graph
ggplot(data = america) +
  geom_sf() +
  coord_sf() +
  geom_point(data = police, mapping = aes(x = longitude, y = latitude))


# Bargraph of Race --------------------------------------------------------
police %>% 
  mutate(raceethnicity = raceethnicity %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(mapping = aes(raceethnicity)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()


# Bargraph of Gender ------------------------------------------------------
police %>% 
  mutate(gender = gender %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(mapping = aes(gender)) +
  geom_bar() +
  coord_flip() +
  theme_minimal()


# Histogram of Age --------------------------------------------------------
police %>% 
  mutate(age = as.numeric(age)) %>% 
  ggplot(aes(age)) +
  geom_histogram(binwidth = 3, color = "white") +
  theme_minimal()


# Scatterplot of h_income and college -------------------------------------
ggplot(police, aes(h_income, college, size = share_white)) +
  geom_point(shape = 21, color = "white", fill = "black", alpha = 0.5)

ggplot(police, aes(h_income, college, size = share_black)) +
  geom_point(shape = 21, color = "white", fill = "black", alpha = 0.5)

ggplot(police, aes(h_income, college, size = share_hispanic)) +
  geom_point(shape = 21, color = "white", fill = "black", alpha = 0.5)

ggplot(police, aes(h_income, college, fill = share_white)) +
  geom_hex()

