
# Load Necessary Package --------------------------------------------------
library(tidyverse)
library(tigris)
library(maps)
library(sf)
library(janitor)
library(rgeos)
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

# Load Data ---------------------------------------------------------------
# load police data
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

# load US income data
load(file = "data/US_income copy.rda")

# Rename state name variable
US_income <- US_income %>% 
  rename(state = name)

# load state names data
state_names <- read_csv(file = "data/state_names.csv") %>% 
  clean_names()

# Create Map --------------------------------------------------------------
# get US data from tigris
us <- states(cb = TRUE) %>% 
  clean_names() %>% 
  rename(state_fp = statefp) %>% 
  mutate(state_fp = as.numeric(state_fp))
  
# join tigris data with police data
police_geometry <- full_join(x = police, y = us, by = "state_fp")

# join US income data with state data
US_income <- US_income %>% 
  left_join(x = US_income, y = state_names, by = "state")

# join police data with US_income
police_US <- left_join(x = police, y = US_income, by = "code")


# get data  from urbnmapr
states_sf <- get_urbn_map("states", sf = TRUE)  %>% 
  rename(code = state_abbv)

# join states_sf with police data
police_states <- full_join(x = police, y = states_sf, by = "code")


# create state_name_widget
state_name_widget <- police_geometry %>% 
  mutate(name.y = as_factor(name.y)) %>% 
  pull(name.y) %>% 
  unique() %>% 
  str_to_title() 

# try one state graph (with data from tigris)
police_geometry %>% 
  filter(code == "IN") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity)) +
  coord_sf() +
  theme_void()

# try two state graphs
police_geometry %>% 
  filter(state != "AK" | state != "HI") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity)) +
  coord_sf() +
  theme_void()

# try all us (with data from tigris)
ggplot(data = us) +
  geom_sf(aes(geometry = geometry)) +
  geom_point(data = police, aes(x = longitude, y = latitude)) +
  coord_sf() +
  theme_void()
  
# graph all us (with data from tigris)
police_geometry %>% 
  filter(code == "CA") %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity))

# try all us (with data from urbnmapr)
ggplot(data = states_sf) +
  geom_sf(aes(geometry = geometry)) +
  geom_point(data = police, aes(x = longitude, y = latitude))

# create data-set of cities in west
west_cities <- tribble(
  ~city, ~lat, ~lon,
  #----|-----|-----|
  "San Francisco", 37.7749, -122.4194,
  "Los Angeles", 34.0522, -118.2437,
  "Denver", 39.7392, -104.9903,
  "Salt Lake City", 40.7608, -111.8910,
  "Portland", 45.5051, -122.6750,
  "Seattle", 47.6062, -122.3321
)


# try west region with tigris data
police_geometry %>% 
  filter(state %in% c("CA", "OR", "WA", "NV", "UT", "CO", "MT", "ID", "WY")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity), alpha = 0.5) +
  theme_void() +
  scale_color_brewer(palette = "Set1",
                     limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")) +
  labs(
    title = "Locations of Police Killings in West",
    color = "Race of\nVictim",
    caption = "Each point represents one victim, though note that some points are plotted on top of one another"
  ) +
  annotate(
    geom = "text",
    x = -123.5, y = 35,
    label = "San\nFrancisco"
  ) +
  annotate(
    geom = "curve", 
    x = -123, y = 35, 
    xend = -122.4194, yend = 37.7749, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -121.5, y = 33,
    label = "Los\nAngeles"
  ) +
  annotate(
    geom = "curve", 
    x = -121, y = 33, 
    xend = -118.2437, yend = 34.0522, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -102, y = 43,
    label = "Denver"
  ) +
  annotate(
    geom = "curve", 
    x = -102, y = 42.7, 
    xend = -104.9903, yend = 39.7392, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -112, y = 34,
    label = "Las Vegas"
  ) +
  annotate(
    geom = "curve", 
    x = -113, y = 34, 
    xend = -115.1398, yend = 36.1699, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# try southwest region with tigris data
police_geometry %>% 
  filter(state %in% c("AZ", "NM", "TX", "OK")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity), alpha = 0.5) +
  theme_void() +
  scale_color_brewer(palette = "Set1",
                     limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")) +
  labs(
    title = "Locations of Police Killings in Southwest",
    color = "Race of\nVictim",
    caption = "Each point represents one victim, though note that some points are plotted on top of one another"
  )  +
  annotate(
    geom = "text",
    x = -114, y = 30.7,
    label = "Phoenix"
  ) +
  annotate(
    geom = "curve", 
    x = -114, y = 31, 
    xend = -112.0740, yend = 33.4484, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -94.5, y = 26.8,
    label = "Houston"
  ) +
  annotate(
    geom = "curve", 
    x = -94.5, y = 27, 
    xend = -95.3698, yend = 29.7604, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -98, y = 31,
    label = "Dallas",
    hjust = 1
  ) +
  annotate(
    geom = "curve", 
    x = -98, y = 31, 
    xend = -96.7970, yend = 32.7767, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -97, y = 38,
    label = "Oklahoma City",
    hjust = 0
  ) +
  annotate(
    geom = "curve", 
    x = -97, y = 38, 
    xend = -97.5164, yend = 35.4676, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# try midwest region with tigris data
police_geometry %>% 
  filter(state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity), alpha = 0.5) +
  theme_void() +
  scale_color_brewer(palette = "Set1",
                     limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")) +
  labs(
    title = "Locations of Police Killings in Midwest",
    color = "Race of\nVictim",
    caption = "Each point represents one victim, though note that some points are plotted on top of one another"
  ) +
  annotate(
    geom = "text",
    x = -98, y = 45,
    label = "Minneapolis",
    hjust = 1
  ) +
  annotate(
    geom = "curve", 
    x = -98, y = 45, 
    xend = -93.2650, yend = 44.9778, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -88, y = 41,
    label = "Chicago",
    hjust = 1
  ) +
  annotate(
    geom = "curve", 
    x = -88, y = 41, 
    xend = -87.6298, yend = 41.8781, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# try southeast region with tigris data
police_geometry %>% 
  filter(state %in% c("AR", "LA", "MS", "AL", "FL", "TN", "KY", "GA", "SC", "NC", "VA", "WV")) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity), alpha = 0.5) +
  theme_void() +
  scale_color_brewer(palette = "Set1",
                     limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")) +
  labs(
    title = "Locations of Police Killings in Southeast",
    color = "Race of\nVictim",
    caption = "Each point represents one victim, though note that some points are plotted on top of one another"
  ) +
  annotate(
    geom = "text",
    x = -84, y = 32.6,
    label = "Atlanta",
    hjust = .5
  ) +
  annotate(
    geom = "curve", 
    x = -84, y = 32.6, 
    xend = -84.3880, yend = 33.7490, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -79, y = 28,
    label = "Orlando",
    hjust = 0
  ) +
  annotate(
    geom = "curve", 
    x = -79, y = 28, 
    xend = -81.15, yend = 28.5383, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# try northeast region with tigris data
police_geometry %>% 
  filter(state %in% c("NY", "PA", "MD", "DC", "DE", "NJ", "CT", "MA", "NH", "VT", "ME")) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry)) +
  coord_sf() +
  geom_point(aes(x = longitude, y = latitude, color = raceethnicity), alpha = 0.5) +
  theme_void() +
  scale_color_brewer(palette = "Set1",
                     limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")) +
  labs(
    title = "Locations of Police Killings in Northeast",
    color = "Race of\nVictim",
    caption = "Each point represents one victim, though note that some points are plotted on top of one another"
  ) +
  annotate(
    geom = "text",
    x = -72, y = 39.5,
    label = "New York City",
    hjust = 0
  ) +
  annotate(
    geom = "curve", 
    x = -72, y = 39.5, 
    xend = -74.0060, yend = 40.7128, 
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  annotate(
    geom = "text",
    x = -78, y = 38,
    label = "Baltimore",
    hjust = 1
  ) +
  annotate(
    geom = "curve", 
    x = -78, y = 38, 
    xend = -76.6122, yend = 39.2904,
    curvature = .2, 
    arrow = arrow(length = unit(2, "mm")),
    color = "black"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

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


# Bargraph of Cause of Death ----------------------------------------------
cause_counts <- police %>% 
  count(cause)

# Cause of Death by Itself
ggplot(police, aes(fct_rev(fct_infreq(cause)))) +
  geom_bar(aes(fill = cause), show.legend = FALSE) +
  coord_flip() +
  labs(
    x = "Cause of Death",
    y = "Number of Victims"
  ) +
  scale_fill_manual(
    values = c("Gunshot" = "dark red", 
               "Taser" = "black", 
               "Death in custody" = "black", 
               "Struck by vehicle" = "black",  
               "Unknown" = "black")
  ) +
  geom_label(data = cause_counts, aes(label = n, y = n), show.legend = FALSE) +
  theme_minimal() 

# Cause of Death by Armed
ggplot(police, aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Cause of Death",
    y = "Percentage of Victims",
    title = "Cause of Death By How Victim was Armed",
    fill = "Weapon Carried\nby Victim"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = .5)
  )

# Cause of Death by Race
ggplot(police, aes(cause, fill = raceethnicity)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Cause of Death",
    y = "Percentage of Victims",
    title = "Cause of Death By Race of Victim",
    fill = "Race"
  ) +
  scale_fill_brewer(palette = "Set1",
                    limits = c("White", "Hispanic/Latino", "Black", "Native American", 
                               "Unknown", "Asian/Pacific Islander")) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = .5)
  )


# Unused Bargraphs --------------------------------------------------------
police %>% 
  filter(raceethnicity == "Black") %>% 
  ggplot(aes(cause)) +
  geom_bar()

police %>% 
  filter(raceethnicity == "White") %>% 
  ggplot(aes(cause)) +
  geom_bar()

police %>% 
  filter(raceethnicity == "Hispanic/Latino") %>% 
  ggplot(aes(cause)) +
  geom_bar()

ggplot(police, aes(armed)) +
  geom_bar()

ggplot(police, aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip()

ggplot(police, aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip()

ggplot(police, aes(cause, fill = raceethnicity)) +
  geom_bar(position = "fill") +
  coord_flip()

police %>% 
  filter(raceethnicity == "White") %>% 
  ggplot(aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(
    values = c("#FF5354", "#BE8900", "#5BA700", "#18EE00", "#00BC7E", "#00A9ED", "#9B68FF")
  )

police %>% 
  filter(raceethnicity == "Black") %>% 
  ggplot(aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_discrete(
    limits = c("Firearm", "Knife", "No", "Non-Lethal Firearm", "Other", "Unknown", "Vehicle", "Disputed")
  )

police %>% 
  filter(raceethnicity == "Asian/Pacific Islander") %>% 
  ggplot(aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip()

police %>% 
  filter(raceethnicity == "Hispanic/Latino") %>% 
  ggplot(aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip()

police %>% 
  filter(raceethnicity == "Native American") %>% 
  ggplot(aes(cause, fill = armed)) +
  geom_bar(position = "fill") +
  coord_flip()

ggplot(police, aes(cause, fill = raceethnicity)) +
  geom_bar(position = "dodge") +
  coord_flip()

ggplot(police, aes(cause, fill = raceethnicity)) +
  geom_bar(position = "fill") +
  coord_flip()

ggplot(police, aes(armed, fill = raceethnicity)) +
  geom_bar(position = "fill") +
  coord_flip()

ggplot(data = police, aes(h_income, fill = raceethnicity, color = raceethnicity)) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(
    labels = label_comma()
  ) +
  scale_color_brewer(
    palette = "Set1",
    limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
  ) +
  scale_fill_brewer(
    palette = "Set1",
    limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
  ) +
  theme_minimal() +
  labs(
    x = "median household income of census tract (dollars)",
    fill = "Race of Victim",
    color = "Race of Victim",
    title = "Income of Census Tract by Race of Victim"
  ) 


# Density Plot of Census Tract Population Percentages ---------------------
ggplot(police, aes(share_white)) +
  geom_histogram(bin_width = 3)

ggplot(police, aes(share_black)) +
  geom_histogram()

ggplot(police, aes(share_hispanic)) +
  geom_histogram()

ggplot(police, aes(h_income)) +
  geom_histogram()
