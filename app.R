# Andrews Final Project: Shiny App for Police Murders in US in 2015 -------------------------------------------

# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)
library(tigris)
library(maps)
library(sf)
library(janitor)
library(rgeos)

# Source Data Processing Script --------------------------------------
source(file = "data_processing.R")

# Define UI for application that draws a histogram --------------------------
ui <- fluidPage(
  
  # Add Title
  titlePanel("Deaths by the Police in the United States (January-June 2015)"),
  
  # Add Main Interface
  sidebarLayout(
    # Sidebar for Introduction and Data Citation
    sidebarPanel(
      h2("Introduction"),
      p("Police brutality has, unfortunately, been a persistent problem in contemporary America. In the first 6 months of 
        2015 alone, 467 people were killed at the hands of police officers. This app provides
        detailed graphics and tables about those who were killed by police between January
        and June of 2015 in an attempt to raise awareness about police violence, 
        and bring the country closer to creating communities where all feel safe and welcome."),
      h2("Data"),
      p("The data used in this app was obtained from FiveThirtyEight, who utilized the data for their 2015 article", 
        em("Where Police Have Killed Americans in 2015."),
        "The journalists and data-scientists at FiveThirtyEight in turn compiled this data 
        from two different sources: the Guardian's data-base on police murders in the United States 
        and the 2015 American Community Survey conducted by the United States Census Bureau.
        To access the GitHub repo where the data was found,",
        a(href = "https://github.com/fivethirtyeight/data/tree/master/police-killings", "click here."))
      
    ),
    # Main Panel
    mainPanel(
      # Tab One: Drop-Down Menu Input, Map Output
      tabsetPanel(
        tabPanel(title = "Map",
                 br(),
                 br(),
                 selectInput("region",
                             label = "Select regional map to view:",
                             choices = c("West", 
                                         "Southwest", 
                                         "Southeast", 
                                         "Midwest",
                                         "Northeast"),
                             selected = "Midwest"
                 ),
                 plotOutput("map"),
                 br(),
                 br(),
                 p(strong("Note to viewer:"),
                   "Please be patient, maps take a moment to load."),
                 br(),
                 br(),
                 uiOutput("cities"),
                 br(),
                 tableOutput("city_dat"),
                 br(),
                 br(),
                 br()),
        # Tab Two: Drop-Down Menu Input, Bar Graph Output and
        # Slider Bar Input, Histogram Output
        tabPanel(title = "Demographics",
                 br(),
                 fluidRow(column(6, plotOutput("bargraph")),
                          column(6, plotOutput("histogram"))
                   ),
                 br(),
                 br(),
                 fluidRow(column(6, selectInput("var",
                                                label = "Choose a variable:",
                                                choices = c("Race", "Gender"))
                 ),
                 column(6, sliderInput("bins",
                                       label = "Select the number of bins:",
                                       min = 1,
                                       max = 70,
                                       value = 5,
                                       animate = TRUE))
                 ),
                 ),
        # Tab Three: Radio Button Input with Scatterplot Output
        tabPanel(title = "Census Tracts",
                 br(),
                 br(),
                 plotOutput("scatterplot"),
                 br(),
                 br(),
                 radioButtons("census_var",
                              label = "Select variable for size of points:",
                              choices = c("Percent non-Hispanic White",
                                          "Percent Black",
                                          "Percent Hispanic/Latinx"))),
        tabPanel(title = "Cause of Death")
            )
         )
      )
   )

# Define server logic required to draw a histogram  ----------------------
server <- function(input, output) {
  # create map output 
  output$map <- renderPlot({
    if (input$region == "West"){
     police_geometry %>% 
        filter(state %in% c("CA", "OR", "WA", "NV", "UT", "CO", "MT", "ID", "WY")) %>% 
        ggplot() +
        geom_sf(aes(geometry = geometry)) +
        coord_sf() +
        geom_point(aes(x = longitude, y = latitude, color = raceethnicity, shape = raceethnicity),
                   alpha = 0.95, size = 3) +
        theme_void() +
        scale_color_brewer(
          palette = "Set1",
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        scale_shape_manual(
          values = c(4, 6, 5, 0, 2, 1),
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        labs(
          title = "Locations of Police Killings in West",
          color = "Race of\nVictim",
          shape = "Race of\nVictim",
          caption = "Each shape represents one victim"
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
      
      girafe(ggobj  = west_plot)
      
    } else if (input$region == "Southwest") {
      police_geometry %>% 
        filter(state %in% c("AZ", "NM", "TX", "OK")) %>% 
        ggplot() +
        geom_sf(aes(geometry = geometry)) +
        coord_sf() +
        geom_point(aes(x = longitude, y = latitude, color = raceethnicity, shape = raceethnicity),
                   alpha = 0.95, size = 3) +
        theme_void() +
        scale_color_brewer(
          palette = "Set1",
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        scale_shape_manual(
          values = c(4, 6, 5, 0, 2, 1),
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        labs(
          title = "Locations of Police Killings in Southwest",
          color = "Race of\nVictim",
          shape = "Race of\nVictim",
          caption = "Each shape represents one victim"
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
    } else if (input$region == "Southeast") {
      police_geometry %>% 
        filter(state %in% c("AR", "LA", "MS", "AL", "FL", "TN", "KY", "GA", "SC", "NC", "VA", "WV")) %>% 
        ggplot() +
        geom_sf(aes(geometry = geometry)) +
        coord_sf() +
        geom_point(aes(x = longitude, y = latitude, color = raceethnicity, shape = raceethnicity),
                   alpha = 0.95, size = 3) +
        theme_void() +
        scale_color_brewer(
          palette = "Set1",
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        scale_shape_manual(
          values = c(4, 6, 5, 0, 2, 1),
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        labs(
          title = "Locations of Police Killings in Southeast",
          color = "Race of\nVictim",
          shape = "Race of\nVictim",
          caption = "Each shape represents one victim"
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
    } else if (input$region == "Midwest") {
      police_geometry %>% 
        filter(state %in% c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")) %>% 
        ggplot() +
        geom_sf(aes(geometry = geometry)) +
        coord_sf() +
        geom_point(aes(x = longitude, y = latitude, color = raceethnicity, shape = raceethnicity),
                   alpha = 0.95, size = 3) +
        theme_void() +
        scale_color_brewer(
          palette = "Set1",
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        scale_shape_manual(
          values = c(4, 6, 5, 0, 2, 1),
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        labs(
          title = "Locations of Police Killings in Midwest",
          color = "Race of\nVictim",
          shape = "Race of\nVictim",
          caption = "Each shape represents one victim"
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
    } else {
      police_geometry %>% 
        filter(state %in% c("NY", "PA", "MD", "DC", "DE", "NJ", "CT", "MA", "NH", "VT", "ME")) %>%
        ggplot() +
        geom_sf(aes(geometry = geometry)) +
        coord_sf() +
        geom_point(aes(x = longitude, y = latitude, color = raceethnicity, shape = raceethnicity),
                   alpha = 0.95, size = 3) +
        theme_void() +
        scale_color_brewer(
          palette = "Set1",
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        scale_shape_manual(
          values = c(4, 6, 5, 0, 2, 1),
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        labs(
          title = "Locations of Police Killings in Northeast",
          color = "Race of\nVictim",
          shape = "Race of\nVictim",
          caption = "Each shape represents one victim"
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
    }
  })
  
  # create a reactive object changing region label to a vector of state abbreviations
  region <- reactive({switch(input$region,
                   "West" = c("CA", "OR", "WA", "NV", "UT", "CO", "MT", "ID", "WY"),
                   "Southwest" = c("AZ", "NM", "TX", "OK"),
                   "Southeast" = c("AR", "LA", "MS", "AL", "FL", "TN", "KY", "GA", "SC", "NC", "VA", "WV"),
                   "Midwest" = c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH"),
                   "Northeast" = c("NY", "PA", "MD", "DC", "DE", "NJ", "CT", "MA", "NH", "VT", "ME"))
    })
  
  # create select bar input of cities, based on the region selected
  output$cities <- renderUI({
    cities <- police %>% 
      filter(state %in% region()) %>% 
      distinct(city)
    
    cities <- cities$city
    
    selectInput("city_names", 
                label = "Select a city within the region choosen above to view information about
                victim(s) in that city:",
                choices = cities)
  })
  
  # create table of information about cities
  output$city_dat <- renderTable({
    police %>% 
      filter(city == input$city_names) %>% 
      select(name, city, state, month, day, year, age, gender, raceethnicity)
  })
  
  
  # create data-set of counts of levels of gender
  # for labels on gender bargraph
  gender_counts <- police %>% 
    count(gender)
  
  # create data-set of counts of levels of raceethnicity
  # for labels on race bargraph
  race_counts <- police %>% 
    count(raceethnicity)
  
  # Create Bargraph Output
  output$bargraph <- renderPlot({
    
    # Code for graph if input$var is "Race"
    if(input$var == "Race"){
      # graph
      ggplot(police, mapping = aes(fct_rev(fct_infreq(raceethnicity)), fill = raceethnicity)) +
        # get rid of legend
        geom_bar(show.legend = FALSE) +
        # flit axes
        coord_flip() +
        # add title, change x-axis label
        labs(
          x = "Race",
          title = "Race of Victim"
        ) +
        # change fill colors of graph
        scale_fill_manual(
          values = c("White" = "dark red", 
                     "Black" = "black", 
                     "Hispanic/Latino" = "black", 
                     "Unknown" = "black",  
                     "Asian/Pacific Islander" = "black",
                     "Native American" = "black")
        ) +
        # add count labels for each bar
        geom_label(data = race_counts, aes(label = n, y = n),
                  # move labels down five units
                  nudge_y = -3,
                  # change size
                  size = 4,
                  # change color
                  color = "white",
                  show.legend = FALSE) +
        # add minimal theme
        theme_minimal()  +
        # edit size, face and position of title
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = .5)
        )
    # Code for Graph if input$var is "Gender"
    } else {
      # graph
      ggplot(police, mapping = aes(fct_rev(fct_infreq(gender)), fill = gender)) +
        # get rid of legend
        geom_bar(show.legend = FALSE) +
        # flip axes
        coord_flip() +
        # add title, change x-axis title
        labs(
          x = "Gender",
          title = "Gender of Victim"
        ) +
        # change fill colors of bars
        scale_fill_manual(
          values = c("Male" = "#000080",
                     "Female" = "#de6fa1")
        ) +
        # add labels of counts to each bar
        geom_label(data = gender_counts, aes(label = n, y = n),
                  # move labels down five units
                  nudge_y = -9.5,
                  # change size
                  size = 4,
                  # change color
                  color = "white",
                  show.legend = FALSE) +
        # add minimal theme
        theme_minimal()  +
        # edit size, face and position of title
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = .5)
        )
    }
  })
  
  # Create histogram output
  output$histogram <- renderPlot({
    # create a vector of values for age
    x <- police %>% 
      filter(!is.na(age)) %>% 
      pull(age)
    
    # set the breaks of the bins based on the range of 
    # age and the input value of bins
    bin_breaks <- seq(min(x), max(x), length.out = input$bins + 1)
    
    ggplot(police, aes(age)) +
      # create histogram, set input$bins to bins = 
      # change color and fill
      geom_histogram(breaks = bin_breaks, 
                     color = "white",
                     fill = "#aa6c39") +
      # add title and change axis labels
      labs(
        x = "Age",
        y = "Count",
        title = "Age Range of Victims"
      ) +
      # add minimal theme
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = .5)
      )
  })
  
  # create reactive object to map radio button options to variables in police
  size <- reactive({
    switch(input$census_var,
           "Percent non-Hispanic White" = police$share_white,
           "Percent Black" = police$share_black,
           "Percent Hispanic/Latinx" = police$share_hispanic
    )
  })
  
  # create reactive object to map radio button options to fill colors
  fill_color <- reactive({
    switch(input$census_var,
           "Percent non-Hispanic White" = "#630436",
           "Percent Black" = "#228B22",
           "Percent Hispanic/Latinx" = "#00316E"
    )
  })
  
  # create reactive object to map radio button options to legend titles
  legend_title <- reactive({
    switch(input$census_var,
           "Percent non-Hispanic White" = "% of victim's\ncensus tract\npopulation that is non-Hispanic White",
           "Percent Black" = "% of victim's\ncensus tract\npop that is Black",
           "Percent Hispanic/Latinx" = "% of victim's\ncensus tract\npop that is Hispanic/Latinx"
    )
  })
  
  # Create scatterplot output
  output$scatterplot <- renderPlot({
    
    # create graph
    ggplot(police, aes(x = h_income, y = college, size = size())) +
      # add points, changing shape, color and alpha,
      # mapping a different color to fill based on share_ variable used
      geom_point(shape = 21, color = "white", fill = fill_color(), alpha = 0.5) +
      # add minimal theme
      theme_minimal() +
      # change axis labels, add title
      labs(
        title = "Percentage of College Graduates\nby Median Household Income of Victim's Census Tract",
        x = "median household income of census tract",
        y = "% of 25+ population in\ncensus tract with BA or higher"
      ) +
      # change x-axis scale
      scale_x_continuous(
        labels = comma,
        breaks = breaks_width(25000)
      ) +
      # edit legend title
      scale_size(
        name = legend_title()
      ) +
      # edit elements of title
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      )
  })

}

# Run the application --------------------------------------
shinyApp(ui = ui, server = server)