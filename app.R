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
# install.packages
library(thematic)
library(showtext)

# Source Data Processing Script --------------------------------------
source(file = "data_processing.R")

# Define UI for application that draws a histogram --------------------------
ui <- fluidPage(
  # Add theme
  theme = bslib::bs_theme(bootswatch = "simplex"),
  
  # Add Title
  titlePanel("Murders by the Police in the United States (January-June 2015)"),
  
  # Add Main Interface
  sidebarLayout(
    # Sidebar for Introduction and Data Citation
    sidebarPanel(
      h2("Introduction"),
      p("Police brutality and violence has, unfortunately, been a persistent problem in contemporary America. In the first 6 months of 
        2015 alone",
        em("467"),
        "people were killed at the hands of police officers. This app provides
        detailed graphics and tables about those who were killed by police between January
        and June of 2015 in an attempt to raise awareness about police violence, 
        and bring the country closer to creating communities where all feel safe and welcome."),
      div("To navigate through the sections of the app, click on the labeled tabs.", style = "color:red"),
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
        tabPanel(title = "Location of Deaths",
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
        tabPanel(title = "Demographic Data",
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
        tabPanel(title = "Census Tract Data",
                 br(),
                 br(),
                 plotOutput("scatterplot"),
                 br(),
                 br(),
                 radioButtons("census_var",
                              label = "Select variable for size of points:",
                              choices = c("Percent non-Hispanic White",
                                          "Percent Black",
                                          "Percent Hispanic/Latinx")),
                 br(),
                 br(),
                 plotOutput("density")),
        tabPanel(title = "Cause of Death",
                 br(),
                 br(),
                 fluidRow(column(8, plotOutput("cause")),
                          column(4, radioButtons("fill_var",
                                                 label = "Select fill/legend variable:",
                                                 choices = c("None",
                                                             "Armed",
                                                             "Race")))))
            )
         )
      )
   )

# Define server logic required to draw a histogram  ----------------------
server <- function(input, output) {
  
  # Map Output -----
  output$map <- renderPlot({
    
    # Define plot using an if, else if, else statement
    if (input$region == "West"){
      
    # filter police_geometry (dataset with tigris geometric maping data)
    # by states in the vector
     police_geometry %>% 
        filter(state %in% c("CA", "OR", "WA", "NV", "UT", "CO", "MT", "ID", "WY")) %>% 
        # plot state boundaries
        ggplot() +
        geom_sf(aes(geometry = geometry)) +
        coord_sf() +
        # add points for each victim, mapping raceethnicity to color and shape
        geom_point(aes(x = longitude, y = latitude, color = raceethnicity, shape = raceethnicity),
                   alpha = 0.95, size = 3) +
        # void theme
        theme_void() +
        # change the color palette of the points, change order of points
        scale_color_brewer(
          palette = "Set1",
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        # manually set the shapes, change order of shapes to match points
        scale_shape_manual(
          values = c(4, 6, 5, 0, 2, 1),
          limits = c("White", "Hispanic/Latino", "Black", "Native American", "Unknown", "Asian/Pacific Islander")
        ) +
        # Change title, legend label, and add caption
        labs(
          title = "Locations of Police Killings in West",
          color = "Race of\nVictim",
          shape = "Race of\nVictim",
          caption = "Each shape represents one victim"
        ) +
        # add text for san francisco
        annotate(
          geom = "text",
          x = -123.5, y = 35,
          label = "San\nFrancisco"
        ) +
        # add arrow for san francisco
        annotate(
          geom = "curve", 
          x = -123, y = 35, 
          xend = -122.4194, yend = 37.7749, 
          curvature = .2, 
          arrow = arrow(length = unit(2, "mm")),
          color = "black"
        ) +
        # add text for los angeles
        annotate(
          geom = "text",
          x = -121.5, y = 33,
          label = "Los\nAngeles"
        ) +
        # add arrow for los angeles
        annotate(
          geom = "curve", 
          x = -121, y = 33, 
          xend = -118.2437, yend = 34.0522, 
          curvature = .2, 
          arrow = arrow(length = unit(2, "mm")),
          color = "black"
        ) +
        # add text for denver
        annotate(
          geom = "text",
          x = -102, y = 43,
          label = "Denver"
        ) +
        # add arrow for denver
        annotate(
          geom = "curve", 
          x = -102, y = 42.7, 
          xend = -104.9903, yend = 39.7392, 
          curvature = .2, 
          arrow = arrow(length = unit(2, "mm")),
          color = "black"
        ) +
        # add text for las vegas
        annotate(
          geom = "text",
          x = -112, y = 34,
          label = "Las Vegas"
        ) +
        # add arrow for las vegas
        annotate(
          geom = "curve", 
          x = -113, y = 34, 
          xend = -115.1398, yend = 36.1699, 
          curvature = .2, 
          arrow = arrow(length = unit(2, "mm")),
          color = "black"
        ) +
        # change elements of title
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5)
        )
      
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
          caption = "Each shape represents one victim\nSouth Dakota and North Dakota absent because these states had no
          police killings in 2015"
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
          caption = "Each shape represents one victim\nVermont and Rhode Island absent because these states had no
          police killings in 2015"
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
   
  # Cities Select Bar and Table --------------
  
  # create a reactive object changing region label to a vector of state abbreviations
  region <- reactive({switch(input$region,
                   "West" = c("CA", "OR", "WA", "NV", "UT", "CO", "MT", "ID", "WY"),
                   "Southwest" = c("AZ", "NM", "TX", "OK"),
                   "Southeast" = c("AR", "LA", "MS", "AL", "FL", "TN", "KY", "GA", "SC", "NC", "VA", "WV"),
                   "Midwest" = c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH"),
                   "Northeast" = c("NY", "PA", "MD", "DC", "DE", "NJ", "CT", "MA", "NH", "VT", "ME"))
    })
  
  # create a selectInput of cities that is based on the region selected 
  # in the region selectInput using renderUI
  output$cities <- renderUI({
    
    # create cities dataset
    cities <- police %>% 
      # filter by states in the vector selected
      filter(state %in% region()) %>% 
      # get the unique city names
      distinct(city)
    
    # extract city names as a vector
    cities <- cities$city
    
    # create city selectInput
    selectInput("city_names", 
                label = "Select a city within the region choosen above to view information about
                victim(s) in that city:",
                choices = cities)
  })
  
  # create table of information about victims in cities selected
  output$city_dat <- renderTable({
    police %>% 
      # filter by city name selected
      filter(city == input$city_names) %>% 
      # keep only name, city, state, month, day, year, age, gender and raceethnicity columns
      select(name, city, state, lawenforcementagency, month, day, year, age, gender, raceethnicity)
  })
  
  # Create Race/Gender Bargraph Output ---------
  
  # create data-set of counts of levels of gender
  # for labels on gender bargraph
  gender_counts <- police %>% 
    count(gender)
  
  # create data-set of counts of levels of raceethnicity
  # for labels on race bargraph
  race_counts <- police %>% 
    count(raceethnicity)
  
  # output
  output$bargraph <- renderPlot({
    
    # Code for graph if input$var is "Race"
    if(input$var == "Race"){
      # graph
      ggplot(police, mapping = aes(fct_rev(fct_infreq(raceethnicity)))) +
        # get rid of legend
        geom_bar(aes(fill = raceethnicity), show.legend = FALSE) +
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
                  # get rid of legend
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
      ggplot(police, mapping = aes(fct_rev(fct_infreq(gender)))) +
        # get rid of legend
        geom_bar(aes(fill = gender), show.legend = FALSE) +
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
                  # get rid of legend
                  show.legend = FALSE) +
        # add minimal theme
        theme_minimal()  +
        # edit size, face and position of title
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = .5)
        )
    }
  })
  
  # Create histogram of age output ----------------
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
      scale_x_continuous(
        breaks = breaks_width(10)
      ) +
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
  
  # Census Tract Scatterplot --------------
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
           "Percent non-Hispanic White" = "% of census tract\npopulation that is non-Hispanic White",
           "Percent Black" = "% of census tract\npop that is Black",
           "Percent Hispanic/Latinx" = "% of census tract\npop that is Hispanic/Latinx"
    )
  })
  
  # output
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
        title = "Percentage of College Graduates\nby Median Household Income in Census Tract where Victim was Killed",
        x = "median household income of census tract  (dollars)",
        y = "% of 25+ population in\ncensus tract with BA or higher"
      ) +
      # change x-axis scale
      scale_x_continuous(
        labels = comma,
        breaks = breaks_width(25000)
      ) +
      scale_y_continuous(
        labels = label_percent()
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
  

  # Income by Race Density Plot ---------------------------------------------

  output$density <- renderPlot({
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
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
      ) +
      labs(
        x = "median household income of census tract (dollars)",
        fill = "Race of Victim",
        color = "Race of Victim",
        title = "Income of Census Tract in which Victim was Killed\nby Race of Victim"
      ) 
  })
  
  # Create Cause of Death Barplot-----------------------------
  
  # create data-set with counts of each level of 
  # cause for labels on bargraph
  cause_counts <- police %>% 
    count(cause)
  
  # create output
  output$cause <- renderPlot({
    if (input$fill_var == "None") {
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
    } else if (input$fill_var == "Armed"){
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
    } else {
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
    }
  })

}

# Run the application --------------------------------------
shinyApp(ui = ui, server = server)