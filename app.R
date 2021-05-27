# Andrews Final Project: Shiny App for Police Murders in US in 2015 -------------------------------------------

# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)


# Load Data ---------------------------------------------------------------
police <- read_csv("data/police_killings.csv") %>% 
  mutate(age = as.numeric(age),
         share_white = as.numeric(share_white),
         share_black = as.numeric(share_black),
         share_hispanic = as.numeric(share_hispanic))


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
                 sliderInput("state",
                             label = "Select map to view",
                             choices = state_name_widget,
                             value = "Iowa"
                 ),
                 plotOutput("map")),
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
  
  # Create scatterplot output
  output$scatterplot <- renderPlot({
    
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
             "Percent non-Hispanic White" = "#228B22",
             "Percent Black" = "#630436",
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