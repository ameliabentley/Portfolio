library(dbplyr)
library(dplyr)
library(ggplot2)
library(shiny)

athlete_events <- read.csv("athlete_events.csv") #loading the data
#unique_medals <- athlete_events %>% #to make sure i just had one mdeal per event, doing so changed the rows of data from 271,116 to 271,106
  #filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%      
  #distinct(Event, NOC, .keep_all = TRUE) %>%               
 # count(NOC, Medal, name = "Total")
colours <- c("Gold" = "#FFD700", "Bronze" = "#CD7F32", "Silver" = "#C0C0C0")#choose pretty colours to match the medels 
min_year <- min(athlete_events$Year)
max_year <- max(athlete_events$Year)#did this because i didnt know the min year i knew it ended in 2016 but just thought i would find the min and max this way to keep it the same 

# for the scatter plot, figfuring out the number of athletes vs the number of events 
noc <- unique(athlete_events$NOC)
athlete_event_summary <- athlete_events %>%
  group_by(Year) %>%
  summarise(
    athelets = n_distinct(ID),
    events = n_distinct(Event)
  )

Sex <- c("M", "F") #creating a variable for sex in a vector althlugh i couldnt get the graphs to split i could make the graphs still

Sports <- unique(athlete_events$Sport)#find all the unique sports 

# Print the vector to check
print(sports_vector)
#i couldnt quite get q2.d to work, i was able to filter the data by sex to make individual graphs 
#but when i tried to modify my plots to make them side by side it kept not working and not running so i decided to leave it 
ui <- fluidPage(
  titlePanel("Olympic Medal Statistics"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Stacked" = "stacked", "Side by Side" = "side_by_side")),
      selectInput("sex", "Select sex of athelete:",
                  choices = c("Male" = "Male", "Female" = "Female", "Both" = "Both")),
      checkboxGroupInput("sport", "select a sport:", 
                         choices = Sports, 
                         selected = Sports),
      checkboxGroupInput("NOC", "select a noc:", 
                         choices = noc, 
                         selected = noc),
      sliderInput("years", "Choose years", sep="",
                  min = min_year, max = max_year, value= c(1896,2016))
    ),
    mainPanel(
      h3("Medal stacked bar chart"),
      plotOutput("Plot"),
      h3("Athletes vs Events Scatter Plot"),
      plotOutput("scatterPlot")
    )
  )
) 

server <- function(input, output) {
  #putting in all the filters for the side bar
  df.selection <- reactive({
    filter_data <- athlete_events %>%
      filter(Sport %in% input$sport) %>%
      filter(NOC %in% input$NOC)%>%
      filter(Year >= input$years[1], Year <= input$years[2])
    #filtering by sex
    if (input$sex != "Both") {
      selected_sex <- if (input$sex == "Male") "M" else "F"
      filter_data <- filter_data %>%
        filter(Sex == selected_sex) 
    }
    
    filter_data  
  })
  
 unique_medals <- reactive({
    df.selection() %>%
      filter(Medal %in% c("Gold", "Silver", "Bronze")) %>%
      distinct(Event, NOC, .keep_all = TRUE) %>%
      count(NOC, Medal, name = "Total")
  })
  
  athlete_event_summary <- reactive({
    df.selection() %>%
      group_by(Year) %>%
      summarise(
        Number_of_Athletes = n_distinct(ID),
        Number_of_Events = n_distinct(Event)
      )
  })
  #creating the bar chard
  output$Plot <- renderPlot({
    ggplot(unique_medals(), aes(x = NOC, y = Total, fill = Medal)) +
      geom_bar(stat = "identity", position = if (input$plotType == "stacked") "stack" else "dodge") +
      scale_fill_manual(values = colours) +
      labs(title = "Medals by NOC",
           x = "NOC",
           y = "Number of Medals") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  })
  #creating the scatter plot 
  output$scatterPlot <- renderPlot({
    ggplot(athlete_event_summary(), aes(x = Number_of_Athletes, y = Number_of_Events)) +
      geom_point() +
      labs(title = "Number of Athletes vs Number of Events Across All Games",
           x = "Number of Athletes",
           y = "Number of Events") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

