library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
#library(testthat)

#source("DataWrangling.R") #loads in your analysis file

# Overview ---------------------------------------------------------------------

# Homework 5: Examining Fight Song Lyrics - Shiny App
# Before you begin, make sure you read and understand the assignment description
# on canvas first! This assignment will not make sense otherwise. 
# For each question/prompt, write the necessary code to create your shiny app. 
# Make sure you have written AND TESTED all of your code in analysis.R before 
# beginning this part of the assignment. 

df <- read.csv("combinedFrame.csv") #DO NOT CHANGE!
summary <- read.csv("summary.csv")
accident_weather_summary <- read.csv("seasonSummary.csv")
weather_summary <- read.csv("weatherSummary.csv")
cause_summary <- read.csv("causeSummary.csv")

# Creating your UI -------------------------------------------------------------
#
# Part of this UI is created for you. You need to add elements to make your UI
# match the example provided in the instruction. 
#
#-------------------------------------------------------------------------------

page_one <-tabPanel(
  "Introduction", 
  img(src = 'rainy-driving.jpg', style = 'width: 80%;'),
  titlePanel("Does Weather Really Affect Driving?"),
  mainPanel(
    p("With the combination of these data sets, we aim to discuss the weather patterns and car accidents combined. This website is supposed to be educational and help drivers stay safe through all weather conditions. Whether it depends on the car model, or what type of weather conditions are occurring, it is always helpful to understand how to practice defensive and safe driving. With these particular data sets, we are discussing New York weather, with going in detail of the amount of “bad” weather in each season  and how often car accidents happen with different weather conditions. ")
  ),
)

page_two <-tabPanel(
  "Accidents in New York", 
  sidebarLayout(
    mainPanel(
      htmlOutput(outputId = "title"),
      plotlyOutput(outputId = "accidents")
    ),
    sidebarPanel(
      h3("View it by Weather"),
      selectInput(inputId = "chosen_season", label = "Choose a Forecast", choices = c("Overview", "Rain", "Snow", "Clear")),
      htmlOutput(outputId = "accident_summary"),
    )
  ),
  h2("The Effect of Seasons"),
  p("With this graph, we can tell that winter has the worst accident rates. This is due to slippery roads, low visibility among the large amount of reasons why these season are the worst. In Spring and Summer, there is a large amount of rain which leads to a lot of slippery roads. Drivers should be more careful to drive the speed limit and steer clear of reckless driving in order to practice safe driving in these conditions. With a large amount of rainfall, the data shows that rainfall in seasons are dangerous. Furthermore, in winter there is a lot of snow and ice, again making slippery road conditions. Often times in this type of weather it is important to drive slower than the speed limit and be careful to watch out for black ice. "),
)

page_three <- tabPanel(
  "Weather in New York",
  sidebarLayout(
    mainPanel(
      htmlOutput(outputId = "weather_title"),
      plotlyOutput(outputId = "weather")
    ),
    sidebarPanel(
      h3("View Weather by Season"),
      selectInput(inputId = "chosen_season_weather", label = "Choose a Season", choices = c("Fall", "Spring", "Summer", "Winter")),
      htmlOutput(outputId = "weather_explanation"),
    )
  ),
  mainPanel(
    h2("How does weather vary by season and why does it matter?"),
    p("Fall weather according to this graph indicates that rain and clear skies are about equal to occur during the fall season. This could be why Fall sees the least amount of accidents because of the clear weather, but clear skies can still cause accidents because of the cloudy skies. Cloudy skies can be difficult to drive in due to brightness when you do not expect it: spring weather is mostly rainy, which is expected. Due to a lot of rainfall, drivers need to make sure their windshield wipers are working properly and their tires are prepared for slicker conditions. In winter, it is also mainly rainy, so the above rainy conditions tips apply. However, there is also a bit of snow, which again, drivers need to be more careful than ever, and have the proper tools to get snow off of their windshield and know how to clear the fog in the car when you get in initially. In summer, there is still more rain, hence the rainy conditions tips apply again. ")
  ),
)

page_four <- tabPanel(
  "Analysis",
  h1("The Dangerous Effects of Weather on Driving"),
  sidebarLayout(
    mainPanel(
      htmlOutput(outputId = "title_cause"),
      plotlyOutput(outputId = "cause")
    ),
    sidebarPanel(
      h3("View it by Weather"),
      selectInput(inputId = "season_cause", label = "Choose a Season", choices = c("Fall", "Spring", "Summer", "Winter")),
      selectInput(inputId = "cause_cause", label = "Choose a Cause", choices = c(
      "Driver Inattention/Distraction",
      "Following Too Closely",
      "Failure to Yield Right-of-Way",
      "Driver Inexperience",
      "Pavement Slippery")),
      htmlOutput(outputId = "cause_explanation"),
    )
  ),
  h2("Pay Attention!"),
  p("If it's raining in the summer, accidents tend to happen more frequently. Most likely, this is a result of drivers not anticipating summertime rain. Since summers are frequently extremely hot and sunny, drivers are less experienced in driving in the rain than in clear weather. In addition, summertime tends to make people more carefree and distracted due to the heat. A common cause of accidents in the winter is drivers following too closely behind the person in front of them. This is most likely a result of drivers trying to avoid accidents by moving faster than they should because of the slicker roads. Instead, they should be driving more cautiously and slowly. Spring weather is similar to winter weather because of the heavy rains that cause the pavement to become slick. Drivers must be more cautious when attempting to speed and mindful of other people's driving styles. People must drive defensively and with greater caution. "),
)


ui <-navbarPage(
  "Dangerous Driving Seasons",
  page_one,         
  page_two,         
  page_three,
  page_four
)


# Creating your Server ---------------------------------------------------------
#
# In this section you will add the logic to make your UI fully interactive. Make
# sure you name all your input and output elements correctly! 
#
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
  output$title <- renderUI({
    return(h2(paste("Accidents in New York by Weather -", input$chosen_season)))
  })
  
  
  output$accidents <- renderPlotly({
    if (input$chosen_season == "Overview") {
      p <-  ggplot(accident_weather_summary, aes(x = season, y = total_other + total_rain + total_snow, fill = season)) +
        geom_col() +
        labs(x = "Season", y = "Total Accidents") +
        coord_cartesian(ylim = c(0, 9336)) +
        scale_fill_manual(values = c("#031c4a", "#0640ac", "#216cf7", "#84aefb")) 
    } else if (input$chosen_season == "Rain") {
      p <-  ggplot(accident_weather_summary, aes(x = season, y = total_rain, fill = season)) +
        geom_col() +
        labs(x = "Season", y = "Total Accidents") +
        coord_cartesian(ylim = c(0, 9336)) +
        scale_fill_manual(values = c("#031c4a", "#0640ac", "#216cf7", "#84aefb"))
    } else if (input$chosen_season == "Snow") {
      p <-  ggplot(accident_weather_summary, aes(x = season, y = total_snow, fill = season)) +
        geom_col() +
        labs(x = "Season", y = "Total Accidents") +
        coord_cartesian(ylim = c(0, 9336)) +
        scale_fill_manual(values = c("#031c4a", "#0640ac", "#216cf7", "#84aefb"))
    } else if (input$chosen_season == "Clear") {
      p <-  ggplot(accident_weather_summary, aes(x = season, y = total_other, fill = season)) +
        geom_col() +
        labs(x = "Season", y = "Total Accidents") +
        coord_cartesian(ylim = c(0, 9336)) +
        scale_fill_manual(values = c("#031c4a", "#0640ac", "#216cf7", "#84aefb"))
    } 
  })
  
  output$accident_summary <- renderUI({
    if (input$chosen_season == "Overview") {
      return(HTML(paste("With a <b>total of", sum(summary$total_crashes), "</b> accidents over 4 months and 4 seasons, <b>Winter</b> is seen to be the most dangerous with <b>9072</b> accidents.")))
    } else if (input$chosen_season == "Rain") {
      return(HTML(paste("Out of ", sum(summary$total_crashes), " total accidents, <b>", sum(accident_weather_summary$total_rain), "</b> accidents happened in the rain")))
    } else if (input$chosen_season == "Snow") {
      return(HTML(paste("Out of ", sum(summary$total_crashes), " total accidents, <b>", sum(accident_weather_summary$total_snow), "</b> accidents happened in the snow")))
    } else {
      return(HTML(paste("Out of ", sum(summary$total_crashes), " total accidents, <b>", sum(accident_weather_summary$total_other), "</b> accidents happened in clear weather")))
    }
  })
  
  output$weather <- renderPlotly({
    p <-  ggplot(filter(weather_summary, season == input$chosen_season_weather), aes(x = weather, y = percentage, fill = weather)) +
      geom_col() +
      labs(x = "Weather Type", y = "Frequency of Weather (%)") +
      coord_cartesian(ylim = c(0, 100)) +
      scale_fill_manual(values = c("#294312", "#7cc837", "#d3edbc")) 
    return(p)
  })
  
  output$weather_title <- renderUI({
    return(h2(paste(input$chosen_season_weather, "Weather Frequency")))
  })
  
  output$weather_explanation <- renderUI({
    if (input$chosen_season_weather == "Fall") {
      return(HTML("Fall is the least rainy season, with the weather distrubted as only <b>52% rain</b>, and the other <b>48% clear </b>"))
    } else if (input$chosen_season_weather == "Spring") {
      return(HTML("Spring is a moderate rain season, with the weather distrubted with <b>68% rain</b>, and only <b>32% clear </b> days"))
    } else if (input$chosen_season_weather == "Summer") {
      return(HTML("Summer is a heavy rain season, with the weather distrubted with <b>80% rain</b>, and only <b>20% clear </b> days"))
    } else {
      return(HTML("Winter is the least clear season, with the weather distrubted with only <b>16% clear </b> days, <b>81% rain</b>, and <b>3% snow </b> days"))
    }
  })
  
  output$cause_explanation <- renderUI({
    if (input$cause_cause == "Driver Inattention/Distraction") {
      return(HTML(paste(input$cause_cause, "is a problem that becomes amplified in the rain, because of the conditions that gives drivers less time to react and already reduced visibility of the roads")))
    } else if (input$cause_cause == "Following Too Closely") {
      return(HTML(paste(input$cause_cause, "becomes a problem in the rain, because stopping distances become even shorter, putting tailgaters more at risk for accidents")))
    } else if (input$cause_cause == "Failure to Yield Right-of-Way") {
      return(HTML(paste(input$cause_cause, "becomes a problem in the rain, because visibility is reduced, making it harder for drivers to see clearly and make the correct decisions")))
    } else if (input$cause_cause == "Driver Inexperience") {
      return(HTML(paste(input$cause_cause, "becomes a problem in the rain, because it amplifies the stress and increases the already hard and new complexity of driving for them")))
    } else {
      return(HTML(paste(input$cause_cause, "becomes a problem in the rain, because rain is more likely to make surfaces wet and slippery, putting more hazards on the roads and putting drivers and risk of hydroplanning and going out of control more")))
    }
  })
  
  output$cause <- renderPlotly({
    p <-  ggplot(filter(filter(cause_summary, CONTRIBUTING.FACTOR.VEHICLE.1 == input$cause_cause), season == input$season_cause), aes(x = weather, y = count, fill = weather)) +
      geom_col() +
      labs(x = "Weather Type", y = "Total Accidents") +
      coord_cartesian(ylim = c(0, 1800)) +
      scale_fill_manual(values = c("#550d00", "#ff2800", "#ffb7aa"))
    return(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

