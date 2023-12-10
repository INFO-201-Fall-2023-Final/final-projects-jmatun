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

# Creating your UI -------------------------------------------------------------
#
# Part of this UI is created for you. You need to add elements to make your UI
# match the example provided in the instruction. 
#
#-------------------------------------------------------------------------------

page_one <-tabPanel(
  "Introduction", 
  titlePanel("Intro Page Title"),
  
  br(), 
  
  p("Intro, explain what we're trying to tell"),
  br(),
)

page_three <- tabPanel(
  "Weather in New York",
  mainPanel(
    #h1("Intro"),
    HTML("<b>How does weather vary by season and why does it matter?</b>"),
    p("Rough Draft, explain the weather trends in New York and how certain seasons are rainier/snowier leading to more accidents")
  ),
  sidebarLayout(
    mainPanel(
      htmlOutput(outputId = "weather_title"),
      plotlyOutput(outputId = "weather")
    ),
    sidebarPanel(
      h3("View Weather by Season"),
      selectInput(inputId = "chosen_season_weather", label = "Choose a Season", choices = c("Fall", "Spring", "Summer", "Winter")),
      htmlOutput(outputId = "as"),
    )
  )
)


page_two <-tabPanel(
  "Accidents in New York", 
  p("Explain the dangers of how weather affects accidents (ie slippery roads, hard to see etc...)"),
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
  )
)

page_four <- tabPanel(
  "Analysis",
  h1("The Dangerous Effects of Weather on Driving"),
  p("Analysis, and one more interactive graph (what can we compare and show?)")
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
      return(HTML(paste("With a <b>total of", sum(summary$total_crashes), "</b> accidents over 4 months and 4 seasons, <b>Spring</b> is seen to be the most dangerous with <b>9282</b> accidents.")))
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
      scale_fill_manual(values = c("gray", "black", "white"))
    return(p)
  })
  
  output$weather_title <- renderUI({
    return(h2(paste(input$chosen_season_weather, "Weather Frequency")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

