#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("COVID 19 Metrics & Charts"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("MIN_CASES_CUM",  "Min country cases, total", min = 1, max = 10000000, value = 1000000),
            sliderInput("MIN_DEATHS_CUM", "Min country deaths, total", min = 1, max = 100000, value = 300000),
            dateInput("MIN_DATE", "Start date", min = "2020-03-25", max = today()),
            checkboxInput("TAKE_LOG","Log data?", value = TRUE),
            checkboxInput("pc","Per Capita?", value = TRUE),
            radioButtons("dead","Dead or cases", choices = list("Dead" = TRUE, "Cases" = FALSE), selected = FALSE),
            radioButtons("cumulative","Cumulative or daily", choices = list("Cumulative" = TRUE, "Daily" = FALSE), selected = TRUE),
            sliderInput("START_CASES_NO", "N: start cases", min = 1, max = 100000, value = 1),
            sliderInput("START_DEATHS_NO", "N: start deaths", min = 1, max = 10000, value = 10),
            sliderInput("CHART_MAX", "Chart max", min = 1, max = 10000000, value = 10000000),
            sliderInput("MA", "Smooth - average over last ... days", min = 1, max = 28, value = 7),
            checkboxInput("dailyRateChange","dailyRateChange", value = FALSE),
            checkboxInput("legend_off","legend_off", value = TRUE)
            

            #countries_include = c("Poland", "Czechia", "Sweden", "Singapore", "Korea, South", "Switzerland", "Belgium", "Armenia", "US", "Brazil", "India", "Mexico", "United Kingdom", "Italy", "France", "Spain", "Peru", "Argentina", "Colombia", "Russia", "Israel", "Chile", "Iran")
        
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mainPlot", height = 800)
        )
    )
))
