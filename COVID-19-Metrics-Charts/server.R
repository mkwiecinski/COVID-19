#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(tidyverse)
library(broom)
library(lubridate)
library(zoo)
library(ggthemes)
#library(ftplottools)


## GLOBAL VARS

url_conf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_dead <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
url_tests<- "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/coronavirus_tests.csv"
#lookup <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"

labs_x = "Days from patient 0"
caption = "data source: github.com/CSSEGISandData/COVID-19"
subtitle = NA


## LOAD DATA 

population <- fread("API_SP.POP.TOTL_DS2_en_csv_v2_866861.csv", header = TRUE) %>% 
    select(`Country Name`, `2018`) %>% 
    rename(country = `Country Name`, pop = `2018`) %>%
    mutate(pop = pop/1000000)

land  <- fread("API_AG.LND.TOTL.K2_DS2_en_csv_v2_888929.csv", header = TRUE) %>% 
    select(`Country Name`, `2018`) %>% 
    rename(country = `Country Name`, land = `2018`)

data_dead      <- fread(url_dead)

data_conf      <- fread(url_conf)

tests_download <- fread(url_tests, header = TRUE)


## SERVER

shinyServer(function(input, output) {
    
    countries_include = c("Poland", "Czechia", "Sweden", "Singapore", "Korea, South", "Switzerland", "Belgium", "Armenia", "US", "Brazil", "India", "Mexico", "United Kingdom", "Italy", "France", "Spain", "Peru", "Argentina", "Colombia", "Russia", "Israel", "Chile", "Iran")
    
    build_data <- function(START_CASES_NO = 1, dead=FALSE, MA = 1, cumulative = TRUE, pc = TRUE, START_DEATHS_NO = 1, dailyRateChange=FALSE, tRansmission = FALSE, AR = 7){
        
        MIN_CASES_CUM <- input$MIN_CASES_CUM
        MIN_DEATHS_CUM <- input$MIN_DEATHS_CUM
        MIN_DATE <- input$MIN_DATE
        TAKE_LOG <- input$TAKE_LOG
        pc <- input$pc
        dead <- input$dead
        cumulative <- input$cumulative
        START_CASES_NO <- input$START_CASES_NO
        START_DEATHS_NO <- input$START_DEATHS_NO
        CHART_MAX <- input$CHART_MAX
        MA <- input$MA
        dailyRateChange <- input$dailyRateChange
        legend_off <- input$legend_off
        
        tests <- tests_download %>% 
            mutate(date = as.Date(date)) %>%
            mutate(
                country = ifelse(country == "Czech Republic", "Czechia",country),
                country = ifelse(country == "Iran (Islamic Republic of)", "Iran",country),
                country = ifelse(country == "Republic of Korea", "Korea, South",country),
                country = ifelse(country == "USA", "US",country),
                country = ifelse(country == "Mainland China", "China",country)
            ) %>%
            select(country,date,new_tests,tests_cumulative) %>%
            group_by(country) %>%
            filter(new_tests > 0) %>%
            mutate(test_latest = ifelse(date == max(date), 1, 0)) %>%
            ungroup(.) %>%
            rename(tests = tests_cumulative) %>%
            rename(tests_new = new_tests) %>%
            mutate(
                tests = case_when(cumulative == TRUE ~ tests * 1.0, TRUE ~ tests_new*1.0),
                tests = case_when(MA > 1 ~ rollapply(tests,MA,mean,align='right',fill=NA_real_), TRUE ~ 1.0*tests)
            )
        

        data <- if(dead) {
            data_dead
        } else {
            data_conf
        } %>%
            melt.data.table(., id.vars = c(1,2,3,4)) %>% 
            mutate (date = as.Date(variable, format = "%m/%d/%y")) %>% 
            select(-variable) %>% 
            rename(country = `Country/Region`, province = `Province/State`) %>% 
            group_by(country, date) %>% 
            summarize(value = sum(value))
        
        data_first <- 
            data %>% 
            filter((dead == FALSE & value > START_CASES_NO) | (dead == TRUE & value > START_DEATHS_NO)) %>% 
            group_by(country) %>% 
            mutate(maxv = max(value)) %>% 
            top_n(-1, wt = date) %>% 
            select(country, date, maxv) %>% 
            rename(date_first = date)
        
        data_conf <- 
            data %>% 
            left_join(., data_first, by = "country") %>% 
            mutate(is_maxv = ifelse(value == maxv, 1, 0)) %>%
            mutate(days = date-date_first) %>% 
            filter(days >=0) %>% 
            mutate(
                value = case_when(cumulative == TRUE ~ value, TRUE ~ value -dplyr::lag(value)),
            ) %>%  
            left_join(., tests) %>%
            mutate(
                cases_per_test = case_when(MA > 1 ~ rollapply(value/tests,MA,mean,align='right',fill=NA_real_), TRUE ~ 1.0*value/tests)
            ) %>% 
            
            mutate(
                value = case_when(dailyRateChange == TRUE ~ value/lag(value) - 1, TRUE ~ 1.0 * value)
            ) %>%
            
            mutate(
                value = case_when(tRansmission == TRUE ~ value/lag(value, n = AR), TRUE ~ 1.0 * value)
            ) %>%
            
            left_join(., population) %>% 
            mutate(
                value = case_when(MA > 1 ~ rollapply(value      ,MA,mean,align='right',fill=NA_real_), TRUE ~ 1.0*value),
                value = case_when(pc == TRUE ~ value/pop, TRUE ~ 1.0*value),
                tests = case_when(pc == TRUE ~ tests/pop, TRUE ~ 1.0*tests)
            ) %>%
            
            left_join(., land) %>%
            mutate(pop_density = pop/land) %>%
            select(-date_first, -pop) #%>%
            
            #left_join(., policy, by = c("country", "date")) %>%
            #mutate(policy_intro = ifelse(policy_intro == 1, 1, 0))
        
        MIN_VALUE <- if_else(dead == FALSE, MIN_CASES_CUM, MIN_DEATHS_CUM)
        
        conf_filtered <- 
            data_conf %>% 
            filter(
                (
                    maxv > MIN_VALUE 
                    & country != "China" 
                ) 
                | country %in% countries_include
            ) %>%
            filter(value < CHART_MAX)
        
        return(conf_filtered)
    }
    
    formatBack <- function(x) 10^x 
    
    plot_chart <- function(
        data,
        TAKE_LOG,
        dead = FALSE,
        labs_y, 
        labs_x = "Days from patient 0",
        title, 
        subtitle = NA,
        caption = "data source: github.com/CSSEGISandData/COVID-19", 
        legend_off = TRUE
    ){
        
        MIN_CASES_CUM <- input$MIN_CASES_CUM
        MIN_DEATHS_CUM <- input$MIN_DEATHS_CUM
        MIN_DATE <- input$MIN_DATE
        
        pc <- input$TAKE_LOG
        
        cumulative <- input$cumulative
        START_CASES_NO <- input$START_CASES_NO
        START_DEATHS_NO <- input$START_DEATHS_NO
        CHART_MAX <- input$CHART_MAX
        MA <- input$MA
        dailyRateChange <- input$dailyRateChange
        
        
        data <- data %>%
            mutate(
                value = if(TAKE_LOG) log10(value)   else value
            )
        
        if(is.na(subtitle)){
            subtitle <- paste0("countries > ", ifelse(dead, MIN_DEATHS_CUM, MIN_CASES_CUM)," + ",paste(countries_include,collapse=", "))
        }
        
        
        g <- 
            ggplot(data, aes(days, value, colour = country)) + 
            geom_point(alpha = 0.7, size = 1) + 
            geom_line(alpha = 0.2) +
            geom_text(data = data %>% filter(is_maxv == 1), aes(label = country, colour = country, x = days, y = value), hjust = -.1, size = 3) +
            #geom_point(data = data %>% filter(policy_intro == 1), aes(colour = country, x = days, y = value), alpha = 1, size = 2) +
            labs(y=labs_y, x = labs_x) +
            labs(title = title, subtitle = subtitle) +
            labs(caption = caption) +
            #scale_color_tableau(palette = "Tableau 10") +
            (if(TAKE_LOG) scale_y_continuous(labels = formatBack) ) +
            (if(legend_off) theme(legend.position="none")) 
        
        return(g)
    }
    
    
    
    output$mainPlot <- renderPlot({

        MIN_CASES_CUM <- input$MIN_CASES_CUM
        MIN_DEATHS_CUM <- input$MIN_DEATHS_CUM
        MIN_DATE <- input$MIN_DATE
        TAKE_LOG <- input$TAKE_LOG
        pc <- input$pc
        dead <- input$dead
        cumulative <- input$cumulative
        START_CASES_NO <- input$START_CASES_NO
        START_DEATHS_NO <- input$START_DEATHS_NO
        CHART_MAX <- input$CHART_MAX
        MA <- input$MA
        dailyRateChange <- input$dailyRateChange
        legend_off <- input$legend_off

        plot_chart(data = build_data(START_CASES_NO = START_CASES_NO),
                TAKE_LOG = TAKE_LOG,
                labs_y = "log10(Number of cases)",
                title = "1.1 log10(Number of cases), Day 0 = 1 case",
                )

    })

})
