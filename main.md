SARS-nCOV-19
================
MK
16 03 2020

## Params

``` r
url_conf <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_dead <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
lookup <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv"
MIN_VALUE <- 8000
MIN_VALUE_DEATHS <- 750
MIN_DATE_TESTS <- "2020-03-10"
MIN_DATE <- "2020-03-25"
TODAY    <- "2020-04-06"
TAKE_LOG <- TRUE
```

## Calc

``` r
START_CASES_NO = 1
MIN_CASES = MIN_VALUE
MAX_CASES = Inf
CHART_MAX = 10000
countries_include = c("Poland")
dead = FALSE
MA = 3

build_data <- function(START_CASES_NO = 1, MIN_CASES = MIN_VALUE, MAX_CASES = Inf, CHART_MAX = 10000, countries_include = c("Poland"), dead=FALSE, cumulative = TRUE, MA = 3){

  population <- fread("API_SP.POP.TOTL_DS2_en_csv_v2_866861.csv", header = TRUE) %>% 
    select(`Country Name`, `2018`) %>% 
    rename(country = `Country Name`, pop = `2018`) %>%
    mutate(pop = pop/1000000)

  land  <- fread("API_AG.LND.TOTL.K2_DS2_en_csv_v2_888929.csv", header = TRUE) %>% 
    select(`Country Name`, `2018`) %>% 
    rename(country = `Country Name`, land = `2018`)
  
  data_conf <- 
    fread(ifelse(dead,url_dead,url_conf)) %>% 
    melt(., id.vars = c(1,2,3,4)) %>% 
    mutate (date = as.Date(variable, format = "%m/%d/%y")) %>% 
    select(-variable) %>% 
    rename(country = `Country/Region`, province = `Province/State`) %>% 
    group_by(country, date) %>% 
    summarize(value = sum(value)) %>%
    mutate(value_new = value - lag(value)) %>%
    mutate(value_ma = rollapply(value,MA,mean,align='right',fill=NA)) %>%
    mutate(value_ma_new = rollapply(value_new,MA,mean,align='right',fill=NA))
  
  conf_first <- 
    data_conf %>% 
    filter(value > START_CASES_NO) %>% 
    group_by(country) %>% 
    mutate(maxv = max(value)) %>% 
    top_n(-1, wt = date) %>% 
    select(-value, -value_new, -value_ma, -value_ma_new) %>% 
    rename(date_first = date)
  
  tests <- fread("Tests.csv", header = TRUE) %>%
    mutate(date = as.Date(date)) %>%
    mutate(
      country = if_else(country == "Czech Republic", "Czechia",country),
      country = if_else(country == "Iran (Islamic Republic of)", "Iran",country),
      country = if_else(country == "Republic of Korea", "Korea, South",country),
      country = if_else(country == "USA", "US",country),
      country = if_else(country == "Mainland China", "China",country)
    ) %>%
    select(country,date,new_tests,tests_cumulative) %>%
    group_by(country) %>%
    filter(new_tests > 0) %>%
    mutate(test_latest = ifelse(date == max(date), 1, 0)) %>%
    ungroup(.) %>%
    rename(tests = tests_cumulative) %>%
    rename(tests_new = new_tests) %>%
    mutate(tests_ma = rollapply(tests,MA,mean,align='right',fill=NA)) %>%
    mutate(tests_ma_new = rollapply(tests_new,MA,mean,align='right',fill=NA))
  
  conf <- 
    data_conf %>% 
    left_join(., conf_first, by = "country") %>% 
    mutate(days = date-date_first) %>% 
    filter(days >=0) %>% 
    left_join(., population) %>% 
    mutate(value_per_1M = value/pop) %>%
    mutate(value_per_1M_new = value_new/pop) %>%
    mutate(value_ma_per_1M = value_ma/pop) %>%
    mutate(value_ma_per_1M_new = value_ma_new/pop) %>%
    left_join(., tests) %>%
    mutate(tests_per_1M = tests/pop) %>%
    mutate(tests_per_1M_new = tests_new/pop) %>%
    mutate(tests_ma_per_1M = tests_ma/pop) %>%
    mutate(tests_ma_per_1M_new = tests_ma_new/pop) %>%
    mutate(cases_per_test = value/tests) %>%
    mutate(cases_per_test_new = value_new/tests_new) %>%    
    mutate(cases_ma_per_test = rollapply(value/tests,MA,mean,align='right',fill=NA)) %>%
    mutate(cases_ma_per_test_new = rollapply(value_new/tests_new,MA,mean,align='right',fill=NA) ) %>%        
    left_join(., land) %>%
    mutate(pop_density = pop/land) %>%
    select(-date_first, -pop)

  conf_filtered <- 
    conf %>% 
    filter((maxv > MIN_CASES & maxv < MAX_CASES & country != "China") | country %in% countries_include) %>%
    filter(value < CHART_MAX) %>%
    mutate(maxv = ifelse(value == maxv, 1, 0))
  
  return(conf_filtered)
}
```

## Plots

### 1\. Log(number of cases)

![](main_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

![](main_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### 2\. Nominal number of cases

Removed.

### 3\. Nominal number of cases per 1M inhabitants

![](main_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

![](main_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### 4\. Other hypothesis

Hipoteza: im większa gęstość zaludnienia, tym szybciej powinien krążyć
wirus.

### 5\. Tests

Idea:
<https://ourworldindata.org/grapher/tests-vs-confirmed-cases-covid-19-per-million>

![](main_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

![](main_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

### 6\. Forecast

Removed.

### 7\. Deaths (cumulative)

![](main_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

![](main_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### 8\. Deaths

![](main_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

![](main_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
