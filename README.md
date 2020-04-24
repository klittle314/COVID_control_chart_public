# Hybrid Shewhart Chart:  Create count plots across phases of COVID-19 infection history

This project implements a method based on control charts to view phases in daily reported deaths from COVID-19. The method was developed by Lloyd Provost, Shannon Provost, Rocco Perla, Gareth Parry, and Kevin Little.  The code is R and deploys a user interface using Shiny technology.  The R code transforms a time series of daily reported deaths into charts that distinguish phases of COVID-19 infection for a reporting location like a country, state or city.   You can find a description of the method [here](http://www.ihi.org/Topics/COVID-19/Documents/IHI-COVID-19-Data-Dashboard-Introduction-and-Methodology.pdf). For an introduction to the application of the method, [here](https://www.usnews.com/news/healthiest-communities/articles/2020-03-26/coronavirus-pandemic-reaching-critical-tipping-point-in-america-analysis-shows) is an article from U.S. News and World Report.

## Who can use this project?

People who have a basic understanding of Shewhart control charts and want to apply control chart methods to characterize how reported deaths from COVID-19 change over time.  People who have skills in R can modify the code in order to load data sources to replace the built-in sources and to consider other measures, like hospitalizations or ICU cases.

## Getting Started

This document describes the R code and what you need to run it yourself.  It provides sample output for you to check.

### Prerequisites

You need a current version of R (we developed this using R version 3.6.3 and RStudio version 1.2.5033).  We used the R package ggplot2 to construct the graphs; we are exploring using plotly as an alternative.  You need familiarity with the Shiny package that builds the HTML webpages presented to the user. You need to be connected to the internet to enable update of the input data tables.

The code looks for current data in a local folder **data**; if data are not current, the code will attempt to connect to web sites to obtain current data:

```
data_file_country <- paste0('data/country_data_', as.character(Sys.Date()), '.csv')
data_file_state   <- paste0('data/us_state_data_', as.character(Sys.Date()), '.csv')


if (!file.exists(data_file_country)) {
  covid_data <- httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                          authenticate(":", ":", type="ntlm"),
                          write_disk(data_file_country, overwrite=TRUE))
}

if (!file.exists(data_file_state)) {
  download.file(url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
                destfile = data_file_state)
}
```

### Copying the code for local use

[Click here to download the latest version](https://github.com/klittle314/COVID_control_chart_app/archive/master.zip) 

Make sure you have installed the following libraries and dependencies; these are shown at the top of the global.R file.  

```
library(tidyverse)
library(readxl)
library(utils)
library(httr)
library(DT)

```
Alternatively, if you understand how repositories work, you can fork the master branch for your use.

### Structure of the Shiny app
The core files are
1. global.R  This file loads the data from external websites for country and U.S. state/territory COVID daily data.  It also does minimal editing of the data frames to assure common names.  For the U.S. state/territory file, it converts cumulative deaths into deaths reported daily.
2. ui.R  This file defines the Shiny user interface.
3. server.R  This file provides the reactive functions that take default and user-defined inputs to create summary charts and tables.
4. helper.R  This file contains the core functions.   In addition to several small auxiliary functions, the main functions are:
- find_start_date_Provost:  A function that determines dates for analysis based on data properties, along with c-chart parameters
    - Inputs:  input data frame, specified location, start date for analysis
    - Outputs: a list with date of first reported death, date of signal on c-control chart, center line for c-chart, upper control limit for c-chart 

- create_stages_Provost:  A function that will assign stage names to records in a data frame filtered by location name.
   - Inputs:  input data frame, the list of dates from find_start_date_Provost, and the baseline days used to fit the regression model of log10 deaths
   - Outputs: output data frame, with a new column that describes the stage for each record
       - stage 1:  data before the date of first reported death
       - stage 2:  data starting with date of first reported death through the day before a special cause signal on the c-control chart
       - stage 3:  data starting with the date of a special cause signal on the c-control chart
       - stage 4:  data starting after the last day used to fit fit the regression model
            
- make_location_data:  A function that calls the functions find_start_date_Provost and create_stages_Provost to create data frames for plotting.
  - Inputs:  input data frame, specified location, buffer days at end of observed data, baseline days used to fit the regression model of log10 deaths, and start date for analysis
  - Outputs:  data frame for the specified location that has dates, deaths, and stages; data frame with fitted values and limits derived from the regression model of log10 deaths; list of date of first death, date of special cause signal on c-chart, c-chart center line and upper control limit, linear model list from the regression fit.
  
 - make_charts:  A function that produces all the ggplot2 objects presented by the ui
    - Inputs:  specified location, buffer days at end of observed data, output list from function make__location_data, title for the basic graph, caption for the basic graph, logical variable to determine whether the scale of the basic chart is constrained to twice the maximum of the data.
    - Outputs:  a list containing a descriptive message about which plots are possible, a plot for the basic plot tab and a plot for the log chart page.
  
 - make_computation_table:   A function that produces the elements in the table presented on the calculation details tab
     - Inputs:  number of observations in the original data file starting with first reported death, for the specific location; the number of observations used in the regression fit; date of the first reported death; date of special cause sigal on the c-chart; the linear model list produced by the function make_location_data; the baseline days used to fit the regression model of log10 deaths.
    - Outputs:  data frame converted to an HTML table by renderDatatable.
  

### Key parameters
#### global.R
*defStartdate*:  the default start date for analysis, set to NA to have the choice box on the user interface be blank.  Start date for analysis is typically the date of first death; however, the user may over-ride this choice by entering a date in the drop-down box 'Custom start date for calculations instead of date of first death' 

*defBuffer*:  the default number of days to add to the display on the plot(s) after the most recent date in the reported death series.  Set to 7 days.

*defBaseline*:  the default number of days to use as the maximum number of records used in the exponential fit; however, the user may over-ride this choice by entering a number of days in the numeric input box 'Maximum days used to compute exponential growth line and limits'.  If the value for baseline days exceeds the available number of records, all available records are used.  Set to 20 days.

#### helper.R
function find_start_date_Provost

  *cc_length*:  set to 20; the number of records used to compute the c-chart parameters unless there is an unusually long run of zero death days after the first death. 
  
  *Australia_nudge*:  set to 5; an adjustment to the cc_length in honor of Australia which @4-12-2020 had an initial series length 25 and    then c-chart signal at the next record.  Series characterized by long strings of zeros in initial set of records.
  
  *Provost_start_count*:  set to 8; the number of deaths to observe in the death series before starting to compute the c-chart parameters.
   
  *Rule_shift_length*: set to 8; the number of consecutive values above the center line to be declared a signal of special cause on the c-chart
  
 function create_stages_Provost
  
  *min_length_chart*: set to 5; the minimum number of days with deaths > 0 to use in computing the exponential fit (linear fit based on log10(deaths).
  
### Notes on computations related to the fit of the regression line
#### Calculation of the control chart limits using residuals from linear regression on log10 deaths
The code uses the median moving range to estimate 'sigma-hat' in the calculation of the individuals control chart.  Hence the multiplier 3.14 to compute the upper and lower control limits.  The median moving range is more robust to one or two large point-to-point ranges relative to the average moving range.  Usually, use of the average moving range requires two stages:  examine moving ranges to determine if there are any that are unusually large on a chart of moving ranges; discard any ranges that exceed the upper control limit on the range chart, and recalculate the limits on the individuals chart.  We chose to use the median approach to simplify the derivation of the individuals control chart limits.

#### Use of 95% confidence interval for the slope of the regression fit
In function make_charts, we use the lower bound of the 95% confidence interval derived from the linear regression model to determine whether or not the slope is meaningfully different from zero.  If the lower bound is less than zero, we report the linear regression parameters on the calculations tab but do not show an expoential fit and exponential limits in the basic display, nor do we show the log10 chart.

## Test file

You can use the data file in the test_data folder to check the upload data function. Click [here](https://github.com/klittle314/COVID_control_chart_app/blob/master/test_data/France_test1_resort_dates.csv) to examine the test data file.  You should see screens like these if you have successfully run the Shiny app and uploaded the test file:

*Upload data*
![upload data](https://github.com/klittle314/COVID_control_chart_app/blob/master/screen_shots/2020-04-20_Data%20Load.jpg)

*Basic Chart*
![basic chart](https://github.com/klittle314/COVID_control_chart_app/blob/master/screen_shots/2020-04-20_basic%20chart.jpg)

*Log Chart*
![log chart](https://github.com/klittle314/COVID_control_chart_app/blob/master/screen_shots/2020-04-20_log%20chart.jpg)

*Calculations*
![calculation details](https://github.com/klittle314/COVID_control_chart_app/blob/master/screen_shots/2020-04-20_basic%20calculations.jpg)

## Contributing
We have not yet set up a process to incorporate changes into the code.   Check back in the future!

## Authors
Kevin Little outlined the basic design and wrote most of the core functions; Mason DeCammillis built subtantial parts of the Shiny interface; Emily Jones contributed functionality and participated with Lynda Finn to suggest enhancements to the initial design.

## Acknowledgements
Lloyd Provost, Shannon Provost, Rocco Perla, and Gareth Parry reviewed this applications and offered corrections and guidance that have improved the design and function.

## License
This project is licensed under the MIT License -- see the LICENSE.md file for details.
