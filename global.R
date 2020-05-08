# This script is a front-end for shiny app that implements the control chart approach for exponentially growing data
# series by Provost et al. March 2020.
# Drafted by Kevin Little, Ph.D. with help from Mason DeCamillis, Lynda Finn, and Emily Jones

library(tidyverse)
library(readxl)
library(utils)
library(httr)
library(DT)
source("helper.R")

data_file_country <- paste0('data/country_data_', as.character(Sys.Date()), '.csv')
data_file_state   <- paste0('data/us_state_data_', as.character(Sys.Date()), '.csv')
#data_file_state <- paste0('data/us_state_data_', '2020-04-21.csv')

defStartdate <- NA
defBuffer <- 7
#defBaseline is the default value of points to use to compute the exponential growth control limits
defBaseline <- 20

data_choices <- character(0)
data_selected <- NULL
location_choices <- character(0)
location_selected <- NULL

if (!file.exists(data_file_country)) {
  response_country <- try(httr::GET(url = "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                                    authenticate(':', ':', type='ntlm'),
                                    config(timeout = 10),
                                    write_disk(data_file_country, overwrite = TRUE)))
}

if (file.exists(data_file_country)) {
  df_country <- read_csv(data_file_country)
  df_country$dateRep <- as.Date(df_country$dateRep, format = '%d/%m/%Y')
  country_names <- unique(df_country$countriesAndTerritories)
  
  data_choices <- c('Country-level ECDC data', data_choices)
  data_selected <- 'Country-level ECDC data'
  location_choices <- sort(country_names)
  location_selected <- country_names[1]
}

if (!file.exists(data_file_state)) {
  response_state <- try(httr::GET(url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
                                  authenticate(':', ':', type='ntlm'),
                                  config(timeout = 10),
                                  write_disk(data_file_state, overwrite = TRUE)))
}

if (file.exists(data_file_state)) {
  df_state <- read_csv(data_file_state)
  #problems opening the NYT connection 4/4/2020.  Also, native date format is %Y-%m-%d  Manual file manip changes date format.
  df_state$date <- as.Date(df_state$date,format='%m/%d/%Y')
  state_names <- unique(df_state$state)
  #rename state variable to countriesAndTerritories to keep code consistent with nations data set
  colnames(df_state) <- c('dateRep', 'countriesAndTerritories', 'fips', 'cum_cases', 'cum_deaths')
  #compute deaths in the state table, reported are cum deaths--have to work by state
  df_state <- df_state %>%group_by(countriesAndTerritories) %>% 
    mutate(deaths = make_vec(cum_deaths)) %>% mutate(cases = make_vec(cum_cases))
           
  #df_state <- df_state %>% group_by(countriesAndTerritories) %>% 
                    
  
  data_choices <- c('US state-level NY Times data', data_choices)
  data_selected <- 'US state-level NY Times data'
  location_choices <- sort(state_names)
  location_selected <- 'New York'
}

# delete old country/state data files that aren't necessary - not a big deal locally,
# and they're excluded from the git repository, but we don't want to keep uploading
# old data files to shinyapps
data_files <- list.files(
  path = 'data',
  pattern = '(country|us_state)_.+\\.csv',
  full.names = TRUE)

data_files_remove <- setdiff(data_files, c(data_file_country, data_file_state))

file.remove(data_files_remove)

#define messages used in output and to check for conditional branching in calculations and plotting
use_raw_table_messages <- c('Series too short to create a c-chart',
                            'c-chart only',
                            'c-chart plus values after initial signal',
                            'c-chart plus values after initial signal, no sign of exponential growth')

use_new_expo_table_messages <- c('c-chart and exponential fit')
