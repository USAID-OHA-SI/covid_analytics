## PROJECT:  COVID-19 Impact Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Extract COVID-19 and Other related data from public sources
## Date:     2020-05-14
## Updated:  2020-08-25

# LIBRARIES ----------------------------------------------------

library(glamr)        # SI Utilities
library(tidyverse)    # Data wrangling
library(vroom)
library(raster)       # getData function is here
library(rasterVis)
library(sf)
library(glitr)        # SI Plotting
library(RColorBrewer) # Colors Schemes
library(lubridate)    # Date parser
library(janitor)

# GLOBAL --------------------------------------------------------

dir_data <- "Data"
dir_dataout <- "Dataout"

ncdc <- "https://covid19.ncdc.gov.ng/"
tbl <- "table#custom1"

hdx_acaps <- "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"
xfile <- "a[class='btn btn-empty btn-empty-blue hdx-btn resource-url-analytics ga-download']"

jhu_covid <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

# DATA ----------------------------------------------------------

  ## NGA COVID-19 Stats
  
  nga_covid <- extract_tbl_data(src_url = ncdc, tbl_id = tbl) 

  nga_covid <- nga_covid %>% 
    janitor::clean_names() %>% 
    mutate(
      no_of_cases_lab_confirmed = as.integer(str_replace(no_of_cases_lab_confirmed, ",","")),
      no_of_cases_on_admission = as.integer(str_replace(no_of_cases_on_admission, ",",""))
    )
  
  ## Export 
  nga_covid %>% 
    write_csv(path = paste0(dir_dataout, "/nga_ncdc_covid_data_", Sys.Date(), ".csv"), na = "")

  ## NGA Government Measures
  
  govm <- extract_excel_data(src_page = hdx_acaps, link_id = xfile, file_sheet = "Database") 
  
  nga_govm <- govm %>% 
    filter(iso == 'NGA') %>% 
    dplyr::select(
      admin_name = admin_level_name, 
      category, measure, non_compliance,
      targeted = targeted_pop_group, 
      idate = date_implemented
    ) %>% 
    dplyr::mutate(
      admin_name = ifelse(is.na(admin_name), 'Country', admin_name),
      iyear = year(ymd(idate)),
      imonth = month(ymd(idate)),
      iday = day(ymd(idate))
    ) 
    
  nga_govm %>% glimpse()
  
  # Export 
  nga_govm %>% 
    write_csv(path = paste0(dir_dataout, "/nga_acaps_gov_measures_data_", Sys.Date(), ".csv"), na = "")
  
  ## GLOBAL COVID Data
  
  df_covid <- jhu_covid %>% vroom()
  
  df_covid %>% glimpse()
  df_covid %>% View()
  
  df_covid <- df_covid %>% 
    gather(key = "rep_date", value = "cases", -c(1:4)) %>% 
    clean_names() %>% 
    mutate(
      rep_date = mdy(rep_date),
      rep_month = month(rep_date),
      rep_day = day(rep_date),
      rep_year = year(rep_date)
    ) 
  
  df_covid %>% head() %>% prinf()
  
  df_covid %>% 
    dplyr::select(-province_state) %>% 
    group_by(country_region) %>% 
    summarise_at(vars(cases, rep_month, rep_year), sum, na.rm = TRUE) %>% View()
  
  
  df_covid %>% 
    dplyr::select(-province_state) %>% 
    group_by(country_region) %>% 
    summarise_at(
      min_case = min(cases),
      max_case = max(cases),
      cases = sum(cases, na.rm = TRUE),
      n = n()
    ) %>% View()
  
  df_covid %>% 
    dplyr::select(-province_state) %>%
    add_count(country_region, rep_month) %>% 
    View()
    
  
  
  df_covid %>% 
    write_csv(path = paste0(dir_dataout, "/global_covid_data_", as.Date(Sys.Date(), "%Y%m%d"), ".csv"), na = "")
  