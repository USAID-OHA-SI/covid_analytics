## PROJECT:  COVID-19 Impact Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Extract COVID-19 and Other related data from public sources
## Date:     2020-05-14

# LIBRARIES ----------------------------------------------------

library(glamr)        # SI Utilities
library(tidyverse)    # Data wrangling
library(raster)       # getData function is here
library(rasterVis)
library(sf)
library(glitr)        # SI Plotting
library(RColorBrewer) # Colors Schemes
library(lubridate)    # Date parser

# GLOBAL --------------------------------------------------------

dir_data <- "Data"
dir_dataout <- "Dataout"

ncdc <- "https://covid19.ncdc.gov.ng/"
tbl <- "table#custom1"

hdx_acaps <- "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"
xfile <- "a[class='btn btn-empty btn-empty-blue hdx-btn resource-url-analytics ga-download']"

# DATA ----------------------------------------------------------

  # COVID-19 Stats
  
  nga_covid <- extract_tbl_data(src_url = ncdc, tbl_id = tbl) 

  nga_covid <- nga_covid %>% 
    rename_at(vars(everything()), funs(str_replace_all(., "__", "_"))) %>% 
    mutate(
      no_of_cases_lab_confirmed = as.integer(str_replace(no_of_cases_lab_confirmed, ",","")),
      no_of_cases_on_admission = as.integer(str_replace(no_of_cases_on_admission, ",",""))
    )
  
  # Export 
  nga_covid %>% 
    write_csv(path = paste0(dir_dataout, "/nga_ncdc_covid_data.csv"), na = "")

  # Government Measures
  
  nga_govm <- extract_excel_data(src_page = hdx_acaps, link_id = xfile) %>% 
    filter(iso == 'NGA') %>% 
    dplyr::select(
      admin_name = admin_level_name, category, measure, non_compliance,
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
    write_csv(path = paste0(dir_dataout, "/nga_acaps_gov_measures_data.csv"), na = "")
  