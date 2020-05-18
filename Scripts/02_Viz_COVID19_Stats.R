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

# GLOBAL -------------------------------------------------------------

dir_data <- "Data"
dir_gis <- "GIS"
dir_graphics <- "Graphics"

ncdc <- "https://covid19.ncdc.gov.ng/"
tbl <- "table#custom1"

# FUNCTIONS ----------------------------------------------------------

  geo_plot <- function(dataset, 
                       viz_attr, 
                       viz_title, 
                       viz_save = FALSE, ...) {
    
    viz <- dataset %>% 
      ggplot(aes_string(fill = viz_attr)) +
      geom_sf(color=gray(.8), lwd=.5) +
      scale_fill_gradient2(low='yellow',  
                           high='brown', 
                           na.value = 'white') +
      geom_sf_text(aes(label=state), size = 3) +
      coord_sf() +
      labs(
        title = viz_title,
        caption = paste0('OHA/SIEI/SI, Source: NCDC as of ', Sys.Date())
      ) +
      si_style() +
      theme(
        title = element_text(margin = unit(c(1,1,5,1),'pt')),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.key.width = unit(2, "cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line(linetype = 'dashed'),
        panel.background = element_rect(fill=NULL, colour = gray(.6), size = .5)
      )
    
    print(viz)
    
    if (viz_save == TRUE) {
      ggsave(filename = paste0(dir_graphics, "/", viz_title, ".png"),
             width = 11, height = 8)
    }
    
    return(viz)
  }

# DATA ---------------------------------------------------------------
  
  ## Admin boundaries
  
  nga_adm0 <- get_geodata("NGA", 0) %>% 
    st_set_geometry(NULL)
  
  nga_adm1 <- get_geodata("NGA", 1) %>% 
    dplyr::select(state = name_1)
  
  nga_adm2 <- get_geodata("NGA", 2) %>% 
    dplyr::select(state = name_1, lga = name_2)
  
  nga_adm1 %>% 
    st_set_geometry(NULL) %>% 
    pull(state)
  
  ## Covid-19 Stats
  
  nga_covid <- extract_tbl_data(src_url = ncdc, tbl_id = tbl)
  
  nga_covid <- nga_covid %>% 
    rename_at(vars(everything()), funs(str_replace_all(., "__", "_"))) %>% 
    mutate(
      no_of_cases_lab_confirmed = as.integer(str_replace(no_of_cases_lab_confirmed, ",","")),
      no_of_cases_on_admission = as.integer(str_replace(no_of_cases_on_admission, ",",""))
    )
  
  nga_covid %>% 
    glimpse()
  
  nga_covid %>% 
    distinct(states_affected) %>% 
    pull()
  
  View(nga_covid)
  
  ## Join Covid-19 Stats to States' boundaries
  
  nga_geo <- nga_adm1 %>% 
    dplyr::mutate(state = if_else(state == 'Federal Capital Territory', 'FCT', state)) %>% 
    dplyr::left_join(nga_covid, by=c('state' = 'states_affected')) 
  
  nga_geo %>% 
    glimpse() 
  
  nga_geo %>% 
    st_set_geometry(NULL) %>% 
    View()
  
# VIZ ------------------------------------------------------------------

  nga_adm1 %>% 
    ggplot() +
    geom_sf(data=nga_adm2, fill="white", color=gray(.8), lwd=.5, lty ="dotted") +
    geom_sf(fill=NA, color=gray(.5), lwd=.5) +
    coord_sf() +
    theme_bw() +
    theme()
  
  # Generate COVID Maps
  nga_deaths <- geo_plot(nga_geo, 'no_of_deaths', 'NIGERIA - COVID-19 Related Deaths', viz_save = T)
  nga_confirmed <- geo_plot(nga_geo, 'no_of_cases_lab_confirmed', 'NIGERIA - COVID-19 Confirmed Cases', viz_save = T)
  nga_admisions <- geo_plot(nga_geo, 'no_of_cases_on_admission', 'NIGERIA - COVID-19 Cases on Admission', viz_save = T)
  nga_discharged <- geo_plot(nga_geo, 'no_discharged', 'NIGERIA - COVID-19 Discharged Cases', viz_save = T)
  
  nga_geo %>% 
    ggplot(aes(fill = no_of_deaths)) +
    geom_sf(color=gray(.8), lwd=.5) +
    scale_fill_gradient2(low='yellow',  high='brown', na.value = 'white') +
    geom_sf_text(aes(label=state), size = 3) +
    coord_sf() +
    labs(
      title = 'NIGERIA - COVID-19 Related deaths',
      caption = paste0('SIEI/SI, Source: NCDC as of ', Sys.Date())
    ) +
    si_style() +
    theme(
      title = element_text(margin = unit(c(1,1,5,1),'pt')),
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.key.width = unit(2, "cm"),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_line(linetype = 'dashed'),
      panel.background = element_rect(fill=NULL, colour = gray(.6), size = .5)
    )
  