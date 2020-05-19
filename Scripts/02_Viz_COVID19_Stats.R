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
library(gridExtra)
library(patchwork)
library(ggthemes)

# GLOBAL -------------------------------------------------------------

dir_data <- "Data"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_images <- "Images"

ncdc <- "https://covid19.ncdc.gov.ng/"
tbl <- "table#custom1"

# FUNCTIONS ----------------------------------------------------------
  
  # Maps
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
      ggsave(filename = paste0(dir_images, "/", viz_title, ".png"),
             width = 11, height = 8)
    }
    
    return(viz)
  }
  
  # Bar charts => NOT WORKING!
  bar_chart <- function(dataset, viz_attr) {
    
    viz <- dataset %>% 
      dplyr::filter(!is.na(!!viz_attr) & !!viz_attr != 0) %>% 
      ggplot(aes(reorder(state, !!viz_attr), !!viz_attr, fill=!!viz_attr)) +
      geom_col(color = gray(.8), lwd=.1, show.legend = F) +
      scale_fill_gradient2(low='yellow',  high='brown') +
      coord_flip() +
      si_style_nolines() +
      theme(
        axis.text.x = element_blank(),
        axis.title = element_blank()
      )
    
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
  
  # Country context Map
  nga_adm1 %>% 
    ggplot() +
    geom_sf(data=nga_adm2, fill="white", color=gray(.8), lwd=.5, lty ="dotted") +
    geom_sf(fill=NA, color=gray(.5), lwd=.5) +
    coord_sf() +
    geom_sf_text(aes(label=state), size = 2) +
    labs(title = "NIGERIA") +
    coord_sf() +
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
  
  ## Generate COVID Maps
  
  nga_map_deaths <- geo_plot(nga_geo, 'no_of_deaths', 'NIGERIA - COVID-19 Related Deaths', viz_save = T)
  nga_map_confirmed <- geo_plot(nga_geo, 'no_of_cases_lab_confirmed', 'NIGERIA - COVID-19 Confirmed Cases', viz_save = T)
  nga_map_admisions <- geo_plot(nga_geo, 'no_of_cases_on_admission', 'NIGERIA - COVID-19 Cases on Admission', viz_save = T)
  nga_map_discharged <- geo_plot(nga_geo, 'no_discharged', 'NIGERIA - COVID-19 Discharged Cases', viz_save = T)

  ## Generate bar charts of COVID Stats
  
  nga_deaths <- nga_geo %>% 
    dplyr::filter(!is.na(no_of_deaths) & no_of_deaths != 0) %>% 
    ggplot(aes(reorder(state, no_of_deaths), no_of_deaths, fill=no_of_deaths)) +
    geom_col(color = gray(.8), lwd=.1, show.legend = F) +
    scale_fill_gradient2(low='yellow',  high='brown') +
    coord_flip() +
    si_style_nolines() +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank()
    )
  
  nga_confirmed <- nga_geo %>% 
    dplyr::filter(!is.na(no_of_cases_lab_confirmed) & no_of_cases_lab_confirmed != 0) %>% 
    ggplot(aes(reorder(state, no_of_cases_lab_confirmed), no_of_cases_lab_confirmed, fill=no_of_cases_lab_confirmed)) +
    geom_col(color = gray(.8), lwd=.1, show.legend = F) +
    scale_fill_gradient2(low='yellow',  high='brown') +
    coord_flip() +
    si_style_nolines() +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank()
    )
  
  nga_admisions <- nga_geo %>% 
    dplyr::filter(!is.na(no_of_cases_on_admission) & no_of_cases_on_admission != 0) %>% 
    ggplot(aes(reorder(state, no_of_cases_on_admission), no_of_cases_on_admission, fill=no_of_cases_on_admission)) +
    geom_col(color = gray(.8), lwd=.1, show.legend = F) +
    scale_fill_gradient2(low='yellow',  high='brown') +
    coord_flip() +
    si_style_nolines() +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank()
    )
  
  nga_discharged <- nga_geo %>% 
    dplyr::filter(!is.na(no_discharged) & no_discharged != 0) %>% 
    ggplot(aes(reorder(state, no_discharged), no_discharged, fill=no_discharged)) +
    geom_col(color = gray(.8), lwd=.1, show.legend = F) +
    scale_fill_gradient2(low='yellow',  high='brown') +
    coord_flip() +
    si_style_nolines() +
    theme(
      axis.text.x = element_blank(),
      axis.title = element_blank()
    )
  
  ## Arrange plots and save
  
  ## Deaths
  nga_map_deaths + nga_deaths +
    plot_layout(widths = c(2,1))
  
  ggsave(filename = paste0(dir_graphics, "/NIGERIA - COVID-19 Related Deaths by states.png"),
         width = 11, height = 7)
  
  ## Confirmed
  nga_map_confirmed + nga_confirmed +
    plot_layout(widths = c(2,1))
  
  ggsave(filename = paste0(dir_graphics, "/NIGERIA - COVID-19 Confirmed Cases by states.png"),
         width = 11, height = 7)
  
  
  ## Admissions
  nga_map_admisions + nga_admisions +
    plot_layout(widths = c(2,1))
  
  ggsave(filename = paste0(dir_graphics, "/NIGERIA - COVID-19 Cases on Admissions by states.png"),
         width = 11, height = 7)
  
  ## Discharged
  nga_map_discharged + nga_discharged +
    plot_layout(widths = c(2,1))
  
  ggsave(filename = paste0(dir_graphics, "/NIGERIA - COVID-19 Discharged Cases by states.png"),
         width = 11, height = 7)
  