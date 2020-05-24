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
library(janitor)
library(here)
library(patchwork)
library(ggthemes)
library(ggpattern)

# REQUIRED

  ## Run to update covid and gov measures data
  source('./Scripts/01_Extract_Geodata.R')

# GLOBAL -------------------------------------------------------------

  dir_data <- "Data"
  dir_dataout <- "Dataout"
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
                       viz_subtitle = "",
                       overlay = NULL,
                       viz_save = FALSE) {
    
    viz <- dataset %>% 
      ggplot() +
      geom_sf(
        aes_string(fill = viz_attr), 
        color=gray(.8), 
        lwd=.5)
    
    if (!is.null(overlay)) {
      viz <- viz +
        geom_sf(
          data = overlay, 
          size = 1.5, 
          color = gray(.5)) 
    }
    
    viz <- viz +
      scale_fill_gradient2(low='yellow',  
                           high='brown', 
                           na.value = 'white') +
      geom_sf_text(
        aes(label=state), 
        size = 3) +
      coord_sf() +
      labs(
        title = viz_title,
        subtitle = viz_subtitle,
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
  
  # Bar charts => NOT WORKING!
  bar_chart <- function(dataset, 
                        viz_attr) {
    
    viz_attr <- as.name(viz_attr)
    
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
    
    print(viz)
    
    return(viz)
  }

# DATA ---------------------------------------------------------------
  
  ## Admin boundaries
  
  nga_adm0 <- get_geodata("NGA", 0) %>% 
    st_geometry() %>% 
    st_transform(crs = st_crs(3857))
  
  nga_adm1 <- get_geodata("NGA", 1) %>% 
    st_transform(crs = st_crs(3857)) %>% 
    dplyr::select(state = name_1) %>% 
    mutate(state = ifelse(state == 'Federal Capital Territory', 'FCT', state))
  
  nga_adm2 <- get_geodata("NGA", 2) %>% 
    st_transform(crs = st_crs(3857)) %>%
    dplyr::select(state = name_1, lga = name_2) %>% 
    mutate(state = ifelse(state == 'Federal Capital Territory', 'FCT', state))
  
  ## Gov. Measure Data
  
  nga_govm <- read_csv(paste0(dir_dataout, "/nga_acaps_gov_measures_data_", Sys.Date(), ".csv")) %>% 
    filter(admin_name != 'Country') %>% 
    mutate(admin_name = ifelse(admin_name == 'Federal Capital Territory', 'FCT', admin_name)) %>% 
    mutate(admin_name = gsub(" State", "", admin_name)) 
  
  nga_govm_c <- nga_govm %>% 
    distinct(admin_name, category)
  
  nga_adm1 %>% 
    filter(!state %in% (nga_govm_c %>% pull(admin_name)))
    
  nga_govm_geo <- nga_adm1 %>% 
    dplyr::left_join(nga_govm_c, by=c('state' = 'admin_name')) 
  
  nga_govm_geo_lockd <- nga_govm_geo %>% 
    filter(category == 'Lockdown') %>% 
    group_by(category) %>% 
    summarise() %>%
    st_make_grid(what = "corners", cellsize = 40000)
  
  nga_govm_geo_movr <- nga_govm_geo %>% 
    filter(category == 'Movement restrictions') %>% 
    group_by(category) %>% 
    summarise() %>%
    st_make_grid(what = "corners", cellsize = 30000)
  
  nga_govm_geo_pubh <- nga_govm_geo %>% 
    filter(category == 'Public health measures') %>% 
    group_by(category) %>% 
    summarise() %>%
    st_make_grid(what = "corners", cellsize = 10000) 
  
  nga_govm_geo_socd <- nga_govm_geo %>% 
    filter(category == 'Social distancing') %>% 
    group_by(category) %>% 
    summarise() %>%
    st_make_grid(what = "corners", cellsize = 10000) 
  
  ## Covid-19 Stats
  
  nga_covid <- read_csv(paste0(dir_dataout, "/nga_ncdc_covid_data_", Sys.Date(), ".csv"))
  
  nga_covid_geo <- nga_adm1 %>% 
    dplyr::left_join(nga_covid, by=c('state' = 'states_affected')) 
  

# VIZ ------------------------------------------------------------------
  
  # Country context Map
  nga_adm1 %>% 
    ggplot() +
    geom_sf(data=nga_adm2, fill="white", color=gray(.8), lwd=.5, lty ="dotted") +
    geom_sf(fill=NA, color=gray(.5), lwd=.5) +
    coord_sf() +
    geom_sf_text(aes(label=state), size = 2) +
    labs(title = "NIGERIA - Admistrative Sub-divisions") +
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
  
  
  ## Get all relevant columns
  covid_map_cols <- nga_covid_geo %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(-state) %>% 
    names() 
  
  covid_map_titles <- c(
    "Confirmed Cases", 
    "Cases on Admission", 
    "Discharged Cases", 
    "Related Deaths"
  ) %>% 
    setNames(covid_map_cols)

  
  ## Combine maps + plots and save
  
  names(covid_map_titles) %>% 
    map(function(map_col) {
      
      map_title <- paste0('NIGERIA - COVID-19 ', covid_map_titles[[map_col]])
      map_subtitle <- "Grey dots indicate state has local lockdown measures"
      
      map <- geo_plot(nga_covid_geo, 
                      map_col, 
                      map_title, 
                      viz_subtitle = map_subtitle,
                      overlay = nga_govm_geo_lockd, 
                      viz_save = T)
      
      bar <- bar_chart(nga_covid_geo, map_col)
      
      map + bar +
        plot_layout(widths = c(2,1))
      
      ggsave(filename = here(dir_graphics, paste0(map_title, " by states.png")),
             width = 11, height = 7)
    })
  
