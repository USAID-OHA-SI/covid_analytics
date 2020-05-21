## PROJECT:  COVID-19 Impact Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Extract COVID-19 and Other related data from public sources
## Date:     2020-05-19

# LIBRARIES ----------------------------------------------------

library(tidyverse)    # Data wrangling
library(readxl)
library(raster)       # getData function is here
library(rasterVis)
library(rnaturalearth)# Geospatial
library(sf)           # Geospatial
library(glamr)        # SI Utilities
library(glitr)        # SI Plotting
library(RColorBrewer) # Colors Schemes
library(lubridate)    # Date parser
library(ggthemes)
library(patchwork)


# GLOBAL -------------------------------------------------------------

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_images <- "Images"

file_hfr_w <- "HFR_Weekly_Performance_Data.xlsx"
file_datim_sites <- "SBU_PEPFAR_USAID_Site_Coordinates_v3_long_SBU.csv"

# FUNCTIONS -----------------------------------------------------------

  geoviz <- function(dataset, ip, pd=NULL) {
    
    df <- dataset
    partner <- ip
    pdate <- pd
    
    if (is.null(pdate)) {
      
      df <- df %>% 
        filter(prime_partner == partner) %>% 
        group_by(id) %>% 
        summarise(value = sum(value, na.rm = T))
      
      pdate = "all periods"
    } 
    else {
      df <- df %>% 
        filter(prime_partner == partner, period == pdate) 
    }
    
    viz <- df %>%
      ggplot() +
      geom_sf(aes(fill = value)) +
      geom_sf(data = nga_hex, fill = NA, lwd = .5, color = gray(.9)) +
      geom_sf(data = nga_adm1, fill = NA, color = gray(.4)) +
      scale_fill_viridis_c(direction = -1) +
      geom_sf_text(data=nga_adm1, aes(label=state), size=2, color = gray(.4)) +
      coord_sf() +
      labs(
        title = "NIGERIA - Spatiotemporal Distribution of HST_TST",
        subtitle = paste0("Partner: ", partner, ", HFR Period: ", pdate),
        caption = "Produced by OHA/SIEI/SI - Source: USAID/Nigeria, PEPFAR Programs"
      ) +
      theme_minimal() +
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
    
    ggsave(paste0(dir_images, "/NIGERIA - Spatiotemporal Distribution of HST_TST - ", partner, " - ", pdate, ".png"),
           width = 11, height = 8)
  }

# DATA

  ## Geodata 
  
  nga_adm0 <- ne_countries(country = 'Nigeria', scale = 'medium', returnclass = 'sf') %>% 
    st_geometry(NULL) %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_as_sf()
  
  nga_adm0 %>% 
    st_make_grid(what = 'polygons', cellsize = 30000, square = F) %>% 
    plot()
  
  nga_hex <- nga_adm0 %>% 
    #st_make_grid(what = 'polygons', n = c(20, 20), square = F) %>% 
    st_make_grid(what = 'polygons', cellsize = 30000, square = F) %>% 
    st_as_sf() 
  
  nga_hex <- nga_hex %>% 
    st_intersection(nga_adm0) 
  
  nga_hex <- nga_hex %>% 
    mutate(id = row_number())
  
  nga_adm1 <- get_geodata("NGA", 1) %>% 
    dplyr::select(state = name_1) %>% 
    st_as_sf()

  
  ## NGA Facilites
  
  nga_sites <- read_csv(here(dir_data, file_datim_sites)) 
  
  nga_sites %>% 
    glimpse()
  
  nga_sites <- nga_sites %>% 
    filter(iso == 'NGA', !is.na(latitude), !is.na(longitude))
  
  ## Keep only unique sites with "TX_CURR" & "HTS_TST"
  nga_sites <- nga_sites %>% 
    filter(indicator != 'LAB_PTCQI') %>% 
    dplyr::select(orgunituid, latitude, longitude) %>% 
    distinct_all() 
  
  nga_sites <- nga_sites %>% 
    st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326)) %>% 
    st_join(nga_adm1)
  
  nga_sites <- nga_sites %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_join(nga_hex, join = st_intersects)
  
  nga_sites %>% 
    st_set_geometry(NULL) %>% 
    group_by(state) %>% 
    tally() %>% 
    arrange(desc(n))
  
  nga_sites %>% 
    st_set_geometry(NULL) %>% 
    group_by(id) %>% 
    tally() %>% 
    arrange(desc(n))


  ## NGA Weekly Data
  nga_hfr_w <- read_excel(here(dir_data, file_hfr_w), 1)%>% 
    janitor::clean_names()
  
  nga_hfr_w <- nga_hfr_w  %>% 
    rename(
      orgnunit = organisation_unit,
      orgunituid = facility_uid,
      mech_code = mechanism_id,
      prime_partner = hfr_partners,
      indicator = data
    ) %>% 
    mutate(
      period = ymd(period),
      year = year(period),
      month = month(period),
      day = day(period)
    ) 
  
  nga_hfr_hts_tst <- nga_hfr_w %>% 
    filter(indicator == 'HTS_TST') %>% 
    left_join(nga_sites, by=c('orgunituid' = 'orgunituid')) %>% 
    dplyr::select(-state.x, state = state.y)
  
  
  nga_hfr_hts_tst <- nga_hex %>% 
    left_join(nga_hfr_hts_tst, by=c('id'='id')) %>% 
    dplyr::select(-geometry) %>% 
    group_by(id, prime_partner, period) %>% 
    summarise(value = sum(value, na.rm = T)) 

# VIZ -----------------------------------------------------
  
  partner <- 'Chemonics TO3'
  pdate <- '2020-03-30'
  
  geoviz(nga_hfr_hts_tst, partner, pdate)
  
  ## Export maps by Prime Partner
  nga_hfr_hts_tst %>% 
    st_set_geometry(NULL) %>% 
    ungroup() %>% 
    filter(!is.na(prime_partner)) %>% 
    distinct(prime_partner) %>% 
    arrange(prime_partner) %>% 
    pull() %>% 
    map(function(ip) {
      geoviz(nga_hfr_hts_tst, ip)
    })
  
  ## TODO: Facet Maps by IP / Period with no legend
  nga_hfr_hts_tst %>% 
    st_set_geometry(NULL) %>% 
    ungroup() %>% 
    filter(!is.na(prime_partner)) %>% 
    distinct(prime_partner, period) %>% 
    arrange(prime_partner, period) 
  
  