## PROJECT:  COVID-19 Impact Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Extract COVID-19 and Other related data from public sources
## Date:     2020-05-19
## Updated:  2020-05-25

# LIBRARIES ----------------------------------------------------

library(tidyverse)    # Data wrangling
library(readxl)
library(raster)       # getData function is here
library(rasterVis)
library(rnaturalearth)# Geospatial
library(sf)           # Geospatial
library(glamr)        # SI Utilities
library(glitr)        # SI Plotting
library(Wavelength)
library(RColorBrewer) # Colors Schemes
library(lubridate)    # Date parser
library(ggthemes)
library(patchwork)

# REQUIRED - Run to load get_geodata
  source('./Scripts/01_Extract_Geodata.R')

# GLOBAL -------------------------------------------------------------

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_images <- "Images"

file_hfr_w <- "HFR_Weekly_Performance_Data.xlsx"
file_hfr_w2 <- "HFR_Performance.xlsx"
file_datim_sites <- "SBU_PEPFAR_USAID_Site_Coordinates_v3_long_SBU.csv"
file_datim_orgs <- "HFR_FY20_GLOBAL_orghierarchy_20200306.csv"
files_datim_targets <- list.files(path = dir_data, pattern = "HFR_FY20Q1_NGA", full.names = T)

ind_order <- c("HTS_TST", 
               "HTS_TST_POS", 
               "TX_NEW", 
               "TX_CURR",
               "PrEP_NEW"
               )

outlier_thresh = 100

# FUNCTIONS -----------------------------------------------------------
  
  ## For calculating lag differences and not getting a ton of Inf values
  lag_calc <- function(x, y) {
    ifelse(y > 0.000, (x/y) - 1, NA)
  }

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
    st_geometry() %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_as_sf()
  
  nga_hex <- nga_adm0 %>% 
    #st_make_grid(what = 'polygons', n = c(20, 20), square = F) %>% 
    st_make_grid(what = 'polygons', cellsize = 100000, square = F) %>% 
    st_as_sf() 
  
  nga_hex <- nga_hex %>% 
    st_intersection(nga_adm0) 
  
  nga_hex <- nga_hex %>% 
    mutate(id = row_number())
  
  nga_hex %>% plot()
  
  nga_adm1 <- get_geodata("NGA", 1) %>% 
    dplyr::select(state = name_1) %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_as_sf()

  nga_adm1 %>% plot()
  
  
  ## NGA Facilites
  
  nga_sites <- read_csv(here(dir_data, file_datim_orgs)) 
  
  nga_sites %>% glimpse()
  
  nga_sites <- nga_sites %>% 
    filter(
      operatingunit == 'Nigeria', 
      !is.na(facility),
      !is.na(latitude), 
      !is.na(longitude)
    )
  
  nga_sites %>% 
    distinct(snu1) %>% 
    pull() # 37 states
  
  nga_sites %>%  
    distinct_all() %>% 
    count() # 7386 sites
  
  nga_sites %>% 
    group_by(snu1) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    prinf()
  
  nga_sites <- nga_sites %>% 
    mutate(state = snu1) %>% 
    st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326)) 
  
  nga_sites <- nga_sites %>% 
    st_transform(crs = st_crs(3857)) %>% 
    st_join(nga_hex, join = st_intersects)
  
  nga_sites %>% 
    st_set_geometry(NULL) %>% 
    glimpse()
    
  
  ## NGA Targets
  
  nga_targets <- files_datim_targets %>% 
    map_dfr(~readr::read_csv(.x))
  
  nga_targets <- nga_targets %>%  
    group_by(orgunit, orgunituid, mech_code, mech_name, indicator) %>% 
    summarise(
      mer_results = sum(mer_results, na.rm = T),
      mer_targets = sum(mer_targets, na.rm = T)
    ) %>%
    ungroup() #4,338
  
  nga_sites_targets <- nga_targets %>% 
    group_by(orgunit, orgunituid, indicator) %>% 
    summarise(
      mer_results = sum(mer_results, na.rm = T),
      mer_targets = sum(mer_targets, na.rm = T)
    ) %>%
    ungroup() #2,749
  
  nga_sites_targets <- nga_sites_targets %>% 
    left_join(nga_sites, by=c('orgunituid' = 'orgunituid')) %>% 
    filter(!is.na(id)) %>% #2,344
    dplyr::select(-c(orgunit.y, geometry)) %>% 
    rename(orgunit = orgunit.x) 
  
  nga_sites_targets_hex <- nga_hex %>% 
    left_join(nga_sites_targets, by=c('id' = 'id')) %>% 
    group_by(id, indicator) %>% 
    summarise(
      sites = n_distinct(orgunituid),
      mer_results = sum(mer_results, na.rm = T),
      mer_targets = sum(mer_targets, na.rm = T)
    ) %>% 
    ungroup()

  ## NGA Weekly Data
  
  nga_hfr <- read_excel(here(dir_data, file_hfr_w2), 1)%>% 
    janitor::clean_names()
  
  nga_hfr %>% glimpse()
  
  nga_hfr <- nga_hfr %>% 
    mutate(
      state = sub('...', "", psnu),
      date = as.Date(hfr_week_start_date, "%m/%d/%y"),
      mechanism_id = as.numeric(gsub("(^[[:space:]]*)|([[:space:]]*$)", "", mechanism_id)),
      operatingunit = "Nigeria",
      kp_partner = case_when(
        mechanism_or_partner_name == "SFH KP Care 2" ~ 1,
        mechanism_or_partner_name == "Heartland Alliance KP Care 1" ~ 1,
        TRUE ~ 0
      )
    ) %>% 
    dplyr::select(-c(hfr_week_start_date, ou)) %>% 
    rename(
      orgunit = facility_or_community_name,
      orgunituid = facility_or_community_uid,
      mech_code = mechanism_id,
      partner = mechanism_or_partner_name,
      age_coarse = coarse_age,
      value = hfr_result_value
    ) 
  
  date_seq <- nga_hfr %>% 
    arrange(date) %>%  
    distinct(date) %>% 
    mutate(period = row_number())
  
  nga_hfr <- nga_hfr %>% 
    group_by_at(vars(-value, -sex, -age_coarse)) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    group_by_all() %>% 
    mutate(rowcount = n()) %>% 
    ungroup() %>% 
    arrange(orgunituid, indicator, date) %>% 
    left_join(., date_seq, by = c('date' = 'date')) 
  
  nga_hfr %>% glimpse()
  
  nga_hfr %>% 
    distinct(indicator) %>% 
    pull()
  
  nga_hfr <- nga_hfr %>% 
    filter(indicator %in% ind_order)
  
  nga_hfr %>% 
    distinct(state, orgunituid) %>% 
    group_by(state) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(
      state = gsub(" State", "", state),
      state = gsub(" state", "", state),
      state = gsub("-", " ", state)
    ) %>% 
    arrange(desc(n))
  
  
  nga_hfr <- nga_hfr %>% 
    left_join(nga_sites, by=c('orgunituid' = 'orgunituid')) %>% 
    dplyr::select(-state.x, state = state.y)
  
  ## Completeness
  
  
  #TODO: Finish this
  nga_hfr_hex <- nga_hex %>% 
    left_join(nga_hfr, by=c('id'='id')) %>% 
    dplyr::select(-geometry) %>% 
    group_by(state, id, partner, indicator, period) %>% 
    summarise(...) 

# VIZ -----------------------------------------------------
  
  ## No of Sites & Targets
  map_hex_title <- 'NIGERIA - TX_CURR Targets Distribution'
  
  nga_sites_targets_hex %>% 
    filter(!is.na(indicator), indicator == 'TX_CURR') %>% 
    ggplot() +
    geom_sf(data = nga_hex, fill=NA) +
    geom_sf(aes(fill=mer_targets)) + 
    geom_sf(data = nga_adm1, fill=NA, lwd=.5, color = si_colors$grey50) +
    scale_fill_viridis_c(direction = -1) +
    geom_sf_text(data=nga_adm1, aes(label=state), size=4, color=si_colors$grey50) +
    #facet_wrap(~indicator) +
    labs(
      title = map_hex_title,
      subtitle = 'Hex bins are areas of ~100SqKm'
    ) +
    theme_map() +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(2, "cm"),
      legend.title = element_blank()
    )
  
  ggsave(filename = here(dir_graphics, paste0(map_hex_title, " - mer targets.png")),
         width = 11, height = 7)
  
  nga_sites_targets_hex %>% 
    filter(!is.na(indicator)) %>% 
    ggplot() +
    geom_sf(data = nga_hex, fill=NA) +
    geom_sf(aes(fill=mer_targets)) + 
    geom_sf(data = nga_adm1, fill=NA, lwd=.4, color = gray(.4)) +
    scale_fill_viridis_c(direction = -1) +
    geom_sf_text(data=nga_adm1, aes(label=state), size=2) +
    facet_wrap(~indicator) +
    theme_map() +
    theme(
      legend.position = 'bottom',
      legend.key.width = unit(2, "cm")
    )
  
  ##=>
  partner <- 'Chemonics TO3'
  pdate <- '2020-03-30'
  
  geoviz(nga_hfr_hex, partner, date)
  
  ## Export maps by Prime Partner
  nga_hfr_hex %>% 
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
  
  