## PROJECT:  COVID-19 Impact Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Extract Geodata from GADM
## Date:     2020-05-14

# LIBRARIES ----------------------------------------------------

library(glamr)      # SI Utilities
library(tidyverse)  # Data wrangling
library(raster)     # getData function is here
library(rasterVis)
library(sf)
library(glitr)      # SI Plotting
library(RColorBrewer)
library(here)

# GLOBAL --------------------------------------------------------

dir_data <- "Data"
dir_gis <- "GIS"

# FUNCTION ----------------------------------------------------------------
  
  #' Get geospatial data from GADM
  #' 
  #' @param country_name ISO3 code
  #' @param adm_level country administrative boundaries level
  #' @param local save a copy?
  #' @return geodata of type sp
  #' @example 
  #' get_geodata("UGA") 
  #' get_geodata("UGA", adm_level=2, local=FALSE)
  #' 
  get_geodata <- function(country_name, adm_level=0, local=TRUE) {
    
    # Read data form local dir first
    downlaod <- local != TRUE
    
    # geodata location
    path_gis = here()
    
    if (dir.exists(here("GIS"))) {
      path_gis <- here("GIS")
    }
    
    # Extract data from GADM
    geo_data <- raster::getData("GADM", 
                        country = country_name, 
                        level = adm_level,
                        download = downlaod,
                        path = path_gis)
    
    # Force a download
    if (is.null(geo_data)) {
      geo_data <- get_geodata(country_name, adm_level, local = FALSE)
    }
    
    # Convert data into sf format
    geo_data <- st_as_sf(geo_data) %>% 
      rename_at(vars(everything()), tolower)
    
    # Return formatted data
    return(geo_data)
  }

  
  #' Create an hexbins polygon feature class
  #' 
  #' @param country_name iso3 code
  #' @param level country administrative unit level
  #' @param size size of each hex bin in meters
  #' @return country hex polygon as feature class
  #' @example 
  #' create_hexbins('LSO', 1)
  #' create_hexbins('UGA', 1)
  create_hexbins <- function(country_name, level=0, size=NULL) {
    
    cntry_adm <- get_geodata(country_name, adm_level=level) %>% 
      st_transform(crs = st_crs(3857))
    
    cntry_adm0 <- cntry_adm
    
    if ( level != 0 ) {
      cntry_adm0 <- cntry_adm %>% 
        mutate(
          id = row_number(),
          area = st_area(.)
        ) %>% 
        group_by(id) %>% 
        summarise(area = sum(area))
    }
    
    cntry_hex <- cntry_adm0 %>% 
      st_make_grid(what = "polygones", square = FALSE, n = c(10, 20)) %>% 
      st_intersection(cntry_adm0)
    
    viz <- cntry_hex %>% 
      ggplot() +
      geom_sf(fill=NA, lwd=.3, color=grey30k) +
      geom_sf(data=cntry_adm, fill=NA, lwd=.5, color=grey50k) +
      labs(title = country_name, caption = "OHA/SIEI - Data Source: GADM") +
      theme_void()
    
    print(viz)
    
    return(cntry_hex)
  }
  
  # Test
  #uga_hex <- create_hexbins('UGA', 1)
  #lso_hex <- create_hexbins('LSO', 1)

# DATA -------------------------------------------------------

  nga0 <- get_geodata("NGA", 0) %>% 
    st_geometry()

  nga1 <- get_geodata("NGA", 1) %>% 
    dplyr::select(hasc = hasc_1, name = name_1) %>% 
    dplyr::mutate(hasc = substr(hasc, 4,5))

  nga1 %>% 
    ggplot(aes(fill=name, label=hasc)) +
    geom_sf(show.legend = F, color = 'white', lwd =.5) +
    scale_fill_discrete() +
    geom_sf_text(size = 2) +
    labs(x="", y="") +
    si_style() +
    theme(
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      panel.background = element_rect(fill = 'white', colour = gray(.5))
    )

