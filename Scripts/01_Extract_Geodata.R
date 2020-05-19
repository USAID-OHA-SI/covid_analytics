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

# GLOBAL --------------------------------------------------------

#folder_setup()

dir_data <- "Data"
dir_gis <- "GIS"

# FUNCTION ----------------------------------------------------------------
  
  get_geodata <- function(country_name, adm_level=0, local=TRUE) {
    
    # Read data form local dir first
    downlaod <- local != TRUE
    
    # geodata location
    path_gis = here()
    
    if (dir.exists(here("gis"))) {
      path_gis <- here("gis")
    }
    
    # Extract data from GADM
    geo_data <- raster::getData("GADM", 
                        country = country_name, 
                        level = adm_level,
                        download = downlaod,
                        path = dir_gis)
    
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

# DATA -------------------------------------------------------

  nga0 <- get_geodata("NGA", 0) %>% 
    st_geometry()

  nga0 <- get_geodata("NGA", 0) %>% 
    st_geometry()

  nga1 <- get_geodata("NGA", 1)

  nga1 %>% 
    st_set_geometry(NULL) %>% 
    glimpse()

  nga1 <- nga1 %>% 
    dplyr::select(hasc = hasc_1, name = name_1) %>% 
    dplyr::mutate(hasc = substr(hasc, 4,5))
  

  nga1 %>% 
    st_geometry() %>% 
    ggplot() +
    geom_sf(fill=NA)+
    bwTheme()

  nga1 %>% 
    ggplot(aes(fill=name, label=hasc)) +
    geom_sf(show.legend = F, color = 'white', lwd =.5) +
    #scale_fill_brewer(palette = "Spectral") +
    geom_sf_text(size = 2) +
    labs(title = 'Nigeria - COVID-19 Impact', x=NULL, y=NULL, caption = 'Source: GADM, NCDC, JHU') +
    si_style() +
    theme(
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      panel.background = element_rect(fill = 'white', colour = gray(.5))
    )

