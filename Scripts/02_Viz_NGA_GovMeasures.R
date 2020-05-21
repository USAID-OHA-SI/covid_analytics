## PROJECT:  COVID-19 Impact Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Visualize GovMeasures at local levels
## Date:     2020-05-19

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
dir_dataout <- "Dataout"
dir_gis <- "GIS"
dir_graphics <- "Graphics"
dir_images <- "Images"

file_govm <- "nga_acaps_gov_measures_data.csv"

# FUNCTIONS ----------------------------------------------------------
  
  ## Maps
  
  geo_plot <- function(dataset, viz_var, viz_val=NULL) {
    
    # Variable color
    viz_color <- "#b2df8a"
    
    if (viz_var == "category")
      viz_color <- "#8dd3c7"
    
    # Variable name
    viz_var <- as.name(viz_var)
    
    # Geodata
    geodata <- get_geodata('NGA', 1) %>% 
      dplyr::select(state = name_1) %>% 
      mutate(state = if_else(state == 'Federal Capital Territory', 'FCT', state))
    
    # Filter dataset
    df <- dataset %>% 
      filter(admin_name != 'Country', !!viz_var == viz_val) %>% 
      group_by(admin_name) %>% 
      summarise(
        govm = if_else(n_distinct(!!viz_var) > 0, 'YES', 'NO')
      )
    
    # Join data to geo
    geodata <- geodata %>% 
      left_join(df, by = c("state" = "admin_name")) %>% 
      mutate(govm = if_else(is.na(govm), 'NO', govm)) %>% 
      mutate(govm = factor(govm, levels = c('YES', 'NO')))
    
    # Plot
    viz <- geodata %>% 
      ggplot(aes(fill = factor(govm))) +
      geom_sf(color=gray(.9), lwd=.5) +
      scale_fill_manual(values = c(viz_color, "#F5F5F5"), labels = c("YES", "NO")) +
      geom_sf_text(aes(label=state), size = 3) +
      coord_sf() +
      labs(
        title = toupper(paste0("Government Measure [", viz_val, "]")),
        caption = paste0('OHA/SIEI/SI, Source: ACAPS as of ', Sys.Date())
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

    # Export map
    ggsave(
      filename = paste0(dir_images, "/NIGERIA-GovMeasures-", viz_var, "_", viz_val, ".png"),
      width = 11, height = 8
    )
    
    return(viz)
  }


# DATA ---------------------------------------------------------------

# Government Measures
  
  ## NGA COVID-19 Related Government Measures
  
  nga_govm <- read_csv(here(dir_dataout, file_govm))

  nga_govm <- nga_govm %>% 
    mutate(admin_name = if_else(admin_name == 'Federal Capital Territory', 'FCT', admin_name)) %>%
    mutate(admin_name = gsub(" State", "", admin_name))
  
  nga_govm %>% 
    glimpse() 
  
  ## Federal Measures
  nga_govm %>% 
    filter(admin_name == 'Country') %>% 
    distinct(category, measure) %>% 
    arrange(category, measure)
  
  ## States Measures
  nga_govm %>% 
    filter(admin_name != 'Country') %>% 
    distinct(category, measure) %>% 
    arrange(category, measure) 
  
  ## Generate maps for all state level measure categories
  nga_govm %>% 
    filter(admin_name != 'Country') %>% 
    distinct(category) %>% 
    pull() %>% 
    map(function(cat) geo_plot(nga_govm, 'category', cat))
  
  ## Generate maps for all state level measure categories
  nga_govm %>% 
    filter(admin_name != 'Country') %>% 
    distinct(measure) %>% 
    pull() %>% 
    map(function(m) geo_plot(nga_govm, 'measure', m))
    
    
  #geo_plot(nga_govm, 'category', 'Lockdown')
  