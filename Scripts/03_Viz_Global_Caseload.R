## PROJECT:  COVID-19 Impact Analytics
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Extract COVID-19 and Other related data from public sources
## Date:     2020-08-26


# LIBRARIES ----------------------------------------------------

library(tidyverse)    
library(readxl)
library(vroom)
library(raster)       
library(rasterVis)
library(rnaturalearth)
library(sf)  
library(gisr)
library(glamr)        
library(glitr)
library(RColorBrewer) 
library(lubridate)    
library(ggthemes)
library(patchwork)
library(COVIDutilities)
library(Wavelength)
library(here)

# REQUIRED - Run to load get_geodata
#source('./Scripts/01_Extract_Geodata.R')

# GLOBAL ------------------------------------------------------


user <- ""
key <- ""

dir_data <- "Data"
dir_dataout <- "Dataout"
dir_graphics <- "Graphics"

file_covid <- paste0(dir_dataout, "/global_covid_data_", as.Date(Sys.Date(), "%Y%m%d"), ".csv")

footnote <- paste0("USAID/OHA/SIEI - HQ Q3 Data Review, ", Sys.Date())

# DATA --------------------------------------------------------

  ## PEPFAR OUs
  ous <- identify_ouuids(username = user, password = mypwd(key)) 

  ous <- ous %>% 
    dplyr::filter(!str_detect(country, "Region"), type == "OU") %>% 
    dplyr::select(-uid)

  ## COVID-19 - Global Data
  df_covid <- pull_jhu_covid()
  
  df_covid %>% 
    distinct(countryname) %>% 
    arrange(countryname) %>% 
    pull()

  df_covid <- df_covid %>% 
    mutate(countryname = case_when(
      countryname == "Taiwan*" ~ "Taiwan",
      countryname == "Korea, South" ~ "South Korea",
      countryname == "Guinea-Bissau" ~ "Guinea Bissau",
      countryname == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
      countryname == "Congo (Brazzaville)" ~ "Republic of the Congo",
      countryname == "US" ~ "United States",
      TRUE ~ countryname
    ))
    
  countries <- df_covid %>% 
    distinct(countryname) %>% 
    arrange(countryname) %>% 
    pull()
  
  countries
  
  df_covid %>% 
    arrange(countryname, desc(cases)) %>% 
    View(title = "COVID Global Daily")
  
  df_covid_cntry <- df_covid %>%
    group_by(countryname) %>% 
    summarise_at(vars(daily_cases, daily_recoveries, daily_deaths), sum, na.rm = TRUE) %>% 
    ungroup()
  
  df_covid_cntry %>% arrange(desc(daily_cases)) %>% prinf()

  ## Geodata 
  
  spdf <- ne_countries(type = "sovereignty", scale = 110, returnclass = "sf") 

  spdf <- spdf %>% 
    filter(admin != "Antarctica") %>% 
    dplyr::select(sovereignt, admin, name, iso3 = adm0_a3) %>% 
    mutate(admin = case_when(
      admin == "Czech Republic" ~ "Czechia",
      admin == "Ivory Coast" ~ "Cote d'Ivoire",
      admin == "Czechia" ~ "Czech Republic",
      admin == "Myanmar" ~ "Burma",
      admin == "Northern Cyprus" ~ "Cyprus",
      admin == "Republic of Serbia" ~ "Serbia",
      admin == "Republic of Congo" ~ "Republic of the Congo",
      admin == "East Timor" ~ "Timor-Leste",
      admin == "The Bahamas" ~ "Bahamas",
      admin == "United Republic of Tanzania" ~ "Tanzania",
      admin == "United States of America" ~ "United States",
      admin == "Swaziland" ~ "Eswatini",
      TRUE ~ admin
    ))
  
  spdf %>% 
    distinct(admin) %>% 
    arrange(admin) %>% 
    pull(admin) %>% 
    setdiff(countries)

  
# VIZ ---------------------------------------------------------

  ## World Countries
  spdf %>% 
    st_geometry() %>% 
    plot()
  
  spdf_covid <- spdf %>% 
    left_join(df_covid_cntry, by=c("admin" = "countryname")) %>% 
    left_join(ous, by = c("admin" = "country"))
  
  spdf_covid %>% 
    filter(!is.na(type)) %>% 
    pull(type)
  
  ## Confirmed Cases
  ggplot() +
    geom_sf(data = spdf, fill = "white", size = .2, color = NA) +
    geom_sf(data = spdf_covid, aes(fill = log10(daily_cases + 1)), size = .2) +
    geom_sf(data = spdf, fill = NA, size = .2, color = grey10k) +
    geom_sf_text(data = spdf_covid %>% filter(!is.na(type)), 
                 aes(label = iso3), color = grey80k, size = 1.5, fontface = "bold") +
    scale_fill_gradient2(
      low = "yellow",
      #mid = "orange",
      high = "brown",
      na.value = NA, 
      breaks = 2:6,
      labels = 10^c(2:6)
    ) +
    labs(
      title = "COVID-19 - Confirmed Cases",
      subtitle = paste0("Cases are cumulative as of ", Sys.Date(), "\nand may not include all countries and / or territories"),
      caption = paste0(footnote, "\nSource: JHU CSSE COVID-19 Data")
    ) +
    si_style_map() +
    theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(2, "cm"),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(here(dir_graphics, "FY20Q3_COVID-19_ComfirmedCases.png"),
         plot = last_plot(), scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
  ## Recoveries
  ggplot() +
    geom_sf(data = spdf, fill = "white", size = .2, color = NA) +
    geom_sf(data = spdf_covid, aes(fill = log10(daily_recoveries + 1)), size = .2) +
    geom_sf(data = spdf, fill = NA, size = .2, color = grey10k) +
    geom_sf_text(data = spdf_covid %>% filter(!is.na(type)), 
                 aes(label = iso3), color = grey90k, size = 1.5, fontface = "bold") +
    scale_fill_gradient2(
      low = "#edf8e9",
      #mid = "#74c476",
      high = "#006d2c",
      na.value = NA, 
      breaks = 0:6,
      labels = 10^c(0:6)
    ) +
    labs(
      title = "COVID-19 - Recovered Cases",
      subtitle = paste0("Cases are cumulative as of ", Sys.Date(), "\nand may not include all countries and / or territories"),
      caption = paste0(footnote, "\nSource: JHU CSSE COVID-19 Data")
    ) +
    si_style_map() +
    theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(2, "cm"),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(here(dir_graphics, "FY20Q3_COVID-19_Recoveries.png"),
         plot = last_plot(), scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
  
  ## Deaths
  ggplot() +
    geom_sf(data = spdf, fill = "white", size = .2, color = NA) +
    geom_sf(data = spdf_covid, aes(fill = log10(daily_deaths + 1)), size = .2) +
    geom_sf(data = spdf, fill = NA, size = .2, color = grey10k) +
    geom_sf_text(data = spdf_covid %>% filter(!is.na(type)), 
                 aes(label = iso3), color = grey80k, size = 1.5, fontface = "bold") +
    scale_fill_gradient2(
      low = "yellow",
      #mid = "orange",
      high = "red",
      na.value = NA, 
      breaks = 0:5,
      labels = 10^c(0:5)
    ) +
    labs(
      title = "COVID-19 - Deaths",
      subtitle = paste0("Cases are cumulative as of ", Sys.Date(), "\nand may not include all countries and / or territories"),
      caption = paste0(footnote, "\nSource: JHU CSSE COVID-19 Data")
    ) +
    si_style_map() +
    theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.width = unit(2, "cm"),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(here(dir_graphics, "FY20Q3_COVID-19_Deaths.png"),
         plot = last_plot(), scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
  
  
  
  
