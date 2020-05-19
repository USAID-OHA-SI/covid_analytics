# Title: Extract and Munge HDX Government Measures
# Author: Tim Essam
# Date: 2020-04-23
# Notes: for COVID-19 WG


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(readxl)
library(glitr)


# GLOBALS -----------------------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"
images <- "Images"

prinf <- function(df) {
  print(df, n = Inf)
}


# LOAD, MUNGE -------------------------------------------------------------
  #Data are from below:
  # https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset
  
  url <- "https://data.humdata.org/dataset/e1a91ae0-292d-4434-bc75-bf863d4608ba/resource/e571077a-53b6-4941-9c02-ec3214d17714/download/20200421-acaps-covid-19-goverment-measures-dataset-v9.xlsx"
  covid <- basename(url)
  # download.file(url, file.path(data_in, basename(url)))
  # Does not work until you open file and unprotect it

# Grab PEPFAR OUS
  global_org <- read_csv("./Data/HFR_FY20_GLOBAL_orghierarchy_20200306.csv")

  cntries <- global_org %>% 
    distinct(countryname, operatingunit)

# READ IN HDX COVID MEASURES DATA 
  read_path <- file.path(data_in, covid)
   
   dfl <- 
    read_path %>% 
    excel_sheets() %>% 
    set_names() %>%
     purrr::map(read_excel, path = read_path)
    
   
# CLEAN UP MEASURES, FLAG PEPFAR OUS, Re-add Regional categories for FACETS   
 ou_df <- 
   dfl$Database %>% janitor::clean_names() %>% 
   mutate(measure_lump = case_when(
     grepl("curfews|Curfews", measure) ~ "Curfews",
     grepl("limit public gatherings|Limit public gatherings", measure) ~ "Limit public gatherings",
     grepl("strengthening the public health system|Strengthening the public health system", measure) ~ "Strengthening the public health system",
     grepl("testing policy|Testing Policy", measure) ~ "Testing Policy",
     TRUE ~ measure)
   ) %>% 
   # Change to PEPFAR Country names
   mutate(country = str_replace(country, " and ", " & ")) %>% 
   mutate(country = case_when(
     country == "Côte d'Ivoire" ~"Cote d'Ivoire",
     country == "Viet Nam" ~ "Vietnam",
     country == "Congo DR" ~ "Democratic Republic of the Congo",
     country == "Myanmar" ~ "Burma",
     country == "Lao PDR" ~ "Laos",
     TRUE ~ country),
     PEPFAR_ou = if_else(country %in% cntries$countryname, 1, 0)
     )

 ou_df %>% filter(PEPFAR_ou == 1) %>% count()
 
 ou_pepfar <- 
   cntries %>% 
   left_join(ou_df, by = c("countryname" = "country")) %>% 
   mutate(facetvar = case_when(
     !operatingunit %in% c("Asia Region", "West Africa Region", "Western Hemisphere Region") ~ "",
     TRUE ~ str_to_upper(operatingunit)
   ))

 
  ou_pepfar %>% count(facetvar)  %>% prinf() 

 



# VIZ ---------------------------------------------------------------------

# Show general categories of measures implemented by different PEPFAR OUs
 ou_pepfar %>% 
   count(countryname, category, facetvar, sort = TRUE) %>% 
    filter(category != "Humanitarian exemption", facetvar == "") %>% 
   mutate(category_wrap = str_wrap(category, width = 10),
          category_wrap = fct_reorder(category_wrap, n, .desc = TRUE)) %>% 
   arrange(countryname, category_wrap) %>% 
   ggplot(aes(y = reorder(countryname, desc(countryname)), x = category_wrap, fill = n)) +
   geom_tile(color = "white", size = 0.25) + 
    scale_x_discrete(position = "top") +
    scale_fill_viridis_c(direction = -1, option = "A") +
   si_style_nolines() +
   facet_wrap(~facetvar, scales = "free_y", strip.position = "bottom", nrow = 2) +
  labs(x = NULL, y = NULL,
       title = "COUNT OF COVID-19 RELATED MEASURES TAKEN BY PEPFAR OUS",
       subtitle = "Darker colors indicate more measures have been recorded",
       caption = "\nSource: ACAPS COVID-19: Government Measures Dataset",
       fill = "Number of measures recorded") +
    theme(strip.text = element_text(size = 12),
          plot.caption = element_text(hjust = 1)) 

 si_save("Images/PEPFAR_COVID_OU_Measures.png", scale = 1.45)
 
 
# When did a country first enact a measure
 
 ou_pepfar %>% count(measure, sort = TRUE) %>% prinf()

 measures_plot <- function(filter) {
   ou_pepfar %>% 
     group_by(countryname, date_implemented, category, facetvar) %>% 
     summarise(count = n()) %>% 
     ungroup() %>% 
     arrange(date_implemented) %>% 
     mutate(date_order = row_number(), 
            countrysort = fct_reorder(countryname, date_order, .desc = TRUE),
            date_implemented = as.Date(date_implemented)) %>% 
     filter(facetvar == {{filter}}) %>% 
     ggplot(aes(x = date_implemented, y = countrysort, size = count)) +
     geom_point(alpha = 0.25)+ 
     scale_x_date(date_breaks = "1 week") +
     si_style() + 
     scale_color_brewer(palette = "Greys") +
     theme(axis.text.x  = element_text(angle=45, hjust = 1, vjust = 1, size = 8)) +
     labs(x = NULL, y = NULL, 
          title = "COVID-19 GOVERNMENT MEASURES ENACTED BY DATE",
          subtitle = "Larger circle size represents more measures enacted by government",
          caption = "\nSource: ACAPS COVID-19: Government Measures Dataset") 
   #return(last_plot())
   
   ggsave(file.path(images, paste0("COVID_OU_measures", "_", {{filter}}, ".png")),
          plot = last_plot(), dpi = 330, width = 10, height = 5.625)
 }

 
 measures_plot("ASIA REGION")
 unique(ou_pepfar$facetvar) %>% 
   walk(measures_plot)
 

si_save("Images/PEPFAR_COVID_OU_Measures_timeline.png")

ou_pepfar %>% 
  count(measure, category, sort = TRUE) %>%
  prinf()
   


  