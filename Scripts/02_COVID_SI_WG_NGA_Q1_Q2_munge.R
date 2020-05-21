## PROJECT:  find your beach
## AUTHOR:   T. Essam | USAID
## LICENSE:  MIT
## PURPOSE:  Munge and plot Nigeria COVID milestones
## NOTE:     
## DATE:     2020-04-2
## UPDATED:  2020-04-27


# LIBRARIES ---------------------------------------------------------------
library(tidyverse)
library(glitr)
library(readxl)
library(here)
library(glitr)
library(glamr)
library(ISOcodes)
library(rvest)
library(scales)
library(COVIDutilities)

# GLOBALS -----------------------------------------------------------------
 
  data_in <- "Data"
  data_out <- "Dataout"
  viz_out <- "Images"

  NGA_read <- function(sheet = "government_measures") {
     read_excel(here(data_in, "COVID Data.xlsx"), sheet = {{sheet}}) %>% 
       filter(iso_alpha3 == "NGA")
  }

  
  ind_order <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR",
                 "PrEP_NEW")
  
  
# LOAD AND MUNGE COVID CASES FROM JHU & GOVERNMENT MEASURES  ------------------------------------------------

  # COVID Cases
  covid_cases <- pull_jhu_covid() %>% 
    filter(countryname %in% c("Nigeria", "Benin", "Ghana", "Cameroon", "Niger", "Chad")) %>% 
    mutate(nga_color = if_else(countryname == "Nigeria", wapo_dorange, grey20k))
  
  nga_first <- covid_cases %>% filter(countryname == "Nigeria" & first_case == 1) %>% pull(date) 
  nga_holiday <- as.Date("2019-12-25")
                        
  # Extract government measures data    
  df_gm <- extract_excel_data(hdx_govmes_url, hdx_govmes_linkid, 2, 'xlsx') %>%
    filter(iso == "NGA") 

    
# LOAD AND MUNGE DATIM TARGETS --------------------------------------------
  
  # Load Nigeria Target information
  
  target_files <- list.files(here(data_in), pattern = "HFR_FY20Q1", full.names = TRUE) 
  df_datim <- map_dfr(.x = target_files,
    .f = ~readr::read_csv(.x))  
  
  df_datim <- 
    df_datim %>% 
    group_by(orgunituid, mech_code, fy, indicator) %>% 
    summarise_at(vars(starts_with("mer")), sum, na.rm = TRUE) %>% 
    ungroup()    
  
  df_datim_tx_curr <- df_datim %>% 
    filter(indicator == "TX_CURR") %>% 
    mutate(results_to_targets = case_when(
      mer_results > 0 & mer_targets == 0 ~ "results but no targets",
      mer_results > 0 & mer_targets > 0  ~ "results and targets",
      mer_results == 0 & mer_targets > 0 ~ "no results but targets",
      mer_results == 0 & mer_targets == 0 ~ "no results, no targets"
    ))

  df_datim_tx_curr %>% count(results_to_targets)
  
# LOAD AND MUNGE HFR FROM NIGERIA -----------------------------------------

                          
  # Load NGA HFR data
  hfr <- 
    read_excel(here(data_in, "HFR_Weekly_Performance_Data.xlsx")) %>% 
    #filter(Data %in% ind_order) %>% 
    arrange(State, Period) %>% 
    mutate(countryname = "Nigeria",
           state = sub('...', "", State),
           date = as.Date(Period, "%m/%d/%y")) %>% 
    rename(partner = `HFR Partners`,
           orgunituid = `Facility UID`,
           mech_code = `Mechanism ID`, 
           indicator = Data, 
           value = Value,
           orgunit = `Organisation unit`) %>% 
    filter(!is.na(mech_code)) %>% 
    mutate(mech_code = gsub("(^[[:space:]]*)|([[:space:]]*$)", "", mech_code),
           indicator = gsub("HFR-", "", indicator)) %>% 
    arrange(orgunituid, indicator, date)
    
   # Fixing duplicates, flagging KP partners and flagging site transitions 
    hfr <- 
      hfr %>% 
      group_by_all() %>% 
      mutate(rowcount = n()) %>% 
      distinct_all(.keep_all = FALSE) %>% 
      ungroup() %>% 
      dplyr::select(-rowcount) %>% 
      group_by_at(vars(-value)) %>% 
      mutate(rowcount2 = n()) %>%
      summarise(value = max(value, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(kp_partner = case_when(
        partner == "SFH KP Care 2" ~ 1,
        partner == "Heartland Alliance KP Care 1" ~ 1,
        TRUE ~ 0
      )) %>% 
      group_by(orgunit, orgunituid, date, indicator) %>% 
      mutate(site_count_period = n()) %>% 
      group_by(orgunit, orgunituid, indicator) %>% 
      mutate(multiple_entry_flag = max(site_count_period)) %>% 
      ungroup() %>% 
      arrange(orgunituid, orgunit, date, indicator, mech_code) %>% 
      mutate(fix_flag = if_else(multiple_entry_flag == 2 & site_count_period == 2, 1, 0))
    
    hfr %>% filter(fix_flag == 1)
    
    # DUPLICATE SITES PER INDICATOR AND WEEK
    #We need to flag sites that have multiple entries  
    # Summary of the errors in dataset by indicator
    hfr %>% filter(multiple_entry_flag == 2, site_count_period == 2) %>% 
      count(date, indicator) %>%
      spread(date, n)
    
   # Summary of the sites with multiple indicator entries by period of reporting and indicator
     hfr %>% filter(multiple_entry_flag == 2, site_count_period == 2) %>% 
      count(state, date, indicator, orgunituid, orgunit) %>% 
      spread(date, n) %>% 
       write_csv(., here(data_out, "NGA_dates_with_problems_by_indicator.csv"), na = "")
    
    
    hfr %>% 
        group_by(orgunit, orgunituid, date, indicator) %>% 
        mutate(site_count_period = n()) %>% 
        group_by(orgunit, orgunituid, indicator) %>% 
        mutate(multiple_entry_flag = max(site_count_period)) %>% 
        group_by(state, indicator, orgunit, orgunituid, partner, mech_code, date, multiple_entry_flag) %>% 
        summarise(value = sum(value, na.rm = TRUE)) %>% 
        spread(date, value)%>% 
        write_csv(., here(data_out, "NGA_HFR_count_all_wide.csv"))
  
  # Number of sites reporting by week
  hfr %>% 
    filter(indicator == "TX_CURR") %>%  
    group_by(date, indicator) %>% 
    summarise(total = sum(value, na.rm = TRUE),
              row_count = n()) %>% 
    ggplot(aes(date, row_count)) + 
    geom_vline(xintercept = nga_first, size = 2, colour = grey40k, alpha = 0.80) +
    geom_vline(xintercept = as.Date("2020-03-30"), size = 2, colour = grey40k, alpha = 0.80) +
    geom_line(colour = grey40k) +
    geom_point(aes(fill = row_count), size = 12, shape = 21, colour = grey80k) +
    si_style_xline() + 
    geom_text(aes(label = row_count)) +
    scale_fill_viridis_c(option = "A", begin = .55, end = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")  +
    theme(legend.position = "none",
          axis.text.y = element_blank()) +
    labs(x = NULL, y = NULL, 
         title = "Indicator TX_CURR: The number of sites submitting data spiked on September 30th, 2019 & March 30th, 2020", subtitle = "Circles represent the number of sites reported into the HFR database, per week",
         caption = "Source: Nigeria HFR Weekly Data")
    
  ggsave(file.path(viz_out, paste0("NGA_TX_CURR_Site_submission_count", ".png")),
         plot = last_plot(), dpi = 320, width = 10, height = 5.625, device = "png",
         scale = 1.35)

  
  hfr %>% 
    count(state, partner, date, mech_code, indicator) %>% 
    spread(date, n) %>% 
    write_csv(., here(data_out, "NGA_HFR_count_all_indic_wide.csv"))
  

# HFR DATA UPDATE & MUNGING RESPONSE --------------------------------------

  # So Nigeria HFR data is a unique case where you have partner shifts occuring during the year.
  # MSH transitioned sites in 5 States (Niger, Kwara, Kebbi, Sokoto & Zamfara) to ChemonicsTO1 in Q2
  # FHI360 SIDHAS transitioned sites to FHI360 TO2 (EDO, Bayelsa & Lagos) in March 2020. IN HFR
  # TO2 will start reporting April 1st.
  # FHI360 SIDHAS is transiting sites to CHEMONICS TO3 in other states with reporting starting from April 1st.
  
  # DECISION: given these site shifts, the analysis at the IP level is likely not useful. Instead, we propose aalyzing site level data and remain ambivalent about who is implementing. We will include partner_shift flags accordingly to track shifts. We also will check for duplicate reporting within a site with a partner shift. There is no need to balance the panel out, instead we will work with an unbalanced panel and those sites with missing data are assumed to be non-reporting.
  
  date_seq <- hfr %>% distinct(date) %>% mutate(period = row_number())
  
  
  
  
  
  
  
  
  
  # Expand the dates for each facility so we can look at long term patterns of submission
  # Create flags for when data was submitted and create an interaction to capture mech + indicator for plotting
  # Count the number of missing submission periods so we can order plots accordingly
  # fix one problem child:
  # 2019-10-21	ak Akwa-Ibom State	Mercy Hospital	mVf7HZsDkRn	81858	TMEC-RISE	HTS_TST	93	Nigeria	Akwa-Ibom State
  

  
  hfr_balanced <- 
    hfr %>% 
    group_by_at(vars(-value)) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    group_by(orgunituid, indicator, mech_code, orgunit, partner) %>% 
    mutate(submissions = n()) %>% 
    ungroup() %>% 
    complete(date, nesting(orgunituid, mech_code, State, orgunit,
                           partner, indicator, countryname, state), fill = list(value = NA_real_)) %>% 
    group_by(orgunituid, indicator, mech_code, orgunit, partner) %>% 
    mutate(submit_flag = if_else(!is.na(value), 1, 0)) %>% 
    ungroup() %>% 
    arrange(mech_code, orgunituid, indicator, date) %>% 
    mutate(mech_indicator = interaction(mech_code, indicator, sep = "_") %>% as.character(),
           mech_code = as.numeric(mech_code)) %>% 
    group_by(orgunituid, indicator, mech_code, orgunit, partner, ) %>% 
    mutate(missing_count = sum(is.na(value))) %>% 
    ungroup() %>% 
    mutate(no_reporting = if_else(missing_count == 32, 1, 0)) %>% 
    left_join(., date_seq) %>% 
    group_by_at(vars(orgunituid, mech_code, partner, indicator, state, orgunit)) %>% 
    mutate(total = sum(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(all_zeros = if_else(total == 0, 1, 0))
  
  # Determine where NAs stop and where the mechanism started reporting, this is true reporting completeness point
  
    
  # Review for completeness
  missing_mechs <-hfr %>% filter(is.na(mech_code)) %>% count(orgunit, orgunituid) %>% select(-n)
  unique(hfr$state)
  unique(hfr$indicator)
  
  # Check that it's fixed (gsub checks beginning and end)
  utf8::utf8_print(unique(hfr$indicator), utf8 = FALSE)
  





    # Where is HFR missing mech_code, what facilityUIDs?
  tmp <- hfr %>% filter(indicator %in% c("TX_CURR", "TX_NEW")) %>%  left_join(., df_datim)
  

# PLOT and EDA ------------------------------------------------------------

  # TODO: Figure out when a mechanism "starts" and "ends" if this is the case
  # 
  
  
  # Group by 9 IPs (mech_id) acrosss 17 states - but it looks like many mechnanisms had transitions
  # Look at IPs across states by indicator
  # Look at indicators overall
  
  # Plots to make
  # By week, what is the overall completeness? By mech_code? by geography?
  # Dot plot showing completeness metric for all sites in a mech
  
  
  # Heatmap to familiarize with each mechanism
  heat_plot <- function(mech_indicator) {
    ip <- hfr_balanced %>% filter(mech_indicator == {{mech_indicator}}) %>% 
      count(partner) %>% select(-n) %>% pull() 
    
    hfr_balanced %>% 
      filter(mech_indicator == {{mech_indicator}}) %>% 
      mutate(site_order = fct_reorder(orgunituid, missing_count)) %>% 
      ggplot(aes(x = date, y = site_order, fill = log(value+1))) +
      geom_tile(colour = "white", size = 0.25) +
      geom_vline(xintercept = nga_first, size = 1, colour = "#b2182b")+
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y",
                   limits = as.Date(c("2019-09-30", "2020-05-15"))) +
      scale_fill_viridis_c(alpha = 0.25, na.value = grey50k, direction = -1, option = "B") +
      si_style_xline() +
      labs(title = paste0(ip, " ", {{mech_indicator}}),
           y = NULL, x = NULL) +
      theme(legend.position = "none")
    
    ggsave(file.path(viz_out, paste0(ip, "_", {{mech_indicator}}, "_hfr", ".png")),
            plot = last_plot(), dpi = 320, width = 10, height = 5.625, device = "png",
           scale = 2)
    
    return(last_plot())
  }
  
  
  heat_plot(mech_indicator = "18655_TX_CURR")
  unique(hfr_balanced %>% filter(indicator %in% c("TX_CURR")) %>% select(mech_indicator)) %>% 
    pull() %>% 
    walk(heat_plot)
  
  

  # What does this look like across states?
  state_plot <- function(state) {
  
    hfr_balanced %>% 
    filter(state == {{state}} & indicator == "TX_CURR") %>%
    count(orgunituid, partner, state, orgunit) %>% prinf()
      
    hfr_balanced %>% 
    filter(state == {{state}} & indicator == "TX_CURR") %>%
    mutate(site_order = fct_reorder(orgunituid, missing_count)) %>% 
    ggplot(aes(x = date, y = site_order, fill = log(value+1))) +
    geom_tile(colour = "white", size = 0.25) +
      facet_wrap(~partner) +
    geom_vline(xintercept = nga_first, size = 1, colour = "#b2182b")+
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y",
                 limits = as.Date(c("2019-09-30", "2020-05-15"))) +
    scale_fill_viridis_c(alpha = 0.25, na.value = grey50k, direction = -1, option = "B") +
    si_style_xline() 
    
    ggsave(file.path(viz_out, paste0({{state}}, "_hfr", ".png")),
           plot = last_plot(), dpi = 320, width = 10, height = 5.625, device = "png",
           scale = 2)
  }
    
  # No TX_CURR in Anambra
  unique(hfr_balanced$state[hfr_balanced$state != "Anambra state"]) %>% 
    walk(state_plot)
  
  
  hfr_balanced %>% 
    filter(state == "Anambra State" & indicator == "TX_CURR") %>%
    count(orgunituid, partner, state, orgunit) %>% prinf()
  
  
  hfr %>% 
    filter(state ==  & indicator == "TX_CURR") %>%
    mutate(site_order = fct_reorder(orgunituid, missing_count)) %>% 
    
  
  
  

  
  
 
 df_indic %>% 
   filter(indicator %in% ind_order) %>% 
   group_by(indicator, partner) %>% 
   summarise(val = sum(results, na.rm = TRUE)) %>% 
   spread(period, val) %>% 
   ungroup() %>% 
   mutate(change = Q2 - Q1, 
          direction = if_else(change > 0, "up", "down"))
  
  
  
 
 
 
    
  

