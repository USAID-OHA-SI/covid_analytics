## PROJECT:  COVID ANALYTICS
## AUTHOR:   T. Essam | USAID
## LICENSE:  MIT
## PURPOSE:  Plot COVID-19 miilestones with HFR treatment indicators
## NOTE:     
## DATE:     2020-04-2
## UPDATED:  2020-04-27


# LIBRARIES ---------------------------------------------------------------

  library(tidyverse)
  library(scales)
  library(cartodo)
  library(ggridges)



# GLOBALS -----------------------------------------------------------------

 #CartoDB colors to pick from
scales::show_col(c("#009392", "#72aaa1", "#b1c7b3", "#f1eac8", "#e5b9ad", "#d98994", "#d0587e"))
 dup_color <- "#e5b9ad"
 norm_color <- "#b1c7b3"
 drop_color <- "#f1eac8"

 
# ANALYTICAL PLOTS --------------------------------------------------------

  #BIG PICTURE FIRST - trends across time
  hfr_rollup%>% 
    filter(indicator %in% ind_order, indicator != "PrEP_NEW")%>% 
    group_by(indicator, date) %>% 
    summarise(value = sum(value, na.rm = TRUE),
      n = n())%>% 
  group_by(indicator) %>% 
  mutate(y = (mean(value, na.rm = TRUE) *.10),
    pct_change = lag_calc(value, lag(value, order_by = date)),
    max_change = min(pct_change, na.rm = TRUE)) %>% 
  ungroup() %>% prinf()
   
    ggplot(aes(x = date, y = value)) +
    #geom_vline(xintercept = df_who$date, size = 2, color = grey10k) +
    geom_vline(xintercept = nga_first, size = 2, colour = grey20k, alpha = 0.80) +
    geom_vline(xintercept = nga_holiday, colour = grey20k, size = 2, alpha = 0.80) +
    geom_vline(xintercept = as.Date("2020-03-30"), size = 2, colour = grey20k, alpha = 0.80) +  
    geom_area(fill = grey10k, alpha = 0.85) + geom_line(colour = grey80k) +
    geom_point(aes(y = y, 
      fill = case_when(
      n > 500            ~ dup_color, 
      n > 440 & n <= 500 ~ norm_color, 
      TRUE               ~ drop_color)
      ),  
      shape = 21, size = 6, stroke = 0.1, colour = "white")+
    geom_text(aes(label = n, y = y), size = 2)+
    facet_wrap(~indicator, scales = "free") +
    scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")  +
  scale_fill_identity() +
    labs(x = NULL, y = NULL, 
         title = "NIGERIA: SITE REPORTING AND INDICATOR LEVELS ARE DOWN SINCE COVID-19 LOCKDOWN",
      subtitle = "Aggregated numbers for selected indicators.",
         caption = "Source: Nigeria HFR Weekly Data") +
    si_style_ygrid() +
  theme(legend.position = "null",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(2, "lines")) 

ggsave(file.path(viz_out, paste0("NGA_WHY_IT_MATTERS_Plot", ".png")),
  plot = last_plot(), dpi = 320, width = 10, height = 5.625, device = "png",
  scale = 1.4)

# -------------------------------------------------------------------------------
# Focusing just on TX_CURR for presentation
hfr_rollup %>% 
    filter(indicator =="TX_CURR", state %in% c("Akwa-Ibom State", "Cross River State", "Lagos State")) %>% 
    group_by(indicator, date, state) %>% 
    summarise(value = sum(value, na.rm = TRUE),
      n = n()) %>% 
    group_by(indicator, state) %>% 
    mutate(y = 3000) %>% 
    ungroup() %>% 
    ggplot(aes(x = date, y = value)) +
    #geom_vline(xintercept = df_who$date, size = 2, color = grey10k) +
    geom_vline(xintercept = nga_first, size = 2, colour = grey20k, alpha = 0.80) +
    geom_vline(xintercept = nga_holiday, colour = grey20k, size = 2, alpha = 0.80) +
    geom_vline(xintercept = as.Date("2020-03-30"), size = 2, colour = grey20k, alpha = 0.80) +  
    geom_area(fill = grey10k, alpha = 0.85) + geom_line(colour = grey80k) +
    geom_point(aes(y = y, 
      fill = case_when(
        n > 500            ~ "#e5b9ad", 
        n > 440 & n <= 500 ~ "#b1c7b3", 
        TRUE               ~ "#b1c7b3")
    ),  
      shape = 21, size = 8, stroke = 0.1, colour = "white")+
    geom_text(aes(label = n, y = y), size = 3)+
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")  +
    scale_fill_identity() +
  facet_wrap(~state, ncol = 1, scales = "free_y") +
    labs(x = NULL, y = NULL, 
      title = "TX_CURR: USING NIGERIA HFR DATA TO MONITOR TREATMENT TRENDS",
      subtitle = "Line graph shows TX_CURR weekly levels. Circles reflect the number of sites reporting. \n",
      caption = "Source: Nigeria HFR Weekly Data") +
    si_style_ygrid() +
    # annotate(geom="text", x = as.Date("2019-12-23"),
    #   y = 330000, label="Holiday", colour = grey50k, family = "SourceSansPro-Regular", size = 4, hjust = "right") +
    # annotate(geom = "curve", x = as.Date("2019-12-19"), y = 325000, xend = as.Date("2019-12-24"), yend = 310000,
    #   curvature = 0.5, arrow = arrow(length = unit(2, "mm")), colour = grey50k)+
    # annotate(geom = "text", x = as.Date("2020-02-20"),
    #   y = 330000, label = "First Confirmed \n COVID-19 Case", colour = grey50K, 
    #   family = "SourceSansPro-Regular", size = 4) +
    # annotate(geom="text", x = as.Date("2020-4-1"),
    #   y = 330000, label="Lockdown", colour = grey50k, family = "SourceSansPro-Regular", size = 4, hjust = "left") +
    theme(legend.position = "null",
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(2, "lines"))

  ggsave(file.path(viz_out, paste0("NGA_TX_CURR_Plot_states", ".png")),
    plot = last_plot(), dpi = 320,  width = 10, height = 5.625, device = "png",
    scale = 1.25)
  
  

# TX_NET_NEW by SELECT STATES ---------------------------------------------
  hfr_rollup %>% 
    filter(indicator =="TX_CURR", state %in% c("Akwa-Ibom State", "Cross River State", "Lagos State")) %>% 
    
    group_by(indicator, date, state) %>% 
    summarise(value = sum(value, na.rm = TRUE),
      n = n()) %>% 
    arrange(state, date) %>% 
    group_by(state) %>% 
    mutate(tx_net_new = value - lag(value, order_by = date),
      y = -9000) %>% 
    ungroup() %>% 
    ggplot(aes(x = date, y = tx_net_new)) +
    #geom_vline(xintercept = df_who$date, size = 2, color = grey10k) +
    geom_vline(xintercept = nga_first, size = 2, colour = grey20k, alpha = 0.80) +
    geom_vline(xintercept = nga_holiday, colour = grey20k, size = 2, alpha = 0.80) +
    geom_vline(xintercept = as.Date("2020-03-30"), size = 2, colour = grey20k, alpha = 0.80) +  
    geom_col(aes(fill = if_else(tx_net_new > 0, "#8ba68a", "#a68a8b")))  +
    facet_wrap(~state, ncol = 1) +
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")  +
    scale_fill_identity() +
    facet_wrap(~state, ncol = 1, scales = "free_y") +
    labs(x = NULL, y = NULL, 
      title = "TX_NET_NEW: USING NIGERIA HFR DATA TO MONITOR TREATMENT TRENDS",
      subtitle = "Length of the bar reprsents TX_NET_NEW on a weekly basis. \n",
      caption = "Source: Nigeria HFR Weekly Data") +
    si_style_ygrid() +
    theme(legend.position = "null",
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(2, "lines"))
  
  ggsave(file.path(viz_out, paste0("TX_NET_NEW_Plot_states", ".png")),
    plot = last_plot(), dpi = 320,  width = 10, height = 5.625, device = "png",
    scale = 1.25)  
  
  
# VIZ BY STATES - LARGEST DROP IN PARTNERS REPORTING ----------------------

  # Set the total number of sites to appear 1 week after max date
  # Was going to use this at the end of a heatmap
  max_sites <- hfr_rollup %>% filter(indicator == "TX_CURR") %>% 
    group_by(indicator, date, state) %>% 
    summarise(n = n()) %>% 
    group_by(indicator, state) %>% 
    summarise(max = max(n, na.rm = TRUE),
      date = max(date, na.rm = TRUE) + 8) 


# By State
  hfr_rollup %>% 
    filter(indicator =="TX_CURR") %>% 
    group_by(indicator, date, state) %>% 
    summarise(n = n()) %>% 
    group_by(indicator, state) %>% 
    mutate(max = max(n, na.rm = TRUE), 
      max_date = max(date)) %>% 
    ungroup() %>% 
    mutate(reporting_rate = n / max)%>% 
    group_by(state, indicator) %>% 
    mutate(ave_reporting = (sum(n) / sum(max))) %>%
    mutate(state_pct = paste0(state, " ", round(ave_reporting * 100, 0),"%", "\n", "(", max, " sites)")) %>% 
    ungroup() %>% 
    mutate(state_pct = fct_reorder(state_pct, max, .desc = TRUE)) %>% 
    ggplot(aes(x = date, y = n)) + geom_col(aes(y = max), fill = grey10k, alpha = 0.75) +
    geom_vline(xintercept = as.Date("2020-03-30"), linetype = "dashed", colour = grey50k) +
      geom_col(aes(fill = if_else(.data$state %in% c("Kano State", "Jigawa State"), "#b1c7b3", "#d8e3d8"))) + 
    geom_errorbar(aes(x=date, ymin=n, ymax=n), size=0.5, width=5, colour = grey50k) +
   scale_fill_identity() +
    # geom_smooth(se = FALSE, span = 0.5,, size = 1.5, colour = grey50k) + 
    facet_wrap(~state_pct, scales = "free_y", ncol = 4) +
      #labeller = label_wrap_gen(width = 20, multi_line = TRUE)) +
  si_style_xline() + theme(axis.text.y = element_blank())  +
    labs(x = NULL, y = NULL, 
      title = "TX_CURR: SITE REPORTING RATES HAVE DECLINED THE MOST IN KANO STATE AND JIGAWA STATE SINCE LOCKDOWN",
      subtitle = "State level site reporting rates by period, ordered from largest site count to smallest \n",
      caption = "Nigeria HFR Weekly Data")
    
  
  ggsave(file.path(viz_out, paste0("NGA_mechanism_completion_rates", ".png")),
    plot = last_plot(), dpi = 320, width = 10, height = 5.625, device = "png",
    scale = 1.25)
  


# COMPLETENESS BY ALL KEY VARIABLES ---------------------------------------
  hfr_rollup %>% 
    filter(indicator == "HTS_TST") %>% 
    group_by(indicator, date, state) %>% 
    summarise(n = n()) %>% 
    group_by(indicator, state) %>% 
    mutate(max = max(n, na.rm = TRUE), 
      max_date = max(date)) %>% 
    ungroup() %>% 
    mutate(reporting_rate = n / max)%>% 
    group_by(state, indicator) %>% 
    mutate(ave_reporting = (sum(n) / sum(max))) %>%
    mutate(state_pct = paste0(state, " ", round(ave_reporting * 100, 0),"%", "\n", "(", max, " sites)")) %>% 
    ungroup() %>% 
    mutate(state_pct = fct_reorder(state_pct, max)) %>% 
    ggplot(aes(x = date, y = state_pct, fill = reporting_rate)) +
    geom_tile(colour = "white", size = 0.25) +
    geom_text(aes(label = if_else(reporting_rate <= 0.75, paste0(percent(round(reporting_rate, 2), 1)), NA_character_)),
      colour = "white") +
    scale_fill_viridis_c(option = "A", alpha = 0.85, direction = 1, end = .95) +
    si_style_nolines()
  
  
  
#  COMPLETENESS BY MECH ---------------------------------------------------
  hfr_rollup %>% 
    filter(indicator =="TX_CURR") %>% 
    group_by(indicator, date, mech_code, partner) %>% 
    summarise(n = n()) %>% 
    group_by(indicator, mech_code, partner)%>% 
    mutate(max = max(n, na.rm = TRUE), 
      max_date = max(date)) %>% 
    ungroup() %>% 
    mutate(reporting_rate = n / max)%>% 
    group_by(mech_code, indicator) %>% 
    mutate(ave_reporting = (sum(n) / sum(max))) %>%
    mutate(mech_pct = paste0(partner, "\n", mech_code, " ", 
      round(ave_reporting * 100, 0),"%", "\n", "(", max, " sites)")) %>% 
    ungroup()  %>% 
    mutate(state_pct = fct_reorder(mech_pct, max, .desc = TRUE)) %>% 
    ggplot(aes(x = date, y = n)) + geom_col(aes(y = max), fill = grey10k, alpha = 0.75) +
    geom_vline(xintercept = as.Date("2020-03-30"), linetype = "dashed", colour = grey50k) +
    geom_col(aes(fill = "#d8e3d8")) + 
    geom_errorbar(aes(x=date, ymin=n, ymax=n), size=0.5, width=5, colour = grey50k) +
    scale_fill_identity() +
    # geom_smooth(se = FALSE, span = 0.5,, size = 1.5, colour = grey50k) + 
    facet_wrap(~state_pct, scales = "free_y", ncol = 4) +
    #labeller = label_wrap_gen(width = 20, multi_line = TRUE)) +
    si_style_xline() + theme(axis.text.y = element_blank())  +
    labs(x = NULL, y = NULL, 
      title = "TX_CURR: SITE REPORTING RATES HAVE DECLINED THE MOST IN KANO STATE AND JIGAWA STATE SINCE LOCKDOWN",
      subtitle = "State level site reporting rates by period, ordered from largest site count to smallest \n",
      caption = "Nigeria HFR Weekly Data")
  
  ggsave(file.path(viz_out, paste0("NGA_mech_completion_rates", ".png")),
    plot = last_plot(), dpi = 320, width = 10, height = 5.625, device = "png",
    scale = 1.25) 
  
  

# COMPLETENESS BAR GRAPH FOR STATES---------------------------------------------------------------

  # By State
  hfr_rollup %>% 
    filter(indicator =="TX_CURR" & date >= "2020-3-1") %>% 
    group_by(indicator, date, state) %>% 
    summarise(n = n()) %>% 
    group_by(indicator, state) %>% 
    mutate(max = max(n, na.rm = TRUE), 
      max_date = max(date)) %>% 
    ungroup() %>% 
    mutate(reporting_rate = n / max)%>% 
    group_by(state, indicator) %>% 
    mutate(ave_reporting = (sum(n) / sum(max))) %>%
    mutate(state_pct = paste0(state, " ", round(ave_reporting * 100, 0),"%", "\n", "(", max, " sites)")) %>% 
    ungroup() %>% 
    filter(date == max_date) %>% 
    mutate(state_pct = fct_reorder2(state_pct, max, -ave_reporting)) %>% 
    ggplot(aes(y= ave_reporting, x = state_pct)) +
    geom_col(aes(y = 1), fill = grey10k, alpha = 0.75) +
    geom_col(aes(fill = if_else(.data$state %in% c("Kano State", "Jigawa State"), "#b1c7b3", "#d8e3d8"))) + 
    geom_errorbar(aes(x = state_pct, ymin = ave_reporting, ymax = ave_reporting), size=0.5, width = 0.8, colour = grey50k) +
    coord_flip() +
    scale_fill_identity() +
    scale_y_continuous(labels = scales::percent) +
    si_style_xline() +
    labs(x = NULL, y = NULL, 
      title = "TX_CURR: SITE REPORTING RATES HAVE DECLINED THE MOST IN KANO STATE AND JIGAWA STATE SINCE LOCKDOWN",
      subtitle = "State level site reporting rates since March 1st, ordered from largest site count to smallest \n",
      caption = "Nigeria HFR Weekly Data")
   
  
