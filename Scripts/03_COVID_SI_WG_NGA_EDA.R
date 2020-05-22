## PROJECT:  COVID ANALYTICS
## AUTHOR:   T. Essam | USAID
## LICENSE:  MIT
## PURPOSE:  Munge and plot Nigeria COVID milestones
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
    filter(indicator %in% ind_order, indicator != "PrEP_NEW") %>% 
    group_by(indicator, date) %>% 
    summarise(value = sum(value, na.rm = TRUE),
      n = n())%>% 
  group_by(indicator) %>% 
  mutate(y = (mean(value, na.rm = TRUE) *.10),
    pct_change = lag_calc(value, lag(value, order_by = date)),
    max_change = min(pct_change, na.rm = TRUE)) %>% prinf()
 
 %>% 
  ungroup() %>% 
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
hfr%>% 
    filter(indicator =="TX_CURR") %>% 
    filter(fix_flag != 1) %>% 
    group_by(indicator, date) %>% 
    summarise(value = sum(value, na.rm = TRUE),
      n = n()) %>% 
    group_by(indicator) %>% 
    mutate(y = 10000) %>% 
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
        TRUE               ~ "#f1eac8")
    ),  
      shape = 21, size = 8, stroke = 0.1, colour = "white")+
    geom_text(aes(label = n, y = y), size = 3)+
    scale_y_continuous(labels = comma) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")  +
    scale_fill_identity() +
    labs(x = NULL, y = NULL, 
      title = "TX_CURR: USING NIGERIA HFR DATA TO MONITOR TREATMENT TRENDS",
      subtitle = "Line graph shows TX_CURR weekly levels. Circles reflect the number of sites reporting.",
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

ggsave(file.path(viz_out, paste0("NGA_TX_CURR_Plot", ".png")),
  plot = last_plot(), dpi = 320, width = 9.5, height = 3.75, device = "png",
  scale = 1)


# VIZ BY STATES - LARGEST DROP IN PARTNERS REPORTING ----------------------
hfr%>% 
  filter(indicator =="TX_CURR") %>% 
  group_by(indicator, date, state) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = date, y = n, fill = n)) +
    geom_step() + facet_wrap(~state)

