## PROJECT:  COVID ANALYTICS
## AUTHOR:   T. Essam | USAID
## LICENSE:  MIT
## PURPOSE:  Munge and plot Nigeria COVID milestones
## NOTE:     
## DATE:     2020-04-2
## UPDATED:  2020-04-27




# ANALYTICAL PLOTS --------------------------------------------------------

  #BIG PICTURE FIRST
  hfr%>% 
    filter(indicator %in% ind_order, indicator != "PrEP_NEW") %>% 
    group_by(indicator, date) %>% 
    summarise(value = sum(value, na.rm = TRUE),
      n = n()) %>% 
  group_by(indicator) %>% 
  mutate(y = (mean(value, na.rm = TRUE) *.10)) %>% 
  ungroup() %>% 
    ggplot(aes(x = date, y = value)) +
    #geom_vline(xintercept = df_who$date, size = 2, color = grey10k) +
    geom_vline(xintercept = nga_first, size = 2, colour = grey20k, alpha = 0.80) +
    geom_vline(xintercept = nga_holiday, colour = grey20k, size = 2, alpha = 0.80) +
    geom_vline(xintercept = as.Date("2020-03-30"), size = 2, colour = grey20k, alpha = 0.80) +  
    geom_area(fill = grey10k, alpha = 0.65) + geom_line(colour = grey80k) +
    geom_point(aes(y = y, 
      fill = case_when(
      n > 500            ~ "#e5b9ad", 
      n > 400 & n <= 500 ~ "#b1c7b3", 
      TRUE               ~ "#f1eac8")
      ),  
      shape = 21, size = 6, stroke = 0.1, colour = "white")+
    geom_text(aes(label = n, y = y), size = 2)+
    facet_wrap(~indicator, scales = "free") +
    scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")  +
  scale_fill_identity() +
    labs(x = NULL, y = NULL, 
         title = "WHY IT MATTERS? DUPLICATED DATA GIVE WRONG CONCLUSION, MASK POTENTIAL GAINS",
      subtitle = "Aggregated numbers for selected indicators (includes duplicates).",
         caption = "Source: Nigeria HFR Weekly Data") +
    si_style_ygrid() +
  theme(legend.position = "null",
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(2, "lines")) 

ggsave(file.path(viz_out, paste0("NGA_WHY_IT_MATTERS_Plot", ".png")),
  plot = last_plot(), dpi = 320, width = 10, height = 5.625, device = "png",
  scale = 1.4)


#009392,#72aaa1,#b1c7b3,#f1eac8,#e5b9ad,#d98994,#d0587e


hfr_balanced %>% 
  filter(indicator %in% ind_order) %>% 
  filter(indicator == "TX_CURR") %>% 
  group_by(indicator, date, state) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(state_sort = fct_reorder(state, value, .desc = TRUE)) %>% 
  ggplot(aes(x = date, y = value)) +
  #geom_vline(xintercept = df_who$date, size = 2, color = grey10k) +
  geom_vline(xintercept = nga_first, size = 2, colour = grey20k) +
  geom_vline(xintercept = nga_holiday, colour = "#b2182b", size = 2, alpha = 0.25) +
  geom_area(fill = grey10k, alpha = 0.65) + geom_line() +
  facet_wrap(~state_sort, scales = "free") +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = NULL, 
       title = "TX_CURR trends across states",
       caption = "Source: Niger HFR data") +
  si_style()

