




# ANALYTICAL PLOTS --------------------------------------------------------

  #BIG PICTURE FIRST
  hfr_balanced %>% 
    filter(indicator %in% ind_order) %>% 
    group_by(indicator, date) %>% 
    summarise(value = sum(value, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = date, y = value)) +
    #geom_vline(xintercept = df_who$date, size = 2, color = grey10k) +
    geom_vline(xintercept = nga_first, size = 2, colour = grey20k) +
    geom_vline(xintercept = nga_holiday, colour = "#b2182b", size = 2, alpha = 0.25) +
    geom_area(fill = grey10k, alpha = 0.65) + geom_line() +
    facet_wrap(~indicator, scales = "free") +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = NULL, 
         title = "TX_CURR trends across states",
         caption = "Source: Niger HFR data") +
    si_style()



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

