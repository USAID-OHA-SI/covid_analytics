## PROJECT:  COVID ANALYTICS UGANDA
## AUTHOR:   T. Essam | USAID
## LICENSE:  MIT
## PURPOSE:  Plot COVID-19 miilestones with HFR treatment indicators
## NOTE:     
## DATE:     2020-05_26


# LIBRARIES ---------------------------------------------------------------

  library(tidyverse)
  library(vroom)
  library(glamr)
  library(glitr)
  library(scales)


# GLOBALS -----------------------------------------------------------------



# READ AND MUNGE ----------------------------------------------------------

  hfr_uga <- vroom::vroom(here(data_in, "Uganda_HFR_2020_05_22.csv"))
  names(uga_hfr)
  
  hfr_uga <- 
    hfr_uga %>% 
    rename(orgunit = Outlet,
      orgunituid = `Outlet.uid`,
      primepartner = IM,
      primpartneruid = `IM.uid`,
      indicator_long = `DataElement.name`,
      indicator_uid = DataElementuid,
      disagg = DISAGG,
      disagg_uid = `DISAGG.UID`) %>% 
    separate(`Report period`, into = c("period_start", "period_end"), sep = "/")

  hfr_uga %>% count(disagg) %>% knitr::kable()
  

  