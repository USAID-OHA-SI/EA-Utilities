# PURPOSE: Pull budgets for partners
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-22
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(gt)
    library(googlesheets4)
    library(glue)
    library(googledrive)
library(fs) #to create folders
    
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
    df_fsd<-si_path()%>%
      return_latest("Fin")%>%
      gophr::read_msd()
    
  # Functions  
    
    
    print_financial <- function(mech){
      df_mech <- df_totals %>% 
        filter(mech_code == mech)
      
      meta <- df_mech %>% 
        distinct(country, mech_code) %>%
        mutate(country = str_remove_all(country, " |'"),
               name = glue("ER21IMFinance/COP20_{country}_{mech_code}_Financial.csv"))
      
      print(glue("Printing...{meta$country}-{meta$mech_code}"))
      
      write_csv(df_mech, file.path(meta$name), na = "")
    }

# LOAD DATA ============================================================================  

  msd

# MUNGE ============================================================================
  #get financial total
    df_totals <- df_fsd %>% 
             filter(fundingagency == "USAID") %>%
      remove_mo()%>%
      filter(planning_cycle =="COP20")%>%
      mutate(country = ifelse(operatingunit == countryname, operatingunit, glue("{operatingunit}-{countryname}")))%>%
    
      mutate("Program Area: Sub Program Area-Service Level"=glue("{program}: {sub_program}-{interaction_type}"))%>%
    
      mutate("Beneficiary-Sub Beneficiary"=glue("{beneficiary}-{sub_beneficiary}"))%>%  
      group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`) %>% 
      summarise_at(vars(cop_budget_total, workplan_budget_amt, expenditure_amt), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      arrange(country, mech_code)
    
    #create output folders folders
    dir_create("ER21IMFinance")
  
    #create output budget files
    walk(mechs, print_financial)

# SPINDOWN ============================================================================

