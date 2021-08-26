# PURPOSE: Munge and Analysis of
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-08-26
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(here)
library(janitor)
    
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  source("GitHub/EA-Utilities/99_utilities.R")

# LOAD DATA ============================================================================  

    fast<-"~/GitHub/EA-Utilities/Data/Mozambique_COP21_FAST_Final.xlsx"
  df_commodities_1<-get_commodities(fast)
  df_cross_cutting<-get_cross_cutting(fast) 
  df_earmarks<-get_im_earmarks(fast)
  df_initiative<-get_initiative(fast)  
  df_intervention<-get_intervention(fast)
  df_mech_list<-get_mech_list(fast) 
  df_ou_earmarks<-get_ou_earmarks(fast)
  
  df_commodities<- left_join(df_commodities_1, df_mech_list, by = "Mechanism ID")

# MUNGE Financeial Data ============================================================================
  df_opu_dataset<-bind_rows(df_initiative,df_intervention,df_cross_cutting,df_commodities, df_earmarks)  %>%
    dplyr::filter(`Total Planned Funding` !=0) %>%
    dplyr::mutate_at(vars(`COP Budget New Funding`),~replace_na(.,0))%>%
    dplyr::mutate_at(vars(`COP Budget Pipeline`),~replace_na(.,0)) %>% 
    dplyr::mutate(
      `Agency Category` = case_when(
        `Funding Agency` == "USAID/WCF"~ "USAID",
        TRUE ~ `Agency Category` )) %>% 
    dplyr::select('Planning Cycle':'Total Planned Funding','Data Stream', 'Agency Category', 'Cross-Cutting Attribution':'Commodity Unit Cost', 'Earmark') %>% 
    dplyr::mutate(`Program Area`= recode (`Program Area`, "c&T"= "C&T")) %>% 
    dplyr::rename("Country" = `Operating Unit`)
  
  
# Munge DP data ============================================================================

 dp<-"~/GitHub/EA-Utilities/Data/Mozambique_20210513_213731.xlsx"
   df_dp<-tame_dp(dp)
  
  #add in operating unit
  df_all<-get_names(df_all)
  
  
  #tailor Datapack to append to FAST wrangling    
  df_dp<- df_dp %>%
    dplyr::filter(disagg != "KeyPop") %>%
    dplyr::group_by(operatingunit, countryname, fundingagency, mech_code, primepartner, mech_name, indicator, fiscal_year, numeratordenom) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(`Operating Unit` = operatingunit,
                  `Country` = countryname,
                  `Funding Agency` = fundingagency,
                  `Mechanism ID` = mech_code,
                  `Prime Partner Name` = primepartner,
                  `Mechanism Name` = mech_name,
                  `Fiscal Year` = fiscal_year,
                  Indicator = indicator,
                  Target = targets) %>%
    dplyr::mutate(`Fiscal Year` = "2022",
                  `Data Stream` = "MER",
                  `Planning Cycle`="COP21") %>%
    dplyr::mutate(`Agency Category` = `Funding Agency`) %>% 
    dplyr::mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                             ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                    ifelse(`Agency Category` =="Dedupe adjustments Agency","Dedup","Other")))) %>% 
    dplyr::mutate(`Program Area` = dplyr::case_when(Indicator == "HTS_TST_POS" ~ "HTS",
                                                    Indicator == "HTS_TST" ~ "HTS",
                                                    Indicator == "TX_CURR" ~ "C&T",
                                                    Indicator == "TX_NEW" ~ "C&T",
                                                    Indicator == "PrEP_CURR" ~ "PREV",
                                                    Indicator == "PrEP_NEW" ~ "PREV",
                                                    Indicator == "VMMC" ~ "PREV")) 
  

# SPINDOWN ============================================================================
  df_opu_dataset<-bind_rows(df_dp,df_opu_dataset) %>%
    dplyr::mutate("Report Type"="OPU")
  
  
  df_opu_dataset$`Prime Partner Name`=paste(df_opu_dataset$`Prime Partner Name`,"-",df_opu_dataset$`Report Type`)
  df_opu_dataset$`Agency Category`=paste(df_opu_dataset$`Agency Category`,"-",df_opu_dataset$`Report Type`)
  
 write.csv(df_opu_dataset,"opu_data_set_Mozambique.8.26.2021.csv")
  
