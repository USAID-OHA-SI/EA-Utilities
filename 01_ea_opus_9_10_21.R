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
    library(readxl)
    library(tameDP)
    library(gophr)
    
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  source("99_utilities.R")

# LOAD DATA ============================================================================  
#load the baseline FAST you are working with here
    opu_fast<-"~/Data/Uganda_COP21_FAST_OPU_V2.xlsx"
    baseline_fast<-"~/Data/Uganda_COP21_FAST_Final_20 May 2021_12 pm.xlsx"
    
    
  df_commodities_1<-get_commodities(opu_fast)
  df_cross_cutting<-get_cross_cutting(opu_fast) 
  df_earmarks<-get_im_earmarks(opu_fast)
  df_initiative<-get_initiative(opu_fast) 
  df_funding_account<-get_funding_account(opu_fast)
  df_intervention<-get_intervention(opu_fast)
  df_mech_list<-get_mech_list(opu_fast) 
  df_ou_earmarks<-get_ou_earmarks(opu_fast)
  
  df_commodities<- left_join(df_commodities_1, df_mech_list, by = "Mechanism ID")

# MUNGE OPU data============================================================================
  df_in_fa<-full_join(df_initiative,df_funding_account)
  df_opu_fast<-bind_rows(df_intervention,df_cross_cutting,df_commodities, df_earmarks,df_initiative,df_funding_account, df_ou_earmarks)  %>%
    dplyr::filter(`Total Planned Funding` !=0) %>%
    dplyr::mutate_at(vars(`COP Budget New Funding`),~replace_na(.,0))%>%
    dplyr::mutate_at(vars(`COP Budget Pipeline`),~replace_na(.,0)) %>% 
    dplyr::mutate(
      `Agency Category` = case_when(
        `Funding Agency` == "USAID/WCF"~ "USAID",
        TRUE ~ `Agency Category` )) %>% 
    dplyr::select('Planning Cycle':'Total Planned Funding','Data Stream', 'Agency Category', 'Cross-Cutting Attribution':'Commodity Unit Cost','Funding Category':'Funding Account', 'Earmark') %>% 
    dplyr::mutate(`Program Area`= recode (`Program Area`, "c&T"= "C&T")) %>% 
    dplyr::mutate(`Country` = `Operating Unit`) %>%
    dplyr::rename(interaction_type="Interaction Type")
    #dplyr::rename("Country" = `Operating Unit`)
  
  
# Munge DP OPU data ============================================================================
   df_dp_opu<-tame_dp("~/Data/v91_Data Pack_UG_20210525 1614.xlsx")
  
  #add in operating unit
  #df_all<-get_names(df_all)
  
  
  #tailor Datapack to append to FAST wrangling    
  df_dp_opu<- df_dp_opu %>%
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
  

# Bind ============================================================================
  df_opu_dataset<-bind_rows(df_dp_opu,df_opu_fast) %>%
    dplyr::mutate("Report Type"="OPU")
    
    #df_opu_dataset<-df_opu_dataset %>%
    #dplyr::mutate("Report Type"="OPU")
  
  #df_opu_dataset$`Prime Partner Name`=paste(df_opu_dataset$`Prime Partner Name`,"-",df_opu_dataset$`Report Type`)
  #df_opu_dataset$`Agency Category`=paste(df_opu_dataset$`Agency Category`,"-",df_opu_dataset$`Report Type`)
  #df_opu_dataset$`Funding Agency`=paste(df_opu_dataset$`Funding Agency`,"-",df_opu_dataset$`Report Type`)
  #df_opu_dataset$`Operating Unit`=paste(df_opu_dataset$`Operating Unit`,"-",df_opu_dataset$`Report Type`)
  #df_opu_dataset$`Mechanism Name`=paste(df_opu_dataset$`Mechanism Name`,"-",df_opu_dataset$`Report Type`)
  #df_opu_dataset$`Earmark`=paste(df_opu_dataset$`Earmark`,"-",df_opu_dataset$`Report Type`)
  
 countrys_to_filter<-df_opu_dataset%>%
   dplyr::distinct(Country)%>%
   pull()
 df_opu_dataset<-df_opu_dataset%>%
   dplyr::mutate(
     interaction_type = case_when(
       `Program Area` == "PM"~ "PM",
       TRUE ~ `interaction_type` ))
 
 #write.csv(df_opu_dataset, paste0(countrys_to_filter, "OPU file_finance_testing",Sys.Date(), ".csv"))
  
 # Current baseline FAST data ============================================================================
 #fast<-"~/Data/Uganda_COP21_FAST_OPU_V2.xlsx"
 df_commodities_1<-get_commodities(baseline_fast)
 df_cross_cutting<-get_cross_cutting(baseline_fast)
 df_earmarks<-get_im_earmarks(baseline_fast)
 df_initiative<-get_initiative(baseline_fast)
 df_funding_account<-get_funding_account(baseline_fast)
 df_intervention<-get_intervention(baseline_fast)
 df_mech_list<-get_mech_list(baseline_fast) 
 df_ou_earmarks<-get_ou_earmarks(baseline_fast)
 
 df_commodities<- left_join(df_commodities_1, df_mech_list, by = "Mechanism ID")
 df_fast_baseline<-bind_rows(df_intervention,df_cross_cutting,df_commodities, df_earmarks,df_initiative,df_funding_account, df_ou_earmarks)  %>%
   dplyr::filter(`Total Planned Funding` !=0) %>%
   dplyr::mutate_at(vars(`COP Budget New Funding`),~replace_na(.,0))%>%
   dplyr::mutate_at(vars(`COP Budget Pipeline`),~replace_na(.,0)) %>% 
   dplyr::mutate(
     `Agency Category` = case_when(
       `Funding Agency` == "USAID/WCF"~ "USAID",
       TRUE ~ `Agency Category` )) %>% 
   dplyr::select('Planning Cycle':'Total Planned Funding','Data Stream', 'Agency Category', 'Cross-Cutting Attribution':'Commodity Unit Cost','Funding Category':'Funding Account', 'Earmark') %>% 
   dplyr::mutate(`Program Area`= recode (`Program Area`, "c&T"= "C&T")) %>% 
   dplyr::mutate(`Country` = `Operating Unit`) %>%
   dplyr::rename(interaction_type="Interaction Type")



#=======get MER baseline data from original datapack (or potentially MSD-TBD)
 df_dp_baseline<-tame_dp("Data/v91_Data Pack_UG_20210525 1614.xlsx")
#df_dp_baseline<-df_dp_opu


#tailor Datapack to append to FAST wrangling    
 df_dp_baseline<- df_dp_baseline %>%
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

# bind sets=================================================================================
 df_baseline_final<-bind_rows(df_fast_baseline,df_dp_baseline)%>%
   dplyr::mutate(
     interaction_type = case_when(
       `Program Area` == "PM"~ "PM",
       TRUE ~ `interaction_type` ))%>%
   dplyr::mutate("Report Type"="Baseline Fast")
 
 df_combined_final<-bind_rows(df_baseline_final,df_opu_dataset)
 
 write.csv(df_combined_final, paste0(countrys_to_filter, "OPU file_FAST_DP-",Sys.Date(), ".csv"))