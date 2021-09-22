# PURPOSE: Munge and Analysis of
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-01
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

# LOAD OPU Data DATA ============================================================================  
    fast<-"~/Data/Uganda_COP21_FAST_OPU_V2.xlsx"
    df_commodities_1<-get_commodities(fast)
    df_cross_cutting<-get_cross_cutting(fast) 
    df_earmarks<-get_im_earmarks(fast)
    df_initiative<-get_initiative(fast) 
    df_funding_account<-get_funding_account(fast)
    df_intervention<-get_intervention(fast)
    df_mech_list<-get_mech_list(fast) 
    df_ou_earmarks<-get_ou_earmarks(fast)
    
    df_commodities<- left_join(df_commodities_1, df_mech_list, by = "Mechanism ID")
    
    # MUNGE OPU data============================================================================
    df_in_fa<-full_join(df_initiative,df_funding_account)
    df_opu_dataset<-bind_rows(df_intervention,df_cross_cutting,df_commodities, df_earmarks,df_initiative,df_funding_account, df_ou_earmarks)  %>%
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
      dplyr::rename(interaction_type="Interaction Type")%>%
      dplyr::mutate(
        interaction_type = case_when(
          `Program Area` == "PM"~ "PM",
          TRUE ~ `interaction_type` ))
      
    
    df_opu_dataset<-df_opu_dataset %>%
      dplyr::mutate("Report Type"="OPU")
    
    df_opu_dataset$`Prime Partner Name`=paste(df_opu_dataset$`Prime Partner Name`,"-",df_opu_dataset$`Report Type`)
    df_opu_dataset$`Agency Category`=paste(df_opu_dataset$`Agency Category`,"-",df_opu_dataset$`Report Type`)
    df_opu_dataset$`Funding Agency`=paste(df_opu_dataset$`Funding Agency`,"-",df_opu_dataset$`Report Type`)
    df_opu_dataset$`Operating Unit`=paste(df_opu_dataset$`Operating Unit`,"-",df_opu_dataset$`Report Type`)
    df_opu_dataset$`Mechanism Name`=paste(df_opu_dataset$`Mechanism Name`,"-",df_opu_dataset$`Report Type`)
    df_opu_dataset$`Earmark`=paste(df_opu_dataset$`Earmark`,"-",df_opu_dataset$`Report Type`)
    
    countrys_to_filter<-df_opu_dataset%>%
      dplyr::distinct(Country)%>%
      pull()
      
   
    
   
# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

