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
  source("GitHub/EA-Utilities/99_utilities.R")

# LOAD DATA ============================================================================  
#load the FAST you are working with here
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

# MUNGE Financeial Data ============================================================================
  df_in_fa<-full_join(df_initiative,df_funding_account)
  df_opu_dataset<-bind_rows(df_intervention,df_cross_cutting,df_commodities, df_earmarks,df_initiative,df_funding_account, df_ou_earmarks)  %>%
    dplyr::filter(`Total Planned Funding` !=0) %>%
    dplyr::mutate_at(vars(`COP Budget New Funding`),~replace_na(.,0))%>%
    dplyr::mutate_at(vars(`COP Budget Pipeline`),~replace_na(.,0)) %>% 
    dplyr::mutate(
      `Agency Category` = case_when(
        `Funding Agency` == "USAID/WCF"~ "USAID",
        TRUE ~ `Agency Category` )) %>% 
    dplyr::select('Planning Cycle':'Total Planned Funding','Data Stream', 'Agency Category', 'Cross-Cutting Attribution':'Commodity Unit Cost', 'Earmark') %>% 
    dplyr::mutate(`Program Area`= recode (`Program Area`, "c&T"= "C&T")) %>% 
    dplyr::mutate(`Country` = `Operating Unit`) %>%
    dplyr::rename(interaction_type="Interaction Type")
    #dplyr::rename("Country" = `Operating Unit`)
  
  
# Munge DP data ============================================================================
#ignore this for now
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
  

# Bind ============================================================================
  df_opu_dataset<-bind_rows(df_dp,df_opu_dataset) %>%
    dplyr::mutate("Report Type"="OPU")%>%
    
    df_opu_dataset<-df_opu_dataset %>%
    dplyr::mutate("Report Type"="OPU")
  
  df_opu_dataset$`Prime Partner Name`=paste(df_opu_dataset$`Prime Partner Name`,"-",df_opu_dataset$`Report Type`)
  df_opu_dataset$`Agency Category`=paste(df_opu_dataset$`Agency Category`,"-",df_opu_dataset$`Report Type`)
  df_opu_dataset$`Funding Agency`=paste(df_opu_dataset$`Funding Agency`,"-",df_opu_dataset$`Report Type`)
  df_opu_dataset$`Operating Unit`=paste(df_opu_dataset$`Operating Unit`,"-",df_opu_dataset$`Report Type`)
  df_opu_dataset$`Mechanism Name`=paste(df_opu_dataset$`Mechanism Name`,"-",df_opu_dataset$`Report Type`)
  
 countrys_to_filter<-df_opu_dataset%>%
   dplyr::distinct(Country)%>%
   pull()
 df_opu_dataset<-df_opu_dataset%>%
   dplyr::mutate(
     interaction_type = case_when(
       `Program Area` == "PM"~ "PM",
       TRUE ~ `interaction_type` ))
  
 # Current data ============================================================================
 df_fsd<-si_path()%>%
   return_latest("COP17")%>%
   gophr::read_msd()%>%
   filter(planning_cycle=="COP21")%>%
   mutate("Data Stream"="FSD")
 
 df_comp<-si_path()%>%
   return_latest("Comprehensive")%>%
   gophr::read_msd()%>%
   filter(planning_cycle=="COP21")%>%
   mutate("Data Stream"="Initiative")
 
 df_comd<-si_path()%>%
   return_latest("Commodities")%>%
   gophr::read_msd()%>%
   filter(planning_cycle=="COP21")%>%
   mutate("Data Stream"="Commodities")%>%
   dplyr::rename(cop_budget_total = item_budget)%>%
   dplyr::mutate(cop_budget_total=as.double(cop_budget_total))%>%
   dplyr::mutate_at(vars(cop_budget_total),~replace_na(.,0))
 
 df_current<-bind_rows(df_fsd,df_comp,df_comd)%>%
   dplyr::filter(operatingunit %in% countrys_to_filter)
 
 
 
 df_current<-gophr::read_msd
 df_current<-read.csv("~/Data/FY21Q3/FY21Q3i MSD.FSD 23 August 2021.csv")
 df_current<-df_current%>%
   dplyr::filter(Operating.Unit %in% countrys_to_filter)%>%
   dplyr::filter(COP.Year>=2021)%>%
   clean_names()
 
 df_current<-df_current%>%
   dplyr::rename(`Operating Unit` = operatingunit,
                 `Country` = countryname,
                 `Funding Agency` = fundingagency,
                 `Mechanism ID` = mech_code,
                 `Prime Partner Name` = primepartner,
                 `Mechanism Name` = mech_name,
                 `Fiscal Year` = fiscal_year,
                 #Indicator = indicator,
                 #Target = targets,
                 #`Agency Category`=agency_category,
                 `Program Area`=program,
                 `Sub Program Area`=sub_program,
                 `Beneficiary`=beneficiary,
                 `Sub Beneficiary`=sub_beneficiary,
                 #`Data Stream`=data_stream,
                 `Total Planned Funding`= cop_budget_total,
                 `COP Year`=planning_cycle,
   `Initiative Name`=initiative_name,
   `Record Type`=record_type,
   `Is Indigenous Prime Partner`=is_indigenous_prime_partner,
   `Prime Partner Type`=prime_partner_org_type,
   `Funding Type`=funding_type,
   `Funding Account`=funding_account,
  `Funding Category`= funding_category,
  `COP Budget New Funding`= cop_budget_new_funding,
  `COP Budget Pipeline`= cop_budget_pipeline,
  `Commodity Item`= commodity_item,
  `Major Category` = major_category,
  `Minor Category` = minor_category,
  
   )%>%
   dplyr::mutate(
     interaction_type = case_when(
       `Program Area` == "PM"~ "PM",
       TRUE ~ `interaction_type` ))
 
 df_current<-df_current%>%
   dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`),
                 `Fiscal Year`=as.character(`Fiscal Year`))
                 
                 
                 
                 
df_opu_final<-bind_rows(df_current, df_opu_dataset)%>%
  dplyr::select(-c(appropriation_year, award_number,workplan_budget_amt, expenditure_amt,subrecipient_duns,award_number,procurement_type,
                   `Is Indigenous Prime Partner`, `Prime Partner Type`, prime_partner_duns))

#write.csv(,"Uganda.OPU {sys.csv")
write.csv(df_opu_final, paste0(countrys_to_filter, "OPU file_finance", format(Sys.Date.(), "%d-%b-%Y"), ".csv"))



##=================================================
#using SCM as base dataset

df_scm<-si_path()%>%
  return_latest("COPMatrix")%>%
  read.csv()%>%
  clean_names()

df_initiative<-df_scm%>%
  df<- df %>%  dplyr::select('Planning Cycle','Operating Unit':'Partner Name', 'Mechanism Name':'Initiative', 'Implementation Year':'Minor Beneficiary', 'Total New Funding Sources':'Applied Pipeline Amount')  %>%
  #Rename to match BUDGET_ER_MER Dataset
  dplyr::rename("Prime Partner Name" =`Partner Name`, 
                "Is Indigenous Prime Partner" =`Is Indigenous Partner?`,
                "Prime Partner Type" =`Partner Org Type`,
                "Initiative Name" =`Initiative`,
                "Fiscal Year"= `Implementation Year`, 
                "Program Area" = `Major Program`,
                "Sub Program Area" = `Sub Program`,
                "Interaction Type" = `SD/NSD`,
                "Beneficiary" =`Major Beneficiary`,
                "Sub Beneficiary" =`Minor Beneficiary`,
                "New Funding" =`Total New Funding Sources`,
                "Applied Pipeline" =`Applied Pipeline Amount`,) %>%
  
  #Pivot COP Budget New Funding & COP Budget Pipeline to 'funding_type' with value as 'Total Planned Funding'
  gather(`Funding Type`,`Total Planned Funding`, `New Funding`:`Applied Pipeline`) %>%
  
  #Pivot GAP, GHP-STATE, GHP-USAID to 'funding_account' with value as 'COP Budget New Funding'
  # gather(funding_account,`COP Budget New Funding`, `GAP`:`ESF`) %>%
  
  #Create variable 'Data stream' with Initiative
  dplyr::mutate(`Data Stream`="Initiative")  %>% #consider renaming to specify FAST
  
  #Convert columns into characters
  dplyr::mutate(`Initiative Name`= as.character(`Initiative Name`)) %>% 
  dplyr::mutate(`Funding Category`= as.character(`Funding Category`)) %>% 
  dplyr::mutate(`Mechanism Name`=as.character(`Mechanism Name`)) %>% 
  dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
  dplyr::mutate(`Operating Unit`= as.character(`Operating Unit`)) %>% 
  dplyr::mutate(`Prime Partner Name`=as.character(`Prime Partner Name`)) %>% 
  dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`)) %>% 
  dplyr::mutate(`Planning Cycle`=as.character(`Planning Cycle`)) %>% 
  dplyr::mutate(`Interaction Type`=as.character(`Interaction Type`)) %>% 
  dplyr::mutate(`Record Type`=as.character(`Record Type`)) %>% 
  dplyr::mutate(`Program Area`=as.character(`Program Area`)) %>% 
  dplyr::mutate(`Sub Program Area`=as.character(`Sub Program Area`)) %>% 
  dplyr::mutate(`Beneficiary`=as.character(`Beneficiary`)) %>% 
  dplyr::mutate(`Sub Beneficiary`=as.character(`Sub Beneficiary`)) %>% 
  dplyr::mutate(`Funding Agency`=as.character(`Funding Agency`)) %>% 
  dplyr::mutate(`Is Indigenous Prime Partner`= as.character(`Is Indigenous Prime Partner`)) %>% 
  dplyr::mutate(`Prime Partner Type`= as.character(`Prime Partner Type`)) %>% 
  
  #Convert  budget into numeric
  dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`)) %>% 
  
  #Replace NAs in numeric columns
  dplyr::mutate_at(vars(`Total Planned Funding`),~replace_na(.,0))%>%
  
  
  #Drop all rows without an OU specified 
  drop_na('Operating Unit')   %>%
  
  #recode values for different variables as needed
  dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery")) %>%
  dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "NSD"= "Non Service Delivery"))  %>%
  
  
  #Add in agency category column to group agencies
  dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
  mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                    ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                           ifelse(`Agency Category` =="Dedup", "Dedup","Other")))) %>% 
  dplyr::mutate(`Agency Category`= as.character(`Agency Category`))


  filter(fiscal_year=="2021")%>%
  mutate("Data Stream"="MER")
#=======get MER data (on hold till 2022 targets are included)
dp<-"~/GitHub/EA-Utilities/Data/Mozambique_20210513_213731.xlsx"
df_dp_opu<-tame_dp(dp)



#tailor Datapack to append to FAST wrangling    
df_dp_opu<- df_dp_opu %>%
  dplyr::filter(disagg != "KeyPop") %>%
  dplyr::group_by(operatingunit, countryname, fundingagency, mech_code, primepartner, mech_name, indicator, fiscal_year, numeratordenom) %>%
  dplyr::summarise(targets = sum(targets, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
#  dplyr::rename(`Operating Unit` = operatingunit,
  #              `Country` = countryname,
   #             `Funding Agency` = fundingagency,
    #            `Mechanism ID` = mech_code,
     #           `Prime Partner Name` = primepartner,
      #          `Mechanism Name` = mech_name,
       #         `Fiscal Year` = fiscal_year,
        #        Indicator = indicator,
         #       Target = targets) %>%
  df_dp<-df_dp%>%
  dplyr::mutate(`fiscal_year` = "2022",
                `Data Stream` = "MER",
                `Planning Cycle`="COP21") %>%
  dplyr::mutate(`agency_category` = `fundingagency`) %>% 
  dplyr::mutate(`agency_category` = ifelse(`agency_category` == "USAID", "USAID",
                                           ifelse(`agency_category` == "HHS/CDC", "CDC",
                                                  ifelse(`agency_category` =="Dedupe adjustments Agency","Dedup","Other")))) %>% 
  dplyr::mutate(`program_area` = dplyr::case_when(indicator == "HTS_TST_POS" ~ "HTS",
                                                  indicator == "HTS_TST" ~ "HTS",
                                                  indicator == "TX_CURR" ~ "C&T",
                                                  indicator == "TX_NEW" ~ "C&T",
                                                  indicator == "PrEP_CURR" ~ "PREV",
                                                  indicator == "PrEP_NEW" ~ "PREV",
                                                  indicator == "VMMC" ~ "PREV")) 

df_dp_opu<-df_opu_dataset %>%
  dplyr::mutate("Report Type"="OPU")

df_opu_dataset$`Prime Partner Name`=paste(df_opu_dataset$`Prime Partner Name`,"-",df_opu_dataset$`Report Type`)
df_opu_dataset$`Agency Category`=paste(df_opu_dataset$`Agency Category`,"-",df_opu_dataset$`Report Type`)
df_opu_dataset$`Funding Agency`=paste(df_opu_dataset$`Funding Agency`,"-",df_opu_dataset$`Report Type`)
df_opu_dataset$`Operating Unit`=paste(df_opu_dataset$`Operating Unit`,"-",df_opu_dataset$`Report Type`)
df_opu_dataset$`Mechanism Name`=paste(df_opu_dataset$`Mechanism Name`,"-",df_opu_dataset$`Report Type`)

