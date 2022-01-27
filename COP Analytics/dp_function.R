
#might not need all of these
library(tameDP) #version 3.2.4
library(purrr)
library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(gt)
library(glue)
library(webshot)
library(dplyr)
library(devtools)
library(tidyr)
library(gisr)
library(scales)
library(glitr)

#COP22 Datapack Function
  #the first 2 steps, reading in files, mapping dataframe are in the direct code#
 
  datapack_im_tab<-function(df){
    df_all<-get_names(df_all) %>% 
     df_dp<- df_all %>%
    get_names() %>% 
   # df_datapack() %>% 
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
    dplyr::mutate(`Fiscal Year` = "2023",
                  `Data Stream` = "MER",
                  `Planning Cycle`="COP22") %>%
    dplyr::mutate(`Agency Category` = `Funding Agency`) %>% 
    dplyr::mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                             ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                    ifelse(`Agency Category` =="Dedupe adjustments Agency","Dedup","Other")))) %>% 
    dplyr::mutate(`Program Area` = dplyr::case_when(Indicator == "HTS_TST_POS" ~ "HTS",
                                                    Indicator == "HTS_TST" ~ "HTS",
                                                    Indicator == "TX_CURR" ~ "C&T",
                                                    Indicator == "TX_NEW" ~ "C&T",
                                                    Indicator == "PrEP_CT" ~ "PREV",
                                                    Indicator == "PrEP_NEW" ~ "PREV",
                                                    Indicator == "VMMC" ~ "PREV")) 
  
  return(df)
}