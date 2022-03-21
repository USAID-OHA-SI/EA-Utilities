
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
 glimpse(df_all)
  datapack_im_tab<-function(df){
     df<- df %>%
    get_names() %>% 
    dplyr::filter(standardizeddisaggregate != "KeyPop/HIVStatus") %>% 
    dplyr::filter(standardizeddisaggregate != "KeyPop/Result") %>%
    dplyr::filter(standardizeddisaggregate != "KeyPop") %>%
    dplyr::filter(standardizeddisaggregate != "Age/Sex/DREAMS") %>% 
    dplyr::filter(standardizeddisaggregate != "Age/Sex/Preventive") %>% 
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
                  `Planning Cycle`="COP22")
    
    #df<-df %>% agency_category_fast()
 
    df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
           mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                             ifelse(`Agency Category` == "USAID/WCF", "USAID",
                                                    ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                           ifelse(`Agency Category` == "Dedupe adjustments Agency","Dedupe", "Other"
                                                                 ))))) %>% 
           dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
      
       
    df<-df %>%
      dplyr::mutate(`Program Area` = dplyr::case_when(Indicator == "HTS_TST_POS" ~ "HTS",
                                                    Indicator == "HTS_TST" ~ "HTS",
                                                    Indicator == "TX_CURR" ~ "C&T",
                                                    Indicator == "TX_NEW" ~ "C&T",
                                                    Indicator == "PrEP_CT" ~ "PREV",
                                                    Indicator == "PrEP_NEW" ~ "PREV",
                                                    Indicator == "VMMC" ~ "PREV")) 
  
  return(df)
} 