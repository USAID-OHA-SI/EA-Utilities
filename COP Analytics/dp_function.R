
#might not need all of these
library(tameDP) #version 5.1.0 for COP23

 
#COP23 Datapack Function- COP23 updates country, funding_agency and prime_partner_name are named differently this year
  #the first 2 steps, reading in files, mapping dataframe are in the direct code#

  datapack_im_tab<-function(df){
     df<- df %>%
    get_names() %>% 
    dplyr::filter(standardizeddisaggregate != "KeyPop/HIVStatus") %>% 
    dplyr::filter(standardizeddisaggregate != "KeyPop/Result") %>%
    dplyr::filter(standardizeddisaggregate != "KeyPop") %>%
    dplyr::filter(standardizeddisaggregate != "Age/Sex/DREAMS") %>% 
    dplyr::filter(standardizeddisaggregate != "Age/Sex/Preventive") %>% 
    dplyr::group_by(operatingunit, country, funding_agency, mech_code, prime_partner_name, mech_name, indicator, fiscal_year, numeratordenom) %>%
    dplyr::summarise(targets = sum(targets, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(`Operating Unit` = operatingunit,
                  `Country` = country,
                  `Funding Agency` = funding_agency,
                  `Mechanism ID` = mech_code,
                  `Prime Partner Name` = prime_partner_name,
                  `Mechanism Name` = mech_name,
                  `Fiscal Year` = fiscal_year,
                  Indicator = indicator,
                  Target = targets) %>%
   
    dplyr::mutate(`Fiscal Year` = "2024",
                  `Data Stream` = "MER",
                  `Planning Cycle`="COP23")
    
    #df<-df %>% agency_category_fast()
 
    df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
           mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                             ifelse(`Agency Category` == "USAID/WCF", "USAID",
                                                    ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                           ifelse(`Agency Category` == "Dedup","Dedupe", "Other"
                                                                 ))))) %>% 
           dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
      
       
    df<-df %>%
      dplyr::mutate(`Program Area` = dplyr::case_when(Indicator == "HTS_TST_POS" ~ "HTS",
                                                    Indicator == "HTS_TST" ~ "HTS",
                                                    Indicator == "TX_CURR" ~ "C&T",
                                                    Indicator == "TX_NEW" ~ "C&T",
                                                    Indicator == "PrEP_CT" ~ "PREV",
                                                    Indicator == "PrEP_NEW" ~ "PREV",
                                                    Indicator == "VMMC_CIRC" ~ "PREV")) 
  
  return(df)
  } 
  