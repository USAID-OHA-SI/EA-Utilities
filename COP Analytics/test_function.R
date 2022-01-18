
#might not need all of these
library(rmarkdown)
library(stringr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(readxl)
library(here)
library(glamr)
library(purrr)
library(data.table)
#library(googlesheets4)
library(splitstackshape)
#library(googledrive)
library(tameDP)
library(gophr)
library(devtools)

#add Google drive code (possibly)

#COP22 FAST Functions

agency_category_fast<-function(df){
  df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "USAID/WCF", "USAID",
                                             ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                    ifelse(`Agency Category` =="Dedup", "Dedup","Other"))))) %>% 
    dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
}

interaction_type_fast <- function(df){
  df<-df %>% dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery",
                                          "NSD"= "Non Service Delivery"))
}

#creating intervention function based on the SCM of the FAST
FAST_Intervention<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx("C:/Users/jmontespenaloza/Documents/FASTS/Nigeria_COP22_FAST_V1.xlsx", 
                "Standard COP Matrix-R", skip=3)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::select( -c('Global','Prime Partner DUNS','Award Number',
                                'Appropriation Year', 'Initiative',
                                'Funding Category','GAP':'ESF', 
                                'Water':'AB/Y Denominator'))%>%
    dplyr::rename("Prime Partner Name" = `Partner Name`,
                  "Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  "Prime Partner Type" = `Partner Org Type`,
                  "Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Beneficiary" = `Major Beneficiary`,
                  "Sub Beneficiary" = `Minor Beneficiary`,
                  "COP Budget New Funding" = `Total New Funding Sources`,
                  "COP Budget Pipeline" = `Applied Pipeline Amount`) %>% 
  
    #create data stream  
    dplyr::mutate(`Data Stream`="FSD")   
  
  #Convert columns into characters and numeric
  df<-df%>%
    dplyr::mutate_at(vars(`Mechanism ID`, `Fiscal Year`), funs(as.character)) 
    #dplyr::mutate_if(is.double, as.numeric) %>% 
    
    #remove N/A's
    #Drop all rows without an OU specified 
    df <- df %>%  drop_na('Operating Unit')
  
  
  #replace NAs with 0s
  df<-df%>%
    mutate_at(vars(`COP Budget New Funding`:`Total Planned Funding`),~replace_na(.,0))
  
  #Add in agency category column to group agencies
  df<-df %>% agency_category_fast()
  
  #recode values to match naming in Financial Integrated Dataset
  df<- df %>% interaction_type_fast() 
  
  return(df)
}

FAST_CCA<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df,"Standard COP Matrix-R", skip=3)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::select( -c('Global','Prime Partner DUNS','Award Number', 'Cost Type',
                                'Appropriation Year',  'Initiative',
                                'Funding Category','GAP':'Total Planned Funding', 
                                'COVID Adaptation-GHP-State':'Digital Health Investments'))%>%
    dplyr::rename("Prime Partner Name" = `Partner Name`,
                  "Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  "Prime Partner Type" = `Partner Org Type`,
                  "Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Beneficiary" = `Major Beneficiary`,
                  "Sub Beneficiary" = `Minor Beneficiary`) %>%    
  #create data stream  
    dplyr::mutate(`Data Stream`="FAST Cross Cutting Attribution")
  
  #remove N/A's
  df <- df %>%drop_na(`Operating Unit`)
  
  #replace NAs with 0s
  df<-df%>%
    mutate_at(vars(`Water`:`Key Populations: SW`),~replace_na(.,0))
  
  
  #Using pivot long to shift CCA vertically
  df <- df %>% pivot_longer(cols = `Water`:`Key Populations: SW`,
                            names_to = "Cross-Cutting Attribution",
                            values_to = "Total Planned Funding")

  #Convert columns into characters and numeric
  df<-df%>%
    #dplyr::mutate_at(vars(`Mechanism ID`, `Fiscal Year`),as.character(df)) 
    dplyr::mutate_at(vars(`Mechanism ID`, `Fiscal Year`), funs(as.character)) 
 
  
  #Add in agency category column to group agencies
  df<-df %>% agency_category()
  
  #recode values to match naming in Financial Integrated Dataset
  df<-df %>% interaction_type_fast()
  
  return(df)
}

FAST_Initiative<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx("C:/Users/jmontespenaloza/Documents/FASTS/Nigeria_COP22_FAST_V1.xlsx", 
                "Standard COP Matrix-R", skip=3)
  #include columns of interest
  ####change to Deselect!!!!!!WARNING
  df<- df %>% dplyr::select('Planning Cycle','Operating Unit':'Partner Name', 'Mechanism Name':'Initiative', 
                            'Implementation Year':'Minor Beneficiary', 'GAP':'GHP-USAID', 
                            'Total New Funding Sources':'Applied Pipeline Amount') 
  #Rename to match BUDGET_ER_MER Dataset
  df<- df %>% dplyr::rename("Prime Partner Name" =`Partner Name`, 
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
                            "Applied Pipeline" =`Applied Pipeline Amount`)
  
  #Pivot COP Budget New Funding & COP Budget Pipeline to 'funding_type' with value as 'Total Planned Funding'
  df <- df %>% gather(`Funding Type`,`Total Planned Funding`, `New Funding`:`Applied Pipeline`)
  
  #Pivot GAP, GHP-STATE, GHP-USAID to 'funding_account' with value as 'COP Budget New Funding'
  df <- df %>% gather(funding_account,`COP Budget New Funding`, `GAP`:`GHP-USAID`)
  
  #Create variable 'Data stream' with Initiative
  df <- df %>% dplyr::mutate(`Data Stream`="Initiative") #consider renaming to specify FAST
  
  #Convert columns into characters and numeric
  df<-df%>%  dplyr::mutate_at(vars(`Mechanism ID`, `Fiscal Year`), funs(as.character)) %>% 
  
  #Replace NAs in numeric columns
  df <- df %>% dplyr::mutate_at(vars(`Total Planned Funding`),~replace_na(.,0))
  
  #Drop all rows without an OU specified 
  df <- df %>%  drop_na('Operating Unit') 
  
  #recode values for different variables as needed
  df<- df %>% interaction_type_fast()
  
  #Add in agency category column to group agencies
  df<-df %>% agency_category()
  return(df)
}

#pending
FAST_Commodities<-function(df){
  df<-read_xlsx(df, "Commodities-E", skip=3)
  df<- df %>%  
    dplyr::rename("Specify Other Procurement" =`Specify 'Other' Procurement`) %>% 
    dplyr::select( -c('View SPT Item on Commodities-P', 'View Initiative on Initiative-E')) %>% 
   
  #Convert character columns to characters
    dplyr::mutate_at(vars(`Mechanism ID`, `Program Area (Service Delivery Only)`, `Initiative Name`, `Major Category`, `Minor Category`,
                   `Beneficiary`, `Item`,`Item ID`,`Specify Other Procurement`), funs(as.character)) %>% 
    
  #Remove dashes in Facility-based testing and Community-Based Testing temporarily
    dplyr::mutate(`Program Area (Service Delivery Only)`= recode (`Program Area (Service Delivery Only)`, "HTS: Facility-based testing-SD"= "HTS: Facility based testing-SD",
                  "HTS: Community-based testing-SD"= "HTS: Community based testing-SD",
                  "HTS: Facility-based testing-NSD"= "HTS: Facility based testing-NSD",
                  "HTS: Community-based testing-NSD"= "HTS: Community based testing-NSD"))%>%
    dplyr::rename("Program Area" =`Program Area (Service Delivery Only)`) %>%
   
  #Separate out program area, sub program area, beneficiary, sub beneficiary, and interaction type
    separate(col = "Program Area", into=c("Program Area", "Sub Program Area"), sep=":") %>%
    separate(col = "Sub Program Area", into=c("Sub Program Area", "Interaction Type"), sep="-") %>%
    separate(col = "Beneficiary", into=c("Beneficiary", "Sub Beneficiary"), sep=":") %>%
    
  #Rename to match BUDGET_ER_MER Dataset
    dplyr::rename("Program Area" =`Program Area_1`,"Commodity Unit Cost" =`Unit Cost`,
                  "Commodity Unit Price" =`Unit Price`,"Total Planned Funding" =`Total Item Budget`,
                  "Commodity Item" =`Item`,) %>%
    
  #Create new variables
    dplyr::mutate(`Data Stream`="FAST Commodities",`Fiscal Year`="2023",`Planning Cycle`="COP22",
                  `Record Type`="Implementing Mechanism")%>%
    
  #Replace NAs in numeric columns
    dplyr::mutate_at(vars(`Total Planned Funding`),~replace_na(.,0))%>%
    
  #Convert  character columns into Characters
    dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`)) %>% 
    
  #Convert  numeric  columns to numeric
    dplyr::mutate_at(vars(`Commodity Quantity`,`Commodity Unit Price`,
                          `Procurement Management`,`Global Freight`,`Quality Assurance $`,
                          `In Country Logistics $`,  `Procurement Management$`, `Global Freight $`
                          , `Data Quality $`, `Commodity Unit Cost`, `Total Planned Funding`, `Remaining $`), funs(as.numeric)) %>% 
  
  #Drop all rows without a MECH ID specified 
     drop_na('Mechanism ID') %>%
    
  #Recode values for different variables as needed
    df<- df %>% interaction_type_fast()
  #Add dashes back in Facility-based testing and Community-Based Testing 
    dplyr::mutate(`Sub Program Area`= recode (`Sub Program Area`, "Facility based testing"= "Facility-based testing", "Community based testing"= "Community-based testing"))  
  
  return(df)
}

FAST_MECHSLIST<-function(df){
  df<-read_xlsx(df, "Mechs List-R", skip=1)
  df<- df %>%  
    dplyr::select ("OU", "Mechanism ID") %>% 
    dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`))
  return(df)
} 

FAST_Earmarks_IM<-function(df){
  df<-read_xlsx(df, "Standard COP Matrix-R", skip=3)
  #Drop columns you don't need and rename  
  df<- df %>%
    dplyr::mutate_at(vars(`Planning Cycle`: `Cost Type`, `Digital Health Investments`), funs(as.character)) %>%
    
    dplyr::select(-c('Global','Prime Partner DUNS','Award Number',
                               'Appropriation Year',  'Initiative',
                               'Funding Category','GAP':'COVID Adaptation-Applied Pipeline'))%>%
    dplyr::rename("Prime Partner Name" = `Partner Name`,
                  "Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  "Prime Partner Type" = `Partner Org Type`,
                  "Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Beneficiary" = `Major Beneficiary`,
                  "Sub Beneficiary" = `Minor Beneficiary`) %>% 
    dplyr::mutate(`Data Stream`="FAST Earmark") %>% 
  
  #remove N/A's
    drop_na(`Operating Unit`) %>%
  
  #replace NAs with 0s
    mutate_at(vars(`C&T Earmark`:`AB/Y Denominator`),~replace_na(.,0)) %>% 
  
  #Using pivot long to shift Earmarks to long
    pivot_longer(cols = `C&T Earmark`:`AB/Y Denominator`,
                            names_to = "Earmark",
                            values_to = "Total Planned Funding") %>% 
  
  #Convert Earmark budget into numeric
  dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`)) %>%
  
  #recode values to match naming in Budget-ER-MER Dataset
  dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery",
            "NSD"= "Non Service Delivery"))
  return(df)
}

#COP22 Datapack Function


#Additional COP22 Output Datasets
FAST_ESF<-function(df){
  df<-read_xlsx(df, "Standard COP Matrix-R", skip=3)
    df<- df %>%  
      dplyr::select(-c('Water': 'Digital Health Investments'))%>% 
      dplyr::rename("Prime Partner Name" = `Partner Name`,
                                  "Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                                  "Prime Partner Type" = `Partner Org Type`,
                                  "Fiscal Year" = `Implementation Year`,
                                  "Program Area" = `Major Program`,
                                  "Sub Program Area" = `Sub Program`,
                                  "Interaction Type" = `SD/NSD`,
                                  "Beneficiary" = `Major Beneficiary`,
                                  "Sub Beneficiary" = `Minor Beneficiary`,
                                  "COP Budget New Funding" = `Total New Funding Sources`,
                                  "COP Budget Pipeline" = `Applied Pipeline Amount`) %>% 
      dplyr::filter(`ESF`=! 0) %>%
      dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
      drop_na("Initiative Name")
  return(df)
}



#test_functionEA-Utilities\COP Analytics
source("~/GitHub/EA-Utilities/COP Analytics/test_function.R")