
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
#agency category helper function 
agency_category_fast<-function(df){
  df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "USAID/WCF", "USAID",
                                             ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                    ifelse(`Agency Category` == "Dedup", "Dedup","Other"))))) %>% 
    dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
}
#interaction type helper function
interaction_type_fast <- function(df){
  df<-df %>% dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery",
                                          "NSD"= "Non Service Delivery"))
}

COP22_master_clean <- function(df) {
  df <- df %>%
  #dplyr::select( -c('Operating Unit')) %>% 
  dplyr::filter(`Total Planned Funding` !=0) %>%
  dplyr::mutate_at(vars(`COP Budget New Funding`),~replace_na(.,0))%>%
  dplyr::mutate_at(vars(`COP Budget Pipeline`),~replace_na(.,0)) %>%  #%>% 
  #dplyr::select('Planning Cycle':'Total Planned Funding','Data Stream', 'Agency Category', 'Cross-Cutting Attribution':'Commodity Unit Cost', 'Earmark') %>% 
  #dplyr::mutate(`Program Area`= recode (`Program Area`, "c&T"= "C&T")) #%>% 
  dplyr::rename("Country" = `Operating Unit`)
  
  
}
COP22_master_clean_WHR <- function(df) {
  df <- df %>%
    dplyr::rename("Country" = `Operating Unit`) %>% 
    #dplyr::select( -c('Operating Unit')) %>% 
    dplyr::filter(`Total Planned Funding` !=0) %>%
    dplyr::mutate_at(vars(`COP Budget New Funding`),~replace_na(.,0))%>%
    dplyr::mutate_at(vars(`COP Budget Pipeline`),~replace_na(.,0)) %>% 
    dplyr::select('Planning Cycle':'Total Planned Funding','Data Stream', 'Agency Category', 'Cross-Cutting Attribution':'Commodity Unit Cost', 'Earmark') %>% 
    dplyr::mutate(`Program Area`= recode (`Program Area`, "c&T"= "C&T")) #%>% 

  
  
}
glimpse(df)
#By first creating the functions, it will allow us to, later on, clean up/transform the 
#datasets according to variables that are needed to bind the data.

#creating intervention function based on the SCM of the FAST
FAST_Intervention<-function(df){
  #nested read_csv. Can be removed and run separately
  
  #recode values to match naming in Financial Integrated Dataset
  df<- read_xlsx(df)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::select( -c('Award Number','Agency: Is Indigenous Partner?':'Initiative',
                                'Appropriation Year','Prime Partner DUNS',
                                'Funding Category','GAP':'ESF', 
                                'Water':'AB/Y Denominator'))%>%
    dplyr::rename("Prime Partner Name" = `Prime Partner`,
                  #"Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  #"Prime Partner Type" = `Partner Org Type`,
                  #"Fiscal Year" = `Implementation Year`,
                  #"COP Year" = `Planning Cycle`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Targeted Beneficiary" = `Major Beneficiary`,
                  #"Sub Beneficiary" = `Minor Beneficiary`,
                  "COP Budget New Funding" = `Total New Funding Sources`,
                  "COP Budget Pipeline" = `Applied Pipeline Amount`,
                  "Mechanism ID" = `Mechanism Identifier`) %>% 
    
    #create data stream  
    dplyr::mutate(`Data Stream`="FSD")   
  
  #Convert columns into characters and numeric
  df<-df%>%
    dplyr::mutate_at(vars(`Planning Cycle`: `Cost Type`), funs(as.character)) %>% 
    dplyr::mutate_at(vars(`COP Budget New Funding`:`Total Planned Funding`), funs(as.numeric))
  
  #remove N/A's
  #Drop all rows without an OU specified 
  df <- df %>%  drop_na('Operating Unit')
  
  #replace NAs with 0s
  df<-df%>%
    mutate_at(vars(`COP Budget New Funding`:`Total Planned Funding`),~replace_na(.,0))
  
  
  #recode values to match naming in Financial Integrated Dataset
  df<- df %>% interaction_type_fast() 
  #Add in agency category column to group agencies
  df<-df %>% agency_category_fast()
  
  return(df)
}

#Clean the FAST SCM tab using the function for Cross-Cutting Attribution + Earmark data.
FAST_CCA<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::select( -c('Award Number','Agency: Is Indigenous Partner?':'Initiative',
                                'Appropriation Year','Prime Partner DUNS',
                                'Funding Category','GAP':'ESF',
                                "Total New Funding Sources":"Total Planned Funding",
                                "COVID Applied Pipeline":"AB/Y Denominator"))%>%
    dplyr::rename("Prime Partner Name" = `Prime Partner`,
                  #"Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  #"Prime Partner Type" = `Partner Org Type`,
                  #"Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Targeted Beneficiary" = `Major Beneficiary`,
                  #"Sub Beneficiary" = `Minor Beneficiary`,
                  "Mechanism ID" = `Mechanism Identifier`) %>% 
                  #create data stream  
    dplyr::mutate(`Data Stream`="FAST Cross Cutting Attribution")
  
  #remove N/A's
  df <- df %>%drop_na(`Operating Unit`)
  
  #replace NAs with 0s
  df<-df%>%
    mutate_at(vars(`Water`:`Climate - Sustainable Landscapes`),~replace_na(.,0))
  
  
  #Using pivot long to shift CCA vertically
  df <- df %>% pivot_longer(cols = `Water`:`Climate - Sustainable Landscapes`,
                            names_to = "Cross-Cutting Attribution",
                            values_to = "Total Planned Funding")
  
  #Convert columns into characters and numeric
  df<-df%>%
    dplyr::mutate_at(vars(`Planning Cycle`:`Targeted Beneficiary`), funs(as.character)) 
  
  #Add in agency category column to group agencies
  df<-df %>% agency_category_fast()
  
  #recode values to match naming in Financial Integrated Dataset
  df<- df %>% interaction_type_fast() 
  
  return(df)
}

#Clean the FAST SCM tab using the function for Initiative data. 
#****Note that this is not by funding account since the FAST doesn't have applied pipeline by funding account*****
FAST_Initiative<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df)
  
  #include columns of interest
  df<- df %>% dplyr::select(-c('Award Number','Agency: Is Indigenous Partner?':'Prime Partner Org Type',
                               'Appropriation Year','Prime Partner DUNS',
                               'Funding Category','GAP':'ESF', 
                               'Water':'AB/Y Denominator'))
  
  #Rename to match BUDGET_ER_MER Dataset
  df<- df %>% dplyr::rename("Prime Partner Name" =`Prime Partner`, 
                            #"Is Indigenous Prime Partner" =`Is Indigenous Partner?`,
                            #"Prime Partner Type" =`Partner Org Type`,
                            "Initiative Name" =`Initiative`,
                            #"Fiscal Year"= `Implementation Year`, 
                            "Program Area" = `Major Program`,
                            "Sub Program Area" = `Sub Program`,
                            "Interaction Type" = `SD/NSD`,
                            "Targeted Beneficiary" =`Major Beneficiary`,
                            #"Sub Beneficiary" =`Minor Beneficiary`,
                            "New Funding" =`Total New Funding Sources`,
                            "Applied Pipeline" =`Applied Pipeline Amount`,
                            "Mechanism ID" = `Mechanism Identifier`)
  #Pivot COP Budget New Funding & COP Budget Pipeline to 'funding_type' with value as 'Total Planned Funding'
  df <- df %>% gather(`Funding Type`,`Total Planned Funding`, `New Funding`:`Applied Pipeline`)
  
  #Pivot GAP, GHP-STATE, GHP-USAID to 'funding_account' with value as 'COP Budget New Funding'
  #df <- df %>% gather(funding_account,`COP Budget New Funding`, `GAP`:`GHP-USAID`)
  
  #Create variable 'Data stream' with Initiative
  df <- df %>% dplyr::mutate(`Data Stream`="FAST Initiative") #consider renaming to specify FAST
  
  #Convert columns into characters and numeric
  df<-df%>%
   dplyr::mutate_at(vars(`Planning Cycle`:`Funding Type`), funs(as.character)) 
  
  #Replace NAs in numeric columns
  df <- df %>% dplyr::mutate_at(vars(`Total Planned Funding`),~replace_na(.,0))
  
  #Drop all rows without an OU specified 
  df <- df %>%  drop_na('Operating Unit') 
  
  #Add in agency category column to group agencies
  df<-df %>% agency_category_fast()
  
  #recode values to match naming in Financial Integrated Dataset
  df<- df %>% interaction_type_fast() 
  
  return(df)
}

FAST_Earmarks_IM<-function(df){
  df<-read_xlsx(df)
  #Drop columns you don't need and rename  
  df<- df %>%
    dplyr::select(-c('Award Number','Agency: Is Indigenous Partner?':'Initiative',
                     'Appropriation Year','Prime Partner DUNS',
                     "Total New Funding Sources":"Total Planned Funding",
                     'Funding Category','GAP':'ESF', 
                     'Water':'COVID Applied Pipeline'))%>%
    
    dplyr::rename("Prime Partner Name" = `Prime Partner`,
                  #"Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  # "Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Targeted Beneficiary" = `Major Beneficiary`,
                  #"Sub Beneficiary" = `Minor Beneficiary`,
                  "Mechanism ID" = `Mechanism Identifier`) %>% 
  
  dplyr::mutate(`Data Stream`="FAST Earmark")
  
  df <- df %>%   
    dplyr::mutate_at(vars(`Planning Cycle`: `Cost Type`), funs(as.character)) 
  
  #remove N/A's
  #df <- df %>%  
  df <- df %>%drop_na(`Operating Unit`)
  
  #replace NAs with 0s
  df <- df %>%  
    mutate_at(vars(`C&T Earmark`:`AB/Y Denominator`),~replace_na(.,0))
  
  #Using pivot long to shift Earmarks to long
  df <- df %>%
    pivot_longer(cols = `C&T Earmark`:`AB/Y Denominator`,
                 names_to = "Earmark",
                 values_to = "Total Planned Funding") 
  
  #Convert Earmark budget into numeric
  df <- df %>%
    dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`))
  
  #Add in agency category column to group agencies
  df<-df %>% agency_category_fast()
  
  #recode values to match naming in Budget-ER-MER Dataset
  df<- df %>% interaction_type_fast() 
  
  return(df)
}



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
