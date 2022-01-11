
#might not need all of these
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
