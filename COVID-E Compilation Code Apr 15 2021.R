
# Title: FAST COVID-E Tab Data Aggregation
# Author: Vanessa Da Costa & Jairo Montes
# Last updated: "April 19th, 2021"
# Purpose: Compile all COVID-19 data from the COVID-E tab in the COP21 Fast & add Operating Unit, Country, and Funding Agency
#Document needs: Folder of FASTS, df.OU_Country reference file

#Install packages 1 time
install.packages("tidyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("stringr")
install.packages("readxl")
install.packages("here")
install.packages("googlesheets4")
install.packages("remote")
install.packages("data.table")
install.packages("splitstackshape")
install.packages("googledrive")
install.packages("devtools")
devtools::install_github("USAID-OHA-SI/glamr")
devtools::install_github("USAID-OHA-SI/tameDP")
remotes::install_github("ICPI/ICPIutilities")
install.packages("purrr")
install.packages("devtools")


#Run libraries everytime you open R
library(rmarkdown)
library(stringr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(readxl)
library(here)
library(ICPIutilities)
library(glamr)
library(purrr)
library(here)
library(data.table)
library(googlesheets4)
library(splitstackshape)
library(googledrive)


#set your working directory- Session--> Set Working Directory -->Choose Directory (this should go to the folder path before the FASTS folder)
setwd()
here("FASTS")

#Read in Fast files
Fast<-list.files("FASTS",full.names = TRUE)

#Run Function for the Mech List 
FAST_MECHSLIST<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "Mechs List-R")
  #include columns of interest
  df<- df %>%  
    dplyr::select ("Operating Unit", "Funding Agency", "Partner Name","Mechanism Name", "Mechanism ID", "Is Indigenous Partner?") %>% 
    #Rename to match BUDGET_ER_MER Dataset
    dplyr::rename( 
      "Prime Partner Name" =`Partner Name`, 
      "Is Indigenous Prime Partner" =`Is Indigenous Partner?`,
    ) %>%
    
    dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
    
    #Add in agency category column to group agencies
    dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                             ifelse(`Agency Category` =="Dedup", "Dedup","Other"))))
  
  return(df)
} 

#Create MechList Data Frame
#Data frame for Mech List
df.MechsList <- purrr::map_dfr(.x = Fast,
                               .f = ~ FAST_MECHSLIST(.x))

#Run Function for the COVID-E tab
FAST_COVID<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "COVID-E", skip=3)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::rename("Prime Partner Name" = `Partner Name`) 
  
  #Add in agency category column to group agencies
  df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "USAID/WCF", "USAID",
                                             ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                                    ifelse(`Agency Category` =="Dedup", "Dedup","Other"))))) %>% 
    dplyr::mutate(`Agency Category`= as.character(`Agency Category`))%>% 
    dplyr::mutate(`Target completion date of activity`= as.character(`Target completion date of activity`)) %>% 
    dplyr::mutate(`Mechanism ID`= as.character(`Mechanism ID`)) %>% 
    dplyr::mutate(`Funding Agency`= as.character(`Funding Agency`)) %>% 
    dplyr::mutate(`Mechanism Name`= as.character(`Mechanism Name`)) %>% 
    dplyr::mutate(`Prime Partner Name`= as.character(`Prime Partner Name`)) %>% 
    dplyr::mutate(`Category`= as.character(`Category`)) %>% 
    dplyr::mutate(`Brief description of activity` = as.character(`Brief description of activity`)) %>% 
    dplyr::mutate(`Implementation Timing`= as.character(`Implementation Timing`)) %>% 
    dplyr::mutate(`Priority-rank in order`= as.character(`Priority-rank in order`)) %>% 
    dplyr::mutate(`Are GF or other donors funding any activity in this category?`= as.character(`Are GF or other donors funding any activity in this category?`)) %>% 
    dplyr::mutate(`How has PEPFAR coordinated with GF or other donors to ensure there is not duplication of funding for these activities or sites?`= as.character(`How has PEPFAR coordinated with GF or other donors to ensure there is not duplication of funding for these activities or sites?`)) %>% 
    dplyr::mutate(`Estimate number of patients, HCW and/or sites that would be supported with these activities. (e.g. implement IPC for 100 sites and 400 HCW)`= as.character(`Estimate number of patients, HCW and/or sites that would be supported with these activities. (e.g. implement IPC for 100 sites and 400 HCW)`)) %>% 
    dplyr::mutate(`Target completion date of activity`= as.character(`Target completion date of activity`)) %>% 
    dplyr::mutate(`Approved?`= as.character(`Approved?`)) %>% 
    drop_na("Funding Agency")
  
  return(df)
}

#Create COVID19 Data Frame
df.COVID19 <- purrr::map_dfr(.x = Fast,
                             .f = ~ FAST_COVID(.x))

#Create a short version of the Mech List to join to the COVID19 datafram
df.MechsList_short <-df.MechsList %>% 
  dplyr::select('Mechanism ID','Operating Unit') %>% 
  dplyr::rename("Country" = `Operating Unit`) #the operating unit is actually the country

#Join short Mech List to COVID1- tab by Mechanism ID (We want the Country)
df.COVID19_2<- left_join(df.COVID19, df.MechsList_short, by = "Mechanism ID") %>% 
  relocate('Country') %>% 
  #some of the formatting isn't consistent- change to the YYYY-MM-DD format
  dplyr::mutate(`Target completion date of activity`= recode (`Target completion date of activity`, "Q4 FY22"= "2022-09-30")) %>%
  dplyr::mutate(`Target completion date of activity`= recode (`Target completion date of activity`, "Q4 FY21"= "2021-09-30")) 

#Data frame for Operating Unit and Country
df.OU_Country <- read.csv ("OU_Country.csv" , check.names = FALSE)

#output final dataframe
df.COVID19_Final<- left_join(df.COVID19_2, df.OU_Country, by = "Country") 


#Export the dataset to your file folder of choice manually change the date & version (if needed)
write_csv(df.COVID19_Final, "~/Documents/COVID_DATE_v1.csv")


###FINISHED!!!



