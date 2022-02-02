# Title: Global USAID GH Staffing Final Master Dataset
# Author: Vanessa Da Costa
# Last updated: "Oct 22, 2021" ##this will need to be updated for the February 2022 Data##
# Purpose:Combine historical/USAID GH Staffing Dataset with new data
#This code can be updated with each data pull
--------------------------------
#STEP 1: Install packages and run libraries
install.packages("tidyr")
install.packages("tidyverse")
install.packages("dplyr")
Install.packages("stringr")
install.packages("readxl")
install.packages("here")
install.packages("purr")
install.packages("devtools")
install.packages("googlesheets4")
install.packages("remote")
install.packages("data.table")
install.packages("splitstackshape")
install.packages("googledrive")
install.packages("devtools")

#STEP 2: Run libraries
library(stringr)
library(dplyr)
library(tidyverse)
library(tidyr)
library(readr)
library(readxl)
library(here)
library(purrr)
library(here)
library(data.table)
library(googlesheets4)
library(splitstackshape)
library(devtools)
require(tidyverse)
require(readxl)

install.packages("data.table")        # install it
library(data.table)                   # load it
example(data.table)                   # run the examples section of ?data.table
?data.table                           # read
?fread                                # read
update.packages()                     # keep up to date
here()
here::here("USAID Staffing Updates")


#STEP 3: PULL IN latest final master dataset 

  ##UPDATE Master File with latest data collection date.
    #rEMINDER: DELETE 'MASTER' file  & completion tracker from file folder and ensure only country files are in it##
  df1<-read_excel("Final Aggregated Datasets/June 2021 Final Dataset/Staffing_Master_Dataset_July1_AddedNewUniqueIDs.xlsx") %>% 
    dplyr::mutate(`datacollection_date`= as.character(`datacollection_date`)) %>% 
    dplyr::mutate(`hiringcascade`= as.character(`hiringcascade`)) %>% 
    dplyr::mutate(`comments`=" ")
    
  df1<- df1 %>%  dplyr::select('country','staffingid','staffingstatus','position','positiontype','employmenttype',
                                                       'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                                                       'staffingschedule', 'hiringcascade', 'datacollection_date', 'uniqueid', 'comments')
#STEP 4: READ IN ALL NEW TEMPLATES 
  ##UPDATE pathway
  path <- "Mission Health Staffing Update - September 2021"
  Sep_Data<- list.files(path, pattern = "*.xlsx", recursive = TRUE)
  
read_xlsx_files <- function(x){
  df <- read_xlsx(path = paste(path, x, sep = "/"), sheet="Vacant Positions", skip=1)  %>%
    dplyr::mutate(`datacollection_date`= "2021-09-30") %>% ##UPDATE date
    dplyr::mutate(`Operating Unit`= as.character(`Operating Unit`)) %>% 
    dplyr::mutate(`staffingid`= as.character(`staffingid`)) %>% 
    dplyr::mutate(`percentpepfar`= as.character(`percentpepfar`)) %>% 
    dplyr::mutate(`country`= as.character(`country`)) %>% 
    dplyr::mutate(`staffingstatus`= as.character(`staffingstatus`)) %>%  
    dplyr::mutate(`position`= as.character(`position`)) %>% 
    dplyr::mutate(`positiontype`= as.character(`positiontype`)) %>% 
    dplyr::mutate(`positionlevel`= as.character(`positionlevel`)) %>% 
    dplyr::mutate(`employmenttype`= as.character(`employmenttype`)) %>% 
    dplyr::mutate(`citizenship`= as.character(`citizenship`)) %>% 
    dplyr::mutate(`healthfunding`= as.character(`healthfunding`)) %>% 
    dplyr::mutate(`staffingschedule`= as.character(`staffingschedule`)) %>% 
    dplyr::mutate(`datacollection_date`= as.character(`datacollection_date`)) %>% 
    dplyr::mutate(`staffingschedule`= as.character(`staffingschedule`)) %>% 
    dplyr::mutate(`hiringcascade_Jun2021`= as.character(`hiringcascade_Jun2021`)) %>% 
    dplyr::mutate(`comments_Jun2021`= as.character(`comments_Jun2021`)) %>% 
    mutate(hiringcascade=ifelse(is.na(hiringcascade),  `hiringcascade_Jun2021`, `hiringcascade`)) %>% ##For any blank hiring cascades, carry over from June
    dplyr::mutate(`hiringcascade`= as.character(`hiringcascade`)) %>% 
    dplyr::mutate(`uniqueid`= as.character(`uniqueid`)) %>% 
    dplyr::mutate(`comments`= as.character(`comments`)) 
}


#STEP 5: BIND TOGETHER ALL NEW TEMPLATES
df2 <- lapply(Sep_Data, read_xlsx_files ) %>% ##UPDATE Date
bind_rows()

df3<- df2 %>% dplyr::select('country', 'staffingid',
       'staffingstatus','position','positiontype','employmenttype',
       'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
       'staffingschedule', 'hiringcascade', 'datacollection_date', 'uniqueid', 'comments')
  
#STEP 6: BIND TOGETHER OLD AND NEW
df.Sep_all <-rbind(df1,df3) ##UPDATE Date


#STEP 7: Pull in OU by Country Reference Sheet
df.OU<-read_excel("Reference/OU_Country.xlsx")

#STEP 8: Add OUs to the master staffing dataset
df4<- left_join(df.Sep_all,df.OU, by = "country")
Staffing_Aggregated_Final<- df4 %>%  dplyr::select('country','Operating Unit', 'staffingid',
                                                   'staffingstatus','position','positiontype','employmenttype',
                                                   'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                                                   'staffingschedule', 'hiringcascade', 'datacollection_date', 'uniqueid', 'comments')   %>%
  replace(., is.na(.), "") %>% 
  dplyr::mutate(`Operating Unit`= recode (`Operating Unit`, " "= "NOT PEPFAR")) %>% 

#STEP 9: Data cleaning steps  
    dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "filled"= "Filled")) %>% 
    dplyr::mutate(`hiringcascade`= recode (`hiringcascade`, "12. Position Filled/Candidate to start work first week of January"= "12. Position Filled/Candidate started work")) %>% 
    dplyr::mutate(`hiringcascade`= recode (`hiringcascade`, "6. Resumes being screened/evaluated by TEC"= "6. Resumes screened/evaluated by HR or TEC")) %>% 
    dplyr::filter(country != "") %>% 
  dplyr::filter(staffingstatus != "") %>% 
  #Make sure if hiringcascade is 12. Position Filled... staffing status is Filled
  mutate(staffingstatus = case_when( hiringcascade == '12. Position Filled/Candidate started work' ~ 'Filled', TRUE ~ staffingstatus))


##PART 2: STAFFLING LEVELS by OU x Healthfunding

Staffing_Levels_Mar21<-read_excel("Final Aggregated Datasets/TotalStaffCounts_Aggregated_Dataset_May6.xlsx")

#Aggregate staffing levels tab
read_xlsx_files_2 <- function(x){
  df <- read_xlsx(path = paste(path, x, sep = "/"), sheet="Staffing Levels", skip=2)  %>%
    dplyr::mutate(`country`= as.character(`country`)) %>% 
    dplyr::mutate(`healthfunding`= as.character(`healthfunding`)) %>% 
    dplyr::mutate(`Verification`= as.character(`Verification`)) %>% 
    dplyr::mutate(`Number of staff reported in March 2021`= as.character(`Number of staff reported in March 2021`)) %>% 
    dplyr::mutate(`New Number of Staff`= as.character(`New Number of Staff`)) %>% 
    dplyr::rename("Total Staff Sep2021" = `Number of staff reported in March 2021`) %>% 
    dplyr::filter(healthfunding != "")     #to remove any data that was added outside of the table
  
}


Staffing_Levels_Sep21 <- lapply(Sep_Data, read_xlsx_files_2 ) %>% 
  bind_rows() %>% 
  dplyr::mutate(`Total Staff Sep 2021`= ifelse(is.na(Verification) | Verification == "Verified- No Changes",  `Total Staff Sep2021`, `New Number of Staff`)) %>% 
  replace(., is.na(.), "") %>%  
  dplyr::mutate(`Total Staff Sep2021`= as.numeric(`Total Staff Sep 2021`)) %>% 
  dplyr::select('country', 'healthfunding', 'Total Staff Sep2021') 


#Pull in OU by Country Reference Sheet
df.OU<-read_excel("Reference/OU_Country.xlsx")

#Add OUs to the master staffing levels final dataset
Staffing_Levels_Sep21_2<- left_join(Staffing_Levels_Sep21, df.OU, by = "country") %>% 
  dplyr::mutate(across(where(is.numeric), round, 0))


Staffing_Levels_Final<- left_join(Staffing_Levels_Sep21_2, Staffing_Levels_Mar21, by = "country") %>% 
  dplyr::mutate(across(where(is.numeric), round, 0)) %>% 
  dplyr::select('country','Operating Unit', 'healthfunding', 'Total Staff May2021', 'Total Staff Mar2021', 'Total Staff Sep2021') 

#Export Data  

write_csv(Staffing_Aggregated_Sep21,"Staffing_Master_Dataset_Oct22_MissingNewUniqueIDs.csv")
write_csv(Staffing_Levels_Sep21,"TotalStaffCounts_Aggregated_Dataset_Oct22.csv")


#Review Data
unique(Staffing_Levels_Final$`healthfunding`)
unique(Staffing_Levels_Final$`country`)
unique(Staffing_Aggregated_Final$`positiontype`)
unique(Staffing_Aggregated_Final$`hiringcascade`)
unique(Staffing_Aggregated_Final$`staffingstatus`)
unique(Staffing_Aggregated_Final$`staffingschedule`)
unique(Staffing_Aggregated_Final$`employmenttype`)
unique(Staffing_Aggregated_Final$`positionlevel`)
unique(Staffing_Aggregated_Final$`citizenship`)


View(Staffing_Aggregated_Final)
View(Staffing_Levels_Sep21)


