# Title: Global USAID GH Staffing Final Master Dataset
# Author: Vanessa Da Costa
# Last updated: "April 15, 2021"
# Purpose:  Combined/USAID GH Staffing Dataset from October 2020 with Final March 2021 Data
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



##PART 1: MAIN DATASET OCT2020 + MAR2021

#STEP 3: PULL IN HISTORICAL OCTOBER 2020 DATASET 


df1<-read_excel("October 2020 Final Dataset/Staffing_Historical October 2020_Dataset_Mar42021.xlsx") %>%
dplyr::mutate(`datacollection_date`="October 2020") 


#STEP 4: READ IN ALL MARCH 2021 TEMPLATES 

path <- "/Users/Vness/Documents/R/USAID Staffing Updates/Mission Health Staffing Update_ March 2021"

March_Data <- list.files(path, pattern = "*.xlsx", recursive = TRUE)

read_xlsx_files <- function(x){
  df <- read_xlsx(path = paste(path, x, sep = "/"), sheet="Vacant Positions", skip=1)  %>%
    dplyr::mutate(`datacollection_date`="March 2021") %>% 
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
    dplyr::mutate(`hiringcascade`= as.character(`hiringcascade`)) %>% 
    dplyr::mutate(`comments_Mar2021`= as.character(`comments_Mar2021`)) %>% 
    dplyr::mutate(`comments_Oct2020`= as.character(`comments_Oct2020`)) %>% 
    dplyr::mutate(`uniqueid`= as.character(`uniqueid`)) 
  
}

#STEP 5: BIND TOGETHER ALL MARCH 2021 TEMPLATES
df2 <- lapply(March_Data, read_xlsx_files ) %>% 
bind_rows()



#STEP 7: BIND TOGETHER ALL MARCH 2021 to OCT 2020
df.March_all <-rbind(df1,df2)


#Pull in OU by Country Reference Sheet
df.OU<-read_excel("Reference/OU_Country.xlsx")

#Add OUs to the master staffing dataset
df5<- left_join(df.March_all,df.OU, by = "country")
Staffing_Aggregated_Final<- df5 %>%  dplyr::select('country','Operating Unit', 'staffingid',
                                                   'staffingstatus','position','positiontype','employmenttype',
                                                   'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                                                   'staffingschedule', 'hiringcascade', 'datacollection_date', 'uniqueid')   %>%
  replace(., is.na(.), "") %>% 
  dplyr::mutate(`Operating Unit`= recode (`Operating Unit`, " "= "NOT PEPFAR")) %>% 
  #Data cleaning steps  
  dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "TEC is Interviewing Candidates this Week"= "6. Resumes screened/evaluated by HR or TEC"))  %>%
  dplyr::mutate(`positionlevel`= recode (`positionlevel`, "GS-14"= "GS 14")) %>%
  dplyr::mutate(`employmenttype`= recode (`employmenttype`, "USPSC"= "US/TCN PSC")) %>%
  dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "Vacant"= "Vacant (new position)")) %>%
  dplyr::mutate(`citizenship`= recode (`citizenship`, "US"= "US-based American Citizen or Permanent Resident")) %>%
  dplyr::mutate(`positiontype`= recode (`positiontype`, "Technical Advisor"= "Technical Leadership/Management")) 



##PART 2: STAFFLING LEVELS by OU x Healthfunding

#Aggregate staffing levels tab
read_xlsx_files_2 <- function(x){
  df <- read_xlsx(path = paste(path, x, sep = "/"), sheet="Staffing Levels", skip=2)  %>%
    dplyr::mutate(`country`= as.character(`country`)) %>% 
    dplyr::mutate(`healthfunding`= as.character(`healthfunding`)) %>% 
    dplyr::mutate(`Verification`= as.character(`Verification`)) %>% 
    dplyr::mutate(`Number of staff reported in May 2020`= as.character(`Number of staff reported in May 2020`)) %>% 
    dplyr::mutate(`New Number of Staff`= as.character(`New Number of Staff`)) %>% 
    dplyr::rename("Total Staff May2020" = `Number of staff reported in May 2020`) %>% 
     dplyr::filter(healthfunding != "")     #to remove any data that was added outside of the table
    
}


#Create final staffing levels dataset
Staffing_Levels_Total <- lapply(March_Data, read_xlsx_files_2 ) %>% 
  bind_rows() %>% 
  dplyr::mutate(`Total Staff Mar2021`= ifelse(is.na(Verification) | Verification == "Verified- No Changes",  `Total Staff May2020`, `New Number of Staff`)) %>% 
  replace(., is.na(.), "") %>%  
  dplyr::mutate(`Total Staff Mar2021`= recode (`Total Staff Mar2021`, "3 (4 once new HIV advisor onboards later this year)"= "3")) %>% 
  dplyr::mutate(`Total Staff May2020`= as.numeric(`Total Staff May2020`)) %>% 
  dplyr::mutate(`Total Staff Mar2021`= as.numeric(`Total Staff Mar2021`)) %>% 
  dplyr::select('country', 'healthfunding', 'Total Staff May2020', 'Total Staff Mar2021', 'Verification') 
  
 
#Pull in OU by Country Reference Sheet
df.OU<-read_excel("Reference/OU_Country.xlsx")

#Add OUs to the master staffing levels final dataset
Staffing_Levels_Final<- left_join(Staffing_Levels_Total, df.OU, by = "country") %>% 
dplyr::mutate(across(where(is.numeric), round, 0))

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

count(df.March_all, uniqueid=="")
    View(Staffing_Aggregated_Final)
    View(Staffing_Levels_Final)
    
    
#Export Data  
    
    write_csv(  Staffing_Levels_QC, "Staffing_QC_Dataset_Apr9.csv")
  write_csv(  Staffing_Aggregated_Final,"Staffing_Master_Dataset_Apr12.csv")
  write_csv(  Staffing_Levels_Final,"TotalStaffCounts_Aggregated_Dataset_Apr14.csv")

