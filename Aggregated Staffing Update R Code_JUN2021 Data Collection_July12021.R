# Title: Global USAID GH Staffing Final Master Dataset
# Author: Vanessa Da Costa
# Last updated: "July 01, 2021"
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
  df1<-read_excel("Final Aggregated Datasets/March 2021 Final Dataset/Staffing_Master_Dataset_May 6_AddedUniqueIDs.xlsx") %>% 
    dplyr::mutate(`datacollection_date`= as.character(`datacollection_date`)) %>% 
    dplyr::mutate(`hiringcascade`= as.character(`hiringcascade`)) %>% 
    dplyr::mutate(`comments`=" ")
    
  df1<- df1 %>%  dplyr::select('country','staffingid','staffingstatus','position','positiontype','employmenttype',
                                                       'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                                                       'staffingschedule', 'hiringcascade', 'datacollection_date', 'uniqueid', 'comments')
#STEP 4: READ IN ALL NEW TEMPLATES 
  ##UPDATE pathway
  path <- "/Users/Vness/Documents/R/USAID Staffing Updates/Mission Health Staffing Update_ June 2021"
  June_Data<- list.files(path, pattern = "*.xlsx", recursive = TRUE)
 
  
read_xlsx_files <- function(x){
  df <- read_xlsx(path = paste(path, x, sep = "/"), sheet="Vacant Positions", skip=1)  %>%
    dplyr::mutate(`datacollection_date`= "2021-06-25") %>% ##UPDATE date
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
    dplyr::mutate(`comments_Jun2021`= as.character(`comments_Jun2021`)) %>% 
    dplyr::mutate(`comments_Mar2021`= as.character(`comments_Mar2021`)) %>% 
    dplyr::rename("comments" = `comments_Jun2021`) %>% ##UPDATE date
    dplyr::mutate(`datacollection_date`= as.character(`datacollection_date`)) %>% 
    mutate(hiringcascade_Jun2021 =ifelse(is.na(hiringcascade_Jun2021),  `hiringcascade_March2021`, `hiringcascade_Jun2021`)) %>% ##For any blank hiring cascades, carry over from March
    dplyr::mutate(`hiringcascade_Jun2021`= as.character(`hiringcascade_Jun2021`)) %>% 
    dplyr::mutate(`hiringcascade_March2021`= as.character(`hiringcascade_March2021`)) %>% 
    dplyr::rename("hiringcascade" = `hiringcascade_Jun2021`) %>% 
    dplyr::mutate(`uniqueid`= as.character(`uniqueid`)) 
}

#STEP 5: BIND TOGETHER ALL NEW TEMPLATES
df2 <- lapply(June_Data, read_xlsx_files ) %>% ##UPDATE Date
bind_rows()

df3<- df2 %>% dplyr::select('country', 'staffingid',
       'staffingstatus','position','positiontype','employmenttype',
       'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
       'staffingschedule', 'hiringcascade', 'datacollection_date', 'uniqueid', 'comments')
  
#STEP 6: BIND TOGETHER OLD AND NEW
df.June_all <-rbind(df1,df3) ##UPDATE Date


#STEP 7: Pull in OU by Country Reference Sheet
df.OU<-read_excel("Reference/OU_Country.xlsx")

#STEP 8: Add OUs to the master staffing dataset
df3<- left_join(df.June_all,df.OU, by = "country")
Staffing_Aggregated_Final<- df3 %>%  dplyr::select('country','Operating Unit', 'staffingid',
                                                   'staffingstatus','position','positiontype','employmenttype',
                                                   'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                                                   'staffingschedule', 'hiringcascade', 'datacollection_date', 'uniqueid', 'comments')   %>%
  replace(., is.na(.), "") %>% 
  dplyr::mutate(`Operating Unit`= recode (`Operating Unit`, " "= "NOT PEPFAR")) %>% 

#STEP 9: Data cleaning steps  
    dplyr::filter(comments != "No hiring currently. Will hire new position under ROP21") %>% 
    dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "filled"= "Filled")) 

#STEP 10: Export Data  
    
write_csv(Staffing_Aggregated_Final,"Staffing_Master_Dataset_July1_MissingNewUniqueIDs.csv")

