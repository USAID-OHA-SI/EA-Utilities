# Title: Global USAID GH Staffing Dataset Wrangling
# Author: Vanessa Da Costa
# Last updated: "May 24th, 2021"
# Purpose: Wrangle Combined/USAID GH Staffing Dataset from March 2021 to align with June 2021 Template
--------------------------------
  #Install packages and run libraries
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

install.packages("data.table")        # install it
library(data.table)                   # load it
example(data.table)                   # run the examples section of ?data.table
?data.table                           # read
?fread                                # read
update.packages()                     # keep up to date
here()
here::here("USAID Staffing Updates")


#Wrangle all the data
#create working dataset
  df<-read_excel("March 2021 Final Dataset/Staffing_Master_Dataset_May 6_AddedUniqueIDs.xlsx",sheet= 'May 6 staffing')
  df.OU<-read_excel("Reference/OU_Country.xlsx")
  
  df <-df %>% dplyr::select( -c('incomparison','inallanalysis', 'Operating Unit'))
  df_2<- left_join(df,df.OU, by = "country")
  
  df_final <-df_2 %>%
    dplyr::mutate(`datacollection_date`= as.character(`datacollection_date`)) %>% 

#filter out filled positions
    dplyr::filter(staffingstatus != "Filled") %>%
    dplyr::filter(datacollection_date != "2020-10-01") %>%
    dplyr::filter(hiringcascade != "12. Position Filled/Candidate started work") %>% 
    
#default columns
    dplyr::mutate(`datacollection_date`="June 2021") %>%
    dplyr::mutate(`comments_Jun2021`=" ") %>%
    replace(., is.na(.), "") %>%  

#final dataset in order of interest
  dplyr::select( 'country', 'Operating Unit', 'staffingid',	
                   'staffingstatus','position','positiontype','employmenttype', 
                   'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                   'staffingschedule', 'hiringcascade', 'datacollection_date', 'comments_Jun2021', 'uniqueid') 
  
  
write_csv(df_final,"Staffing_June2021_Template_May24_v2.csv")
  