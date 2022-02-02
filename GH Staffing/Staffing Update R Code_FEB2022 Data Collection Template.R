# Title: Global USAID GH Staffing Dataset Wrangling
# Author: Vanessa Da Costa
# Last updated: "February 1, 2022"
# Purpose: Wrangle Combined/USAID GH Staffing Dataset from Sep 2021 to align with Feb  2022 Template
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

#import working dataset
  df<-read_excel("Final Aggregated Datasets/September 2021 Final Dataset/Staffing_Master_Dataset_Oct25_AddedNewUniqueIDs.xlsx") %>% 
    dplyr::rename("hiringcascade_Sep2021" = `hiringcascade`, "comments_Sep2021" = `comments`) %>% 
    dplyr::select( -c('Operating Unit')) %>% 
    dplyr::mutate(`datacollection_date`= as.character(`datacollection_date`)) %>% 
    #filter out historical data (Keep June 25,2021)
    dplyr::filter(datacollection_date != "2020-10-01") %>%  
    dplyr::filter(datacollection_date != "2021-03-01") %>% 
    dplyr::filter(datacollection_date != "2021-06-25") 
    

#add operating unit back into dataset    
df.OU<-read_excel("Reference/OU_Country.xlsx") 
  df_2<- left_join(df,df.OU, by = "country")

#create vacancy dataset
  df_3 <-df_2 %>%
  #filter out filled positions
    dplyr::filter(staffingstatus != "Filled") %>%
    dplyr::filter(hiringcascade_Sep2021 != "12. Position Filled/Candidate started work") %>% 
    dplyr::filter(hiringcascade_Sep2021 != "Position Removed") %>% 
  #rename to standard naming  
    dplyr::rename("hiringcascade" = `hiringcascade_Sep2021`) %>% 
    
  #default columns
    dplyr::mutate(`datacollection_date`="2022-02-01") %>%
    dplyr::mutate(`comments`=" ") %>%
    replace(., is.na(.), "") %>%  
  #final dataset in order of interest
  dplyr::select( 'country', 'Operating Unit', 'staffingid',	
                   'staffingstatus','position','positiontype','employmenttype', 
                   'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                   'staffingschedule', 'hiringcascade', 'datacollection_date', 'comments', 'uniqueid') 
  
# add old hiring cascade and comments dataset
  df_selectold <- df %>% 
  dplyr::select( 'uniqueid','datacollection_date', 'hiringcascade_Sep2021', 'comments_Sep2021') %>% 
  #so we can append the old comments to the final dataset by unqiue ID and data collectiondate
      mutate(`datacollection_date`= recode (`datacollection_date`, "2021-09-30"= "2022-02-01"))
  
#Final Master Template
  df_final<- left_join(df_3,df_selectold, by = c("uniqueid", "datacollection_date")) %>% 
  dplyr::mutate(`hiringcascade`=" ") %>%
  dplyr::select( 'country', 'Operating Unit', 'staffingid',	
                 'staffingstatus','position','positiontype','employmenttype', 
                 'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
                 'staffingschedule', 'hiringcascade', 'datacollection_date', 'comments','hiringcascade_Sep2021','comments_Sep2021', 'uniqueid') 
  
  write.table(df_final, paste ('Templates/Staffing_Feb2022_Template_', Sys.Date(), ".txt"), sep ="\t", row.names=FALSE)
  
