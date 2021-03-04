# Title: Global USAID GH Staffing Dataset Wrangling
# Author: Vanessa Da Costa
# Last updated: "March 4th, 2021"
# Purpose: Wrangle Combined/USAID GH Staffing Dataset from October 2020 to align with March 2021 Template
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
here::here("USAID Staffing Updates/October 2020 Dataset")


#Wrangle all the data
#create working dataset
  df1<-read_excel("Combined Health Mission Staffing Update Oct 2020.xlsx",sheet= 'Combined Data') 
  df2<-read_excel("ISO Codes.xlsx",sheet= 'STAFFING') 
  df<- merge(df1,df2, by = "COUNTRY") 
  
# Drop columns you don't need and rename  
  df_final<- df %>% dplyr::select( -c('Post Job Solicitation\r\nStart Date': 'Post Job Solicitation End Date')) %>%
  dplyr::rename("country" = `COUNTRY`,
                  "staffingid" = `STAFFING ID`,
                  "staffingstatus" = `STAFFING STATUS`,
                  "position" = `POSITION TITLE`,
                  "positiontype" = `POSITION TYPE`,
                  "employmenttype" = `EMPLOYMENT TYPE`,
                  "citizenship" = `CITIZENSHIP TYPE`,
                 "healthfunding" = `HEALTH FUNDING TYPE`,
                 "comments" = `COMMENTS (additional position details, barriers to filling a vacancy, etc.)`,
                "positionlevel" = `Position Level` ) %>%
               
#Add new columns
  dplyr::mutate(`0. No action taken yet`="0") %>%
  dplyr::mutate(`percentpepfar`=" ")   %>%
  dplyr::mutate(`staffingschedule`=" ") %>% 
  dplyr::mutate(`comments_Mar2021`=" ")   %>%
    
    
#create unique ID ###_3DigitISO
  dplyr::group_by(country) %>% mutate(id = row_number()) %>%    
  mutate(id = sprintf("%03d", id)) %>%
  unite(uniqueid, c("id", "CODE")) %>%
    
    select( 'country', 'staffingid',	
            'staffingstatus','position','positiontype','employmenttype', 
            'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
            'staffingschedule', '0. No action taken yet', 'Embassy approval for position, if required',
            'PD', 'Review Documents (HR Assistant or Specialist in Mission)', 
            'Validation from HCTM or South Africa EXO', 'Post Job Solicitation',
            'HR Screening of Offers/Applications', 'Hiring Office evaluating and selecting candidates for interviews', 
            'HR contacting candidates and arranging interviews', 'Interview', 'Reference Check',
            'Hiring Decision', 'Tentative Job Offer', 'Security Clearance', 'Medical Clearance', 'Official Offer', 'comments_Mar2021', 'comments', 'uniqueid') %>%  
    
#Recode values to match naming in March 2021 Staffing Update Template
    #country
    dplyr::mutate(`country`= recode (`country`, "Bostwana"= "Botswana")) %>%
    
    #staffingid
    dplyr::mutate(`staffingid`= recode (`staffingid`, "N/A"= " ")) %>%
    dplyr::mutate(`staffingid`= recode (`staffingid`, "PMI"=  " ")) %>%
    dplyr::mutate(`staffingid`= recode (`staffingid`, "not yet created"=  " ")) %>%
    dplyr::mutate(`staffingid`= recode (`staffingid`, "PMI"=  " ")) %>%
    dplyr::mutate(`staffingid`= recode (`staffingid`, "pending"=  " ")) %>%
    
    #staffingstatus
    dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "Vacant"= "Vacant (new position)")) %>%
    dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "Vacant "= "Vacant (new position)")) %>%
    dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "Vacant (Previously Filled)"= "Vacant (previously filled)")) %>%
    dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "Vacant (New)"= "Vacant (new position)")) %>%
    dplyr::mutate(`staffingstatus`= recode (`staffingstatus`, "Vacant (new)"= "Vacant (new position)")) %>%

    #positionttype
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Administrative Support "= "Administrative Support")) %>%
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Contracting/Financial/HR/Legal"= "Contracting/Financial/Legal")) %>%
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Management"= "Other Management/ Leadership")) %>%
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Other Management/Leadership"= "Other Management/ Leadership")) %>%
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Public Affairs/Public Diplomacy"= "US Mission Leadership and Public Diplomacy")) %>%
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Technical"= "Technical Leadership/Management")) %>%
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Technical Advisor/Program Management"= "Technical Leadership/Management")) %>%
    dplyr::mutate(`positiontype`= recode (`positiontype`, "Technical Leadership/Management"= "Technical Leadership/Management")) %>%
    
    #employmenttype
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "CCN"= "FSN (CCN PSC)")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "USPSC"= "US/TCN PSC")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "Personal Services Agreement"= "FSN (CCN PSC)")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "Personal Services Contract"= "FSN (CCN PSC)")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "Personal Services Contractor"= "FSN (CCN PSC)")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "Direct Hire"= "Other")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "FSO"= "Other")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "Non Personal Services Contract"= "Other")) %>%
    dplyr::mutate(`employmenttype`= recode (`employmenttype`, "STAR"= "Other")) %>%
      
    #citizenship  
    dplyr::mutate(`citizenship`= recode (`citizenship`, "Host Country National"= "Host Country National (or legal permanent resident)")) %>%
    dplyr::mutate(`citizenship`= recode (`citizenship`, "Host Country National "= "Host Country National (or legal permanent resident)")) %>%
    dplyr::mutate(`citizenship`= recode (`citizenship`, "Locally Resident American Citizen"= "Locally Resident American Citizen or US Permanent Resident")) %>%
    dplyr::mutate(`citizenship`= recode (`citizenship`, "Third Country National"= "Internationally Recruited Third Country National")) %>%
    dplyr::mutate(`citizenship`= recode (`citizenship`, "US-based American Citizen"= "US-based American Citizen or Permanent Resident")) %>%
    dplyr::mutate(`citizenship`= recode (`citizenship`, "US-Based American Citizen"= "US-based American Citizen or Permanent Resident")) %>%
    
    #healthfunding
    dplyr::mutate(`healthfunding`= recode (`healthfunding`, "MCH"= "MCH Nutrition")) %>% #update
    dplyr::mutate(`healthfunding`= recode (`healthfunding`, "OE"= "N/A (including OE)")) %>%
    dplyr::mutate(`healthfunding`= recode (`healthfunding`, "PEFPAR"= "PEPFAR")) %>%
    
    #positionlevel
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "FSN9"= "FSN 9")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "GS-14"= "GS 14")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "GS-15"= "GS 15")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "PSC"= "Other")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "USPSC GS-14"= "GS 14")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "USPSC GS-14"= "Other")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "?"= "Other")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "Done"= "Other")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "FSN 14"= " ")) %>%
    dplyr::mutate(`positionlevel`= recode (`positionlevel`, "FSN 15"= " ")) %>%
    
    #hiringcascade to hierarchy
    dplyr::mutate(`Embassy approval for position, if required`= recode(`Embassy approval for position, if required`, "Drafted"="1")) %>% 
    dplyr::mutate(`Embassy approval for position, if required` = recode (`Embassy approval for position, if required`, "N/A"="1")) %>% 
    dplyr::mutate(`Embassy approval for position, if required` = recode (`Embassy approval for position, if required`, "Pending"="1")) %>% 
    dplyr::mutate(`Embassy approval for position, if required` = recode (`Embassy approval for position, if required`, "Pending Approval"="1")) %>% 
    dplyr::mutate(`Embassy approval for position, if required`= recode(`Embassy approval for position, if required`, "Approved"="2")) %>% 
    dplyr::mutate(`Embassy approval for position, if required` = recode (`Embassy approval for position, if required`, "Done"="2")) %>% 
    dplyr::mutate(`Review Documents (HR Assistant or Specialist in Mission)`= recode(`Review Documents (HR Assistant or Specialist in Mission)`, "Reviewed"="3")) %>% 
    dplyr::mutate(`Review Documents (HR Assistant or Specialist in Mission)` = recode (`Review Documents (HR Assistant or Specialist in Mission)`, "Done"="3")) %>% 
    dplyr::mutate(`PD`= recode(`PD`, "Completed"="3")) %>% 
    dplyr::mutate(`PD` = recode (`PD`, "Done"="3")) %>% 
    dplyr::mutate(`Validation from HCTM or South Africa EXO`= recode(`Validation from HCTM or South Africa EXO`, "Completed"="4")) %>% 
    dplyr::mutate(`Validation from HCTM or South Africa EXO` = recode (`Validation from HCTM or South Africa EXO`, "Done"="4")) %>% 
    dplyr::mutate(`Post Job Solicitation` = recode (`Post Job Solicitation`, "Done"="5")) %>% 
    dplyr::mutate(`HR Screening of Offers/Applications` = recode (`HR Screening of Offers/Applications`, "Done"="6")) %>%
    dplyr::mutate(`Hiring Office evaluating and selecting candidates for interviews` = recode (`Hiring Office evaluating and selecting candidates for interviews`, "Done"="6")) %>%    
    dplyr::mutate(`HR contacting candidates and arranging interviews` = recode (`HR contacting candidates and arranging interviews`, "Done"="6")) %>%  
    dplyr::mutate(`Interview` = recode (`Interview`, "Done"="7")) %>%  
    dplyr::mutate(`Reference Check` = recode(`Reference Check`,"Done"="8"))  %>% 
    dplyr::mutate(`Hiring Decision` = recode(`Hiring Decision`, "Done"="9")) %>% 
    dplyr::mutate(`Tentative Job Offer`= recode(`Tentative Job Offer`, "Done"="9"))%>% 
    dplyr::mutate(`Tentative Job Offer`= recode(`Tentative Job Offer`, "Completed"="9")) %>% 
    dplyr::mutate(`Security Clearance` = recode (`Security Clearance`, "Done"="10")) %>% 
    dplyr::mutate(`Security Clearance` = recode (`Security Clearance`, "Completed"="10")) %>% 
    dplyr::mutate(`Medical Clearance` = recode (`Medical Clearance`, "Done"="10")) %>% 
    dplyr::mutate(`Medical Clearance` = recode (`Medical Clearance`, "Completed"="10")) %>% 
    dplyr::mutate(`Official Offer` = recode (`Official Offer`, "Done"="11"))   %>% 
    dplyr::mutate(`Official Offer` = recode (`Official Offer`, "Completed"="11"))   %>% 

    #pivot hiringcascasde long to 'hiringcascade_old' with value as 'hiringcascade' and replace NAs with 0s
    gather(`hiringcascade_old`,`hiringcascade_1`,`0. No action taken yet`:`Official Offer`)  %>% 
    dplyr::mutate(`hiringcascade_1`=as.numeric(`hiringcascade_1`))  %>% 
    drop_na(hiringcascade_1)  %>% 
    dplyr::select( -c('hiringcascade_old'))  %>% 
  
    #keep maximum value by uniqueid
    group_by(uniqueid) %>% top_n(1, hiringcascade_1)  %>% 
    
    #make hiringcascade character
    dplyr::mutate(`hiringcascade_1`=as.character(`hiringcascade_1`)) %>% 
    distinct()  %>% 
    
    #renaming new hiringcascade column (replacing old one) based on 0 no action taken yet and comments
    dplyr::mutate(
      hiringcascade = case_when(
        hiringcascade_1 == 0  & comments == "NzoyaD to develop PD and JDHS. Pending decision regarding AOR/COR/AM responsibility designation."  ~ "2",
        uniqueid == "018_ZMB"~ "2",
        uniqueid == "036_KEN"~ "2",
        uniqueid == "037_KEN"~ "2",
        hiringcascade_1 == 0  & comments == "NDraft PD and JDHS with ElizabethM for review and feedback. Pending decision regarding AOR/COR/AM responsibility designati"  ~ "3",
        hiringcascade_1 == 0  & comments == "PRIORITY GROUP 2. DRAFT PD AND JDHS PENDING DEVELOPMENT"  ~ "3",
        hiringcascade_1 == 0  & comments == "PD and JDHS with ElizabethM for review and feedback"  ~ "3",
        hiringcascade_1 == 0  & comments == "Draft PD and JDHS with WashingtonO and WairimuG for final approval"  ~ "3",
        hiringcascade_1== 0  & comments == "Draft PD with WashingtonO for review and feedback"  ~ "3",
        hiringcascade_1 == 0  & comments == "Pending decision on the use of HPN-harmonized PD"  ~ "3",
        hiringcascade_1 == 0  & comments == "The JD has been updated and will be submitted for classification the week of October 12. (10/16/20)"  ~ "3",
        hiringcascade_1 == 0  & comments == "Minh Huong's replacement. Position re-caged as a 12 (from an 11) and will be advertising it shortly. (10/16/20)"  ~ "4",
        hiringcascade_1 == 0  & comments == "Under recruitment"  ~ "5",
        uniqueid == "002_SLE"~ "5",
        uniqueid == "007_THA"~ "5",
        hiringcascade_1 == 0  & comments == "Under recruitment. RDMA has a consultant supporting in the interim."  ~ "5",
        hiringcascade_1 == 0  & comments == "Recruitment difficulty: trouble finding qualified candidate in 2 rounds; selection committee in progress"  ~ "6",
        hiringcascade_1 == 0  & comments == "Inability to get the position classified at a competitve grade for the work required. Lack of qualitifed candidates"  ~ "6",
        hiringcascade_1 == 0  & comments == "USAID Development Program Specialist. Recruitment in progress as a candidate has been identified."  ~ "6",
        hiringcascade_1 == 0  & comments == "Position based in Lubumbashi - waiting on security clearance of lab construction"  ~ "9",
        hiringcascade_1 == 0  & comments == "The Mission has selected a candidate who is undergoing a security clearance process (which is delayed due to COVID-19)"  ~ "9",
        hiringcascade_1 == 0  & comments == "Need to follow-up with the Health Officer, but expected to be onboard by the end of 2020."  ~ "11",
        TRUE ~ hiringcascade_1 )) %>% 
  
    #hiringcascade to match template
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "0"= "0. No action taken yet"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "1"= "1. Funding authorized/identified"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "2"= "2. Embassy/COM approved; NSDD-38 process, if applicable"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "3"= "3. Developed PD/JDHS or SOW"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "4"= "4. HCTM/HRSU approval of documents"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "5"= "5. Job Solicitation Posted"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "6"= "6. Resumes screened/evaluated by HR or TEC"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "7"= "7. Applicants interviewed"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "8"= "8. Reference checks completed."))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "9"= "9. Tentative job offer made"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "10"= "10. Medical/ Security Clearance Approved"))  %>%
    dplyr::mutate(`hiringcascade` = recode(`hiringcascade`, "11"= "11. Official Offer Letter Made"))  %>%  
    
    dplyr::rename("comments_Oct2020" = `comments`) %>%
  
    replace(., is.na(.), "") %>%  
  
    #final dataset in order of interest
    dplyr::select( 'country', 'staffingid',	
    'staffingstatus','position','positiontype','employmenttype', 
  'citizenship', 'healthfunding', 'percentpepfar', 'positionlevel',
  'staffingschedule', 'hiringcascade', 'comments_Mar2021','comments_Oct2020', 'uniqueid') 
 
    #export data
    write_csv(df_final,"Staffing_Historical_Dataset_Wrangled.csv")
    
    
    
##DATA QUALITY REVIEW STEPS##
#viewing values in the hiring cascade
table(df$`HR Screening of Offers/Applications`)  
table(df$`Hiring Office evaluating and selecting candidates for interviews`)  
table(df$`HR contacting candidates and arranging interviews`)
table(df$`Interview`) #recoded to 7. Applicants interviewed
table(df$`Reference Check`) #recoded to 8. Reference checks completed
table(df$`Hiring Decision`)
table(df$`Tentative Job Offer`) #recoded to 9.Tentative job offer made
table(df$`Security Clearance`) #recoded to 10. Medical/Security Clearance Approved
table(df$`Medical Clearance`) #recoded to 10. Medical/Security Clearance Approved
table(df$`Official Offer`) #recoded to 11. Official Offer made

#viewing values in df
table(df_final$`staffingid`)
table(df_final$`staffingstatus`)
table(df_final$`position`)
table(df_final$`positiontype`)
table(df_final$`employmenttype`)
table(df_final$`citizenship`)
table(df_final$`healthfunding`)
table(df_final$`positionlevel`)
table(df_final$`hiringcascade`)
view(df_final)


glimpse(df)

