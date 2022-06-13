# PURPOSE: Pull Partner MER  Data
# AUTHOR: Ben Kasdan | SIEI
# Additional Edits: Jairo Montes
# LICENSE: MIT
# DATE: 2022-06-13
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
install.packages("patchwork")
library(glamr)
library(gophr)
library(tidyverse)
library(tidytext)
library(patchwork)
library(googlesheets4)
library(glue)
library(googledrive)
library(fs)
  
  # Set paths  
   proj_paths
   
    si_path 
    
  # Functions  
  percent_clean<-function(x, y) {
    ifelse(y > 0.000, (x / y), NA_real_)
  }
  
 

# LOAD DATA ============================================================================  

    
    #df_msd<-si_path()%>%
      #glamr::return_latest("OU_IM")%>%
      #gophr::read_msd()

  df_msd2 <- read_csv("C:/Users/jmontespenaloza/Documents/Raw Datasets/FAST_DATAPACK_06_13_22.csv", col_names = TRUE)
  

  print_mer_cop20 <- function(mechs){
    df_mech <- df %>% 
      filter(`Mechanism ID` %in% mechs)
   
    
    meta <- df_mech %>% 
      distinct(Country, `Mechanism ID`) %>%
      mutate(Country = str_remove_all(Country, " |'"),
             name = glue("COP22 Targets Templates/COP22_{Country}_{`Mechanism ID`}_MER.csv"))
    
    note2<-data.frame(Country="The data above presents COP22 target data from the COP22 Datapack as of 6/8/2022. Please consult with your A/COR or AM to validate these figures.")
    
    note3<-data.frame(Country="For questions please reach out to your mission SI POC.")
    note1<-data.frame(Country=" ")
   
     df_mech<-bind_rows(df_mech,note1,note2,note3)
    
    print(glue("Printing...{meta$Country}-{meta$`Mechanism ID`}"))
    write_csv(df_mech, file.path(meta$name), na = "")
  }
  
  
  # MUNGE ============================================================================
  

  df<-df_msd2%>%
    dplyr::filter(`Data Stream` == "MER") %>%  
    #dplyr::filter(standardizeddisaggregate=="Total Numerator")%>%
    dplyr::filter(`Fiscal Year` =="2023")%>%
    dplyr::filter(`Funding Agency`=="USAID")%>%
    mutate(Country = ifelse(`Operating Unit` == Country, `Operating Unit`, glue("{`Operating Unit`}-{Country}")))%>%
    #mutate("country-mech"=glue("{Country}-{mech_code}"))%>%
    
    #select(`Fiscal year`,`Operating Unit`,Country,`Funding Agency`,`Prime Partner Name`,`Mechanism ID`,`Mechanism Name`,`country-mech`,indicator,targets,cumulative)%>%
    select(`Fiscal Year`,`Operating Unit`,Country,`Funding Agency`,`Prime Partner Name`,`Mechanism ID`,`Mechanism Name`,Indicator,Target)%>%
    #group_by(Country,`country-mech`, `Mechanism ID`,`Mechanism Name`, `Prime Partner Name`, `Fiscal Year`,Indicator)%>%
    group_by(Country, `Mechanism ID`,`Mechanism Name`, `Prime Partner Name`, `Fiscal Year`,Indicator)%>%
    
    summarise_at(vars(Target), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    #mutate(target_achievement=percent_clean(cumulative,targets))%>%
    #ungroup()%>% 
    arrange(Country, `Mechanism ID`)
  
  
  # CREATE Budget  FILES -----------------------------------------------------
  
  #list of mechanism
  mechs <- df %>% 
    distinct(`Mechanism ID`) %>% 
    pull()   

  #create output folders folders
  dir_create("COP22 Targets Templates")

  #create output budget files
  walk(mechs,  print_mer_cop20)
  
  #test one
  print_mer_cop20("81010")
  
  # MOVE TO DRIVE -----------------------------------------------------------
  
  #create folder for upload
  drive_mkdir("COP20 Financial and Programmatic Files",
              path = as_id(glbl_id)) #path is to the ER FY21 Folder but can be changed
  
  #identify list of   
  local_files <- list.files("ER21_Financial_Programmatic", full.names = TRUE)
  
  #push to drive
  walk(local_files,
       ~ drive_upload(.x,
                      path = as_id(fldr_id), #path is to the ER21 test file folder
                      name = basename(.x),
                      type = "spreadsheet"))
  
  #remove all local files
  unlink("ER21_Financial_Programmatic", recursive = TRUE)
  