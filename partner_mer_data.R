# PURPOSE: Pull Partner MER  Data
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-10-01
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries

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
   
    si_paths 
    
  # Functions  
  percent_clean<-function(x, y) {
    ifelse(y > 0.000, (x / y), NA_real_)
  }
  
 

# LOAD DATA ============================================================================  

    
    df_msd<-si_path()%>%
      glamr::return_latest("OU_IM")%>%
      gophr::read_msd()



  print_mer_cop20 <- function(mechs){
    df_mech <- df %>% 
      filter(`country-mech` == mechs)
    
    
    
    meta <- df_mech %>% 
      distinct(countryname, mech_code) %>%
      mutate(countryname = str_remove_all(countryname, " |'"),
             name = glue("ER21_Financial_Programmatic/COP20_{countryname}_{mech_code}_MER.csv"))
    
    
    print(glue("Printing...{meta$mech_code}"))
    write_csv(df_mech, file.path(meta$name), na = "")
  }
  
  
  # MUNGE ============================================================================
  

  df<-df_msd%>%
    dplyr::filter(standardizeddisaggregate=="Total Numerator")%>%
    dplyr::filter(fiscal_year=="2021")%>%
    dplyr::filter(fundingagency=="USAID")%>%
    mutate(country = ifelse(operatingunit == countryname, operatingunit, glue("{operatingunit}-{countryname}")))%>%
    mutate("country-mech"=glue("{country}-{mech_code}"))%>%
    
    select(fiscal_year,operatingunit,countryname,fundingagency,primepartner,mech_code,mech_name,`country-mech`,indicator,targets,cumulative)%>%
    group_by(countryname,`country-mech`, mech_code, mech_name, primepartner, fiscal_year,indicator)%>%
    summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(target_achievement=percent_clean(cumulative,targets))%>%
    ungroup()%>% 
    arrange(`country-mech`)%>%
    filter(mech_code!="81138")
  
  
  # CREATE Budget  FILES -----------------------------------------------------
  
  #list of mechanism
  mechs <- df %>% 
    distinct(`country-mech`) %>% 
    pull()   
  
  #create output folders folders
  dir_create("ER21_Financial_Programmatic")
  
  #create output budget files
  walk(mechs,  print_mer_cop20)
  
  #test one
  print_mer_cop20("Angola-81010")
  
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
  