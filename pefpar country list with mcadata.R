# PURPOSE:  pepfar country list
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-12-17
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
library(googlesheets4)
library(googledrive)
library(janitor)
library(tidyverse)
library(glamr)
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  
  

# LOAD DATA ============================================================================  

    sheet_id_country <- googlesheets4::as_sheets_id('1tPGvGeB6eublMSeR5H0OUVcuMs2iK1XDm1KkzXJZlWI')
    df <- googlesheets4::read_sheet(sheet_id_country, "crosswalk_proposed_list")%>%
      select(country_id,iso_alpha3)
    
    df_pepfar<-pepfar_country_list%>%
      rename(iso_alpha3=countryname_iso)
    
    df_key<-left_join(df_pepfar,df, by="iso_alpha3")
    write.csv(df_key,"pepfar country list for data services.csv")


# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

