# PURPOSE: Munge and Analysis of
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-01
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(tidyverse)
    library(gophr)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(here)
    library(janitor)
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
    source("GitHub/EA-Utilities/99_utilities.R")

# LOAD DATA ============================================================================  

    df_scm<-si_path()%>%
      return_latest("COPMatrix")%>%
      read.csv()%>%
      clean_names()%>%
  
    #clean_scm<-function(df){
     #df<-df%>%
      # clean_names()%>%
       dplyr::rename(`operatingunit`=operating_unit,
                     `mech_code`=mechanism_identifier,
                     `mech_name`=mechanism_name,
                     `program`=major_program,
                     `fundingagency`=funding_agency,
                     )%>%
      
      
      return(df)
    } 
  

# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

