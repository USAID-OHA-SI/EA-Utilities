# PURPOSE: Pull budgets for partners
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-22
# NOTES: use this function to pull partner COP20 Budget, workplan budget, expenditure

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(gt)
    library(googlesheets4)
    library(glue)
    library(googledrive)
library(janitor)
library(fs) #to create folders

# Output folder ============================================================================

load_secrets()

#global material folder
glbl_id <- "1YjTe1H2oQjsPX_Tr7kirRUOnC0KKe4e0" #ER21 Folder

#upload folder
fldr_id <- "1mFOnqSNeRYCpEzN9Kzu0HJBQ88x2maXG" #ER21 special folder for these files. Get this once you set up the folder below
    


    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  
    
  # Functions  
  
    
    print_financial_cop20 <- function(mechs){
      df_mech <- df_totals %>% 
        filter(mech_code == mechs)
          
        df1<-df_mech %>%
      adorn_totals("row",,,,-fiscal_year) %>%
          tail(1)%>%as.vector()%>%
          mutate(fiscal_year=as.integer(fiscal_year))
          
      
      meta <- df_mech %>% 
        distinct(country, mech_code, mech_name) %>%
        mutate(country = str_remove_all(country, " |'"),
               name = glue("ER21_Financial_Programmatic/COP20__Financial_{country}_{mech_code}_{mech_name}.csv"))
      
      note2<-data.frame(country="The data above presents COP budgets, workplan budgets, and expenditures. Only workplan budgets and expenditure will have data at the cost category level.")
      
      note3<-data.frame(country="For questions please reach out to the EA Branch at oha.ea@usaid.gov")
      note1<-data.frame(country=" ")
      df_mech<-bind_rows(df_mech,df1,note1,note2,note3)
      
      print(glue("Printing...{meta$country}-{meta$mech_code}-{meta$mech_name}"))
      write_csv(df_mech, file.path(meta$name), na = "")
    }

# LOAD DATA ============================================================================  

    df_fsd<-si_path()%>%
      return_latest("Fin")%>%
      gophr::read_msd()

# MUNGE ============================================================================
  #get financial total
    df_totals <- df_fsd %>% 
             filter(fundingagency == "USAID") %>%
      remove_mo()%>%
      filter(planning_cycle =="COP20")%>%
      mutate(country = ifelse(operatingunit == countryname, operatingunit, glue("{operatingunit}-{countryname}")))%>%
    
      mutate("Program Area: Sub Program Area-Service Level"=glue("{program}: {sub_program}-{interaction_type}"))%>%
    
      mutate("Beneficiary-Sub Beneficiary"=glue("{beneficiary}-{sub_beneficiary}"))%>%  
      mutate("Cost Category-Sub Cost Category"=glue("{cost_category}-{sub_cost_category}"))%>%
      group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`,`Cost Category-Sub Cost Category`) %>% 
      #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
      summarise_at(vars(cop_budget_total, workplan_budget_amt, expenditure_amt), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      arrange(country, mech_code)
   
# CREATE Budget  FILES -----------------------------------------------------
    
    #list of mechanism
    mechs <- df_totals %>% 
      distinct(mech_code) %>% 
      pull()   
    
    #create output folders folders
    dir_create("ER21_Financial_Programmatic")
  
    #create output budget files
    walk(mechs,  print_financial_cop20)
    
    #test one
    print_financial_cop20("70212")

# MOVE TO DRIVE -----------------------------------------------------------
    
    #create folder for upload
    drive_mkdir("COP20 Financial and Programmatic Files",
                path = as_id(glbl_id)) #path is to the ER FY21 Folder but can be changed
    
    #identify list of   
    local_files <- list.files("ER21IMFinance", full.names = TRUE)
    
    #push to drive
    walk(local_files,
         ~ drive_upload(.x,
                        path = as_id(fldr_id), #path is to the ER21 test file folder
                        name = basename(.x),
                        type = "spreadsheet"))
    
    #remove all local files
    unlink("ER21IMFinance", recursive = TRUE)
    