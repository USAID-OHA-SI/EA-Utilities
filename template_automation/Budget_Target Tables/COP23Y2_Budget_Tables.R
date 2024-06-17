# PURPOSE: Pull budgets for partners
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-09-22
# NOTES: use this function to pull partner COP20 Budget, workplan budget, expenditure

# LOCALS & SETUP ============================================================================

  # Libraries
install.packages("janitor")
library(glamr)
library(gophr)
library(googlesheets4)
library(glue)
library(googledrive)
library(janitor)
library(tidyverse)
library(fs) #to create folders

# Output folder ============================================================================

load_secrets()

#global material folder
glbl_id <- "1YjTe1H2oQjsPX_Tr7kirRUOnC0KKe4e0" #ER21 Folder

#upload folder
fldr_id <- "1mFOnqSNeRYCpEzN9Kzu0HJBQ88x2maXG" #ER21 special folder for these files. Get this once you set up the folder below
    


    
  
  # Set paths  
    proj_path()
   
    si_path() 
    
  
    
  # Functions  
  
    
    print_financial_cop24 <- function(mechs){
      df_mech <- df_totals %>% 
        filter(`Mechanism ID` %in% mechs)
   
        
        df1<-df_mech %>%
      adorn_totals("row",,,,-`Fiscal Year`) %>%
          dplyr::mutate(`Fiscal Year`=as.integer(`Fiscal Year`)) %>% 
          dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
          tail(1)%>%as.vector()#%>%
  
 
      
      meta <- df_mech %>% 
        distinct(Country, `Mechanism ID`) %>%
        mutate(Country = str_remove_all(Country, " |'"),
               name = glue("ER21_Financial_Programmatic/COP23Y2__{Country}_{`Mechanism ID`}_Budget.csv"))
      
      note2<-data.frame(Country="The data above presents Nominal COP23 Year 2 and ROP24 budget data from the COP23/FY25 & ROP24/FY25-26 FAST PAW Dossier as of 6/06/2022. Please consult with your A/COR or AM to validate these figures..")
      
      note3<-data.frame(Country="For questions please reach out to the EA Branch at oha.ea@usaid.gov")
      note1<-data.frame(Country=" ")
      df_mech<-bind_rows(df_mech,df1,note1,note2,note3)
      
      print(glue("Printing...{meta$Country}-{meta$`Mechanism ID`}"))
      write_csv(df_mech, file.path(meta$name), na = "")
    }

# LOAD DATA ============================================================================  

    df_fsd<-si_path()%>%
      return_latest("FAST XL")%>%
      gophr::read_psd()
    
    df_fsd2 <-  read_csv("C:/Users/jmontespenaloza/Documents/Raw Datasets/FAST.csv", col_names = TRUE)
# MUNGE ============================================================================
  #get financial total
    df_totals <- df_fsd2 %>% 
             filter(`Data Stream` == "FSD") %>% 
             filter(`Agency Category` == "USAID") %>%
      filter(`record_type` == "Implementing Mechanism")%>%
      filter(`Planning Cycle` == "COP24")%>%
      mutate(`Country` = ifelse(`Operating Unit` == `Country`, `Operating Unit`, glue("{`Operating Unit`}-{`Country`}")))%>%
    
      mutate("Program Area: Sub Program Area-Service Level"=glue("{`Program Area`}: {`Sub Program Area`}- {`interaction_type`}"))%>%
    
      #mutate("Beneficiary-Sub Beneficiary"=glue("{Beneficiary}- {`Sub Beneficiary`}"))%>%  
      #mutate("Cost Category-Sub Cost Category"=glue("{cost_category}- {sub_cost_category}"))%>%
      group_by(Country, `Mechanism ID`, `Mechanism Name`, `Prime Partner Name`, `Fiscal Year`, `Program Area: Sub Program Area-Service Level`,`Targeted Beneficiary`) %>% 
      #group_by(country, mech_code, mech_name, primepartner, fiscal_year, `Program Area: Sub Program Area-Service Level`,`Beneficiary-Sub Beneficiary`)%>%
      #summarise_at(vars(cop_budget_total, workplan_budget_amt, expenditure_amt), sum, na.rm = TRUE) %>% 
      summarise_at(vars(`Total Planned Funding`), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      arrange(Country, `Mechanism ID`) %>% 
      dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`))
   
# CREATE Budget  FILES -----------------------------------------------------
    
    #list of mechanism
    mechs <- df_totals %>% 
      distinct(`Mechanism ID`) %>% 
      pull()   
    
    #create output folders folders
    dir_create("ER21_Financial_Programmatic")
  
    #create output budget files
    walk(mechs,  print_financial_cop24)
    
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
    