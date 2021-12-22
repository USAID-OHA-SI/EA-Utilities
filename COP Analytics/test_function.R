



FAST_Intervention<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "Standard COP Matrix-R", skip=3)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::select( -c('Global','Prime Partner DUNS','Award Number',
                                'Appropriation Year', 'Initiative',
                                'Funding Category','GAP':'ESF', 
                                'Water':'AB/Y Denominator'))%>%
    dplyr::rename("Prime Partner Name" = `Partner Name`,
                  "Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  "Prime Partner Type" = `Partner Org Type`,
                  "Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Beneficiary" = `Major Beneficiary`,
                  "Sub Beneficiary" = `Minor Beneficiary`,
                  "COP Budget New Funding" = `Total New Funding Sources`,
                  "COP Budget Pipeline" = `Applied Pipeline Amount`) %>% 
    
    dplyr::mutate(`Data Stream`="FSD")   
  
  #Convert columns into characters
  df<-df%>%
    dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
    dplyr::mutate(`Mechanism Name`=as.character(`Mechanism Name`)) %>% 
    dplyr::mutate(`Operating Unit`= as.character(`Operating Unit`)) %>% 
    dplyr::mutate(`Prime Partner Name`=as.character(`Prime Partner Name`)) %>% 
    dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`)) %>% 
    dplyr::mutate(`Planning Cycle`=as.character(`Planning Cycle`)) %>% 
    dplyr::mutate(`Interaction Type`=as.character(`Interaction Type`)) %>% 
    dplyr::mutate(`Record Type`=as.character(`Record Type`)) %>% 
    dplyr::mutate(`Program Area`=as.character(`Program Area`)) %>% 
    dplyr::mutate(`Sub Program Area`=as.character(`Sub Program Area`)) %>% 
    dplyr::mutate(`Beneficiary`=as.character(`Beneficiary`)) %>% 
    dplyr::mutate(`Sub Beneficiary`=as.character(`Sub Beneficiary`)) %>% 
    dplyr::mutate(`Funding Agency`=as.character(`Funding Agency`)) %>% 
    dplyr::mutate(`Is Indigenous Prime Partner`= as.character(`Is Indigenous Prime Partner`)) %>% 
    dplyr::mutate(`Digital Health Investments`=as.character(`Digital Health Investments`)) %>% 
    dplyr::mutate(`Prime Partner Type`= as.character(`Prime Partner Type`))
  
  #convert budget columns to numeric
  df<-df%>%
    dplyr::mutate(`COP Budget New Funding`=as.numeric(`COP Budget New Funding`))%>%
    dplyr::mutate(`COP Budget Pipeline`=as.numeric(`COP Budget Pipeline`))%>%
    dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`)) %>% 
    
    #remove N/A's
    #Drop all rows without an OU specified 
    drop_na('Operating Unit')
  
  
  #replace NAs with 0s
  df<-df%>%
    mutate_at(vars(`COP Budget New Funding`:`Total Planned Funding`),~replace_na(.,0))
  
  #Add in agency category column to group agencies
  df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                             ifelse(`Agency Category` =="Dedup", "Dedup","Other")))) %>% 
    dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
  
  #recode values to match naming in Financial Integrated Dataset
  df<- df %>%  dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery")) %>%
    dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "NSD"= "Non Service Delivery"))
  
  return(df)
}


FAST_CCA<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "Standard COP Matrix-R", skip=3)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::select( -c('Global','Prime Partner DUNS','Award Number', 'Cost Type',
                                'Appropriation Year',  'Initiative',
                                'Funding Category','GAP':'Total Planned Funding', 
                                'COVID Adaptation-GHP-State':'Digital Health Investments'))%>%
    dplyr::rename("Prime Partner Name" = `Partner Name`,
                  "Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  "Prime Partner Type" = `Partner Org Type`,
                  "Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Beneficiary" = `Major Beneficiary`,
                  "Sub Beneficiary" = `Minor Beneficiary`) %>%    
    
    dplyr::mutate(`Data Stream`="FAST Cross Cutting Attribution")
  
  #remove N/A's
  df <- df %>%drop_na(`Operating Unit`)
  
  #replace NAs with 0s
  df<-df%>%
    mutate_at(vars(`Water`:`Key Populations: SW`),~replace_na(.,0))
  
  
  #Using pivot long to shift CCA vertically
  df <- df %>% pivot_longer(cols = `Water`:`Key Populations: SW`,
                            names_to = "Cross-Cutting Attribution",
                            values_to = "Total Planned Funding")
  
  #Convert columns into characters
  df<-df%>%
    dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
    dplyr::mutate(`Mechanism Name`=as.character(`Mechanism Name`)) %>% 
    dplyr::mutate(`Operating Unit`= as.character(`Operating Unit`)) %>% 
    dplyr::mutate(`Prime Partner Name`=as.character(`Prime Partner Name`)) %>% 
    dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`)) %>% 
    dplyr::mutate(`Planning Cycle`=as.character(`Planning Cycle`)) %>% 
    dplyr::mutate(`Interaction Type`=as.character(`Interaction Type`)) %>% 
    dplyr::mutate(`Record Type`=as.character(`Record Type`)) %>% 
    dplyr::mutate(`Program Area`=as.character(`Program Area`)) %>% 
    dplyr::mutate(`Sub Program Area`=as.character(`Sub Program Area`)) %>% 
    dplyr::mutate(`Beneficiary`=as.character(`Beneficiary`)) %>% 
    dplyr::mutate(`Sub Beneficiary`=as.character(`Sub Beneficiary`)) %>% 
    dplyr::mutate(`Funding Agency`=as.character(`Funding Agency`)) %>% 
    dplyr::mutate(`Is Indigenous Prime Partner`= as.character(`Is Indigenous Prime Partner`)) %>% 
    dplyr::mutate(`Prime Partner Type`= as.character(`Prime Partner Type`))
  
  #Convert CCA budget into numeric
  df<-df%>%
    dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`))
  
  #Add in agency category column to group agencies
  df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                             ifelse(`Agency Category` =="Dedup", "Dedup","Other")))) %>% 
    dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
  
  #recode values to match naming in Financial Integrated Dataset
  df<- df %>%  dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery")) %>%
    dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "NSD"= "Non Service Delivery"))
  
  
  return(df)
}

FAST_Initiative<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "Standard COP Matrix-R", skip=3)
  #include columns of interest
  ####change to Deselect!!!!!!WARNING
  df<- df %>%  dplyr::select('Planning Cycle','Operating Unit':'Partner Name', 'Mechanism Name':'Initiative', 'Implementation Year':'Minor Beneficiary', 'Total New Funding Sources':'Applied Pipeline Amount')  %>%
    #Rename to match BUDGET_ER_MER Dataset
    dplyr::rename("Prime Partner Name" =`Partner Name`, 
                  "Is Indigenous Prime Partner" =`Is Indigenous Partner?`,
                  "Prime Partner Type" =`Partner Org Type`,
                  "Initiative Name" =`Initiative`,
                  "Fiscal Year"= `Implementation Year`, 
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Beneficiary" =`Major Beneficiary`,
                  "Sub Beneficiary" =`Minor Beneficiary`,
                  "New Funding" =`Total New Funding Sources`,
                  "Applied Pipeline" =`Applied Pipeline Amount`,) %>%
    
    #Pivot COP Budget New Funding & COP Budget Pipeline to 'funding_type' with value as 'Total Planned Funding'
    gather(`Funding Type`,`Total Planned Funding`, `New Funding`:`Applied Pipeline`) %>%
    
    #Pivot GAP, GHP-STATE, GHP-USAID to 'funding_account' with value as 'COP Budget New Funding'
    #  gather(funding_account,`COP Budget New Funding`, `GAP`:`GHP-USAID`) %>%
    
    #Create variable 'Data stream' with Initiative
    dplyr::mutate(`Data Stream`="Initiative")  %>% #consider renaming to specify FAST
    
    #Convert columns into characters
    dplyr::mutate(`Initiative Name`= as.character(`Initiative Name`)) %>% 
    dplyr::mutate(`Funding Category`= as.character(`Funding Category`)) %>% 
    dplyr::mutate(`Mechanism Name`=as.character(`Mechanism Name`)) %>% 
    dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
    dplyr::mutate(`Operating Unit`= as.character(`Operating Unit`)) %>% 
    dplyr::mutate(`Prime Partner Name`=as.character(`Prime Partner Name`)) %>% 
    dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`)) %>% 
    dplyr::mutate(`Planning Cycle`=as.character(`Planning Cycle`)) %>% 
    dplyr::mutate(`Interaction Type`=as.character(`Interaction Type`)) %>% 
    dplyr::mutate(`Record Type`=as.character(`Record Type`)) %>% 
    dplyr::mutate(`Program Area`=as.character(`Program Area`)) %>% 
    dplyr::mutate(`Sub Program Area`=as.character(`Sub Program Area`)) %>% 
    dplyr::mutate(`Beneficiary`=as.character(`Beneficiary`)) %>% 
    dplyr::mutate(`Sub Beneficiary`=as.character(`Sub Beneficiary`)) %>% 
    dplyr::mutate(`Funding Agency`=as.character(`Funding Agency`)) %>% 
    dplyr::mutate(`Is Indigenous Prime Partner`= as.character(`Is Indigenous Prime Partner`)) %>% 
    dplyr::mutate(`Prime Partner Type`= as.character(`Prime Partner Type`)) %>% 
    
    #Convert  budget into numeric
    dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`)) %>% 
    
    #Replace NAs in numeric columns
    dplyr::mutate_at(vars(`Total Planned Funding`),~replace_na(.,0))%>%
    
    
    #Drop all rows without an OU specified 
    drop_na('Operating Unit')   %>%
    
    #recode values for different variables as needed
    dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery")) %>%
    dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "NSD"= "Non Service Delivery"))  %>%
    
    
    #Add in agency category column to group agencies
    dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                             ifelse(`Agency Category` =="Dedup", "Dedup","Other")))) %>% 
    dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
  return(df)
}

FAST_Commodities<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "Commodities-E", skip=3)
  #include columns of interest
  df<- df %>%  
    dplyr::rename("Specify Other Procurement" =`Specify 'Other' Procurement`) %>% 
    #dplyr::select('Mechanism ID': 'Beneficiary', 'Initiative Name', 'Major Category': 'Item', 'Specify Other Procurement','Commodity Quantity','Unit Price':'Total Item Budget')  %>%
    
    #change to one line code
    dplyr::mutate(`Program Area (Service Delivery Only)`= as.character(`Program Area (Service Delivery Only)`)) %>% 
    dplyr::mutate(`Initiative Name`= as.character(`Initiative Name`)) %>% 
    dplyr::mutate(`Major Category`= as.character(`Major Category`)) %>% 
    dplyr::mutate(`Minor Category`= as.character(`Minor Category`)) %>% 
    dplyr::mutate(`Beneficiary`= as.character(`Beneficiary`)) %>% 
    dplyr::mutate(`Item`= as.character(`Item`)) %>% 
    dplyr::mutate(`Specify Other Procurement`= as.character(`Specify Other Procurement`)) %>% 
    
    #Remove dashes in Facility-based testing and Community-Based Testing temporarily
    dplyr::mutate(`Program Area (Service Delivery Only)`= recode (`Program Area (Service Delivery Only)`, "HTS: Facility-based testing-SD"= "HTS: Facility based testing-SD")) %>%
    dplyr::mutate(`Program Area (Service Delivery Only)`= recode (`Program Area (Service Delivery Only)`, "HTS: Community-based testing-SD"= "HTS: Community based testing-SD")) %>%
    dplyr::mutate(`Program Area (Service Delivery Only)`= recode (`Program Area (Service Delivery Only)`, "HTS: Facility-based testing-NSD"= "HTS: Facility based testing-NSD")) %>%
    dplyr::mutate(`Program Area (Service Delivery Only)`= recode (`Program Area (Service Delivery Only)`, "HTS: Community-based testing-NSD"= "HTS: Community based testing-NSD"))%>%
    dplyr::rename("Program Area" =`Program Area (Service Delivery Only)`) %>%
    separate(col = "Program Area", into=c("Program Area", "Sub Program Area"), sep=":") %>%
    separate(col = "Sub Program Area", into=c("Sub Program Area", "Interaction Type"), sep="-") %>%
    
    # cSplit("Program Area", ":" ,, drop=FALSE) %>%
    # separate(col = "Program Area_2", into=c("Sub Program Area", "Interaction Type"), sep="-") %>%
    separate(col = "Beneficiary", into=c("Beneficiary", "Sub Beneficiary"), sep=":") %>%
    
    #Rename to match BUDGET_ER_MER Dataset
    # dplyr::rename("Program Area" =`Program Area_1`,) %>%
    #dplyr::rename("Commodity Unit Cost" =`Unit Cost`,) %>%
    #dplyr::rename("Commodity Unit Price" =`Unit Price`,) %>%
    dplyr::rename("Total Planned Funding" =`Total Item Budget`,) %>%
    #dplyr::rename("Commodity Item" =`Item`,) %>%
    
    
    #Create variables
    dplyr::mutate(`Data Stream`="FAST Commodities")  %>%
    dplyr::mutate(`Fiscal Year`="2023")  %>%
    dplyr::mutate(`Planning Cycle`="COP22")  %>%
    dplyr::mutate(`Record Type`="Implementing Mechanism")%>%
    
    
    #Replace NAs in numeric columns
    dplyr::mutate_at(vars(`Total Planned Funding`),~replace_na(.,0))%>%
    
    
    #Convert  character columns into Characters
    dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
    dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`)) %>% 
    
    #Convert  numeric  columns to numeric
    #WARNING probably need to change to on line!!!!
    #dplyr::mutate(`Commodity Unit Cost`=as.numeric(`Commodity Unit Cost`)) %>% 
    #dplyr::mutate(`Commodity Unit Price`=as.numeric(`Commodity Unit Price`)) %>% 
    #dplyr::mutate(`Commodity Quantity`=as.numeric(`Commodity Quantity`)) %>% 
    dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`)) %>% 
    
    #dplyr::mutate(`Procurement Management`=as.numeric(`Procurement Management`)) %>% 
    #dplyr::mutate(`Quality Assurance`=as.numeric(`Quality Assurance`)) %>% 
    #dplyr::mutate(`Global Freight`=as.numeric(`Global Freight`)) %>% 
    #dplyr::mutate(`In Country Logistics`=as.numeric(`In Country Logistics`)) %>% 
    #dplyr::mutate(`Data Quality`=as.numeric(`Data Quality`)) %>% 
    #dplyr::mutate(`Procurement Management $`=as.numeric(`Procurement Management $`)) %>% 
    #dplyr::mutate(`Quality Assurance $`=as.numeric(`Quality Assurance $`)) %>% 
    #dplyr::mutate(`Global Freight $`=as.numeric(`Global Freight $`)) %>% 
    #dplyr::mutate(`In Country Logistics $`=as.numeric(`In Country Logistics $`)) %>% 
    #dplyr::mutate(`Data Quality $`=as.numeric(`Data Quality $`)) %>% 
  
  #Drop all rows without a MECH ID specified 
  drop_na('Mechanism ID') %>%
    
    #recode values for different variables as needed
    dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery")) %>%
    dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "NSD"= "Non Service Delivery"))  %>%
    
    #Add dashes back in Facility-based testing and Community-Based Testing 
    dplyr::mutate(`Sub Program Area`= recode (`Sub Program Area`, "Facility based testing"= "Facility-based testing")) %>%
    dplyr::mutate(`Sub Program Area`= recode (`Sub Program Area`, "Community based testing"= "Community-based testing"))  
  
  return(df)
}

FAST_MECHSLIST<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "Mechs List-R")
  #include columns of interest
  #
  df<- df %>%  
    dplyr::select ("Operating Unit", "Mechanism ID") #%>% 
  #Rename to match BUDGET_ER_MER Dataset
  #dplyr::rename( 
  #"Prime Partner Name" =`Partner Name`, 
  #"Is Indigenous Prime Partner" =`Is Indigenous Partner?`,
  #) %>%
  
  dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
    
    #Add in agency category column to group agencies
    dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                             ifelse(`Agency Category` =="Dedup", "Dedup","Other"))))
  
  return(df)
} 

FAST_Earmarks_IM<-function(df){
  #nested read_csv. Can be removed and run separately
  df<-read_xlsx(df, "Standard COP Matrix-R", skip=3)
  
  # Drop columns you don't need and rename  
  df<- df %>% dplyr::select(-c('Global','Prime Partner DUNS','Award Number',
                               'Appropriation Year',  'Initiative',
                               'Funding Category','GAP':'Total Planned Funding', 
                               'Water':'COVID Adaptation-Applied Pipeline'))%>%
    dplyr::rename("Prime Partner Name" = `Partner Name`,
                  "Is Indigenous Prime Partner" = `Is Indigenous Partner?`,
                  "Prime Partner Type" = `Partner Org Type`,
                  "Fiscal Year" = `Implementation Year`,
                  "Program Area" = `Major Program`,
                  "Sub Program Area" = `Sub Program`,
                  "Interaction Type" = `SD/NSD`,
                  "Beneficiary" = `Major Beneficiary`,
                  "Sub Beneficiary" = `Minor Beneficiary`) %>% 
    dplyr::mutate(`Data Stream`="FAST Earmark")   
  
  #remove N/A's
  df <- df %>%drop_na(`Operating Unit`)
  
  #replace NAs with 0s
  df<-df%>%
    mutate_at(vars(`C&T Earmark`:`AB/Y Denominator`),~replace_na(.,0))
  
  #Using pivot long to shift Earmarks vertically
  df <- df %>% pivot_longer(cols = `C&T Earmark`:`AB/Y Denominator`,
                            names_to = "Earmark",
                            values_to = "Total Planned Funding")
  
  #Convert columns into characters
  df<-df%>%
    dplyr::mutate(`Mechanism ID`=as.character(`Mechanism ID`)) %>% 
    dplyr::mutate(`Mechanism Name`=as.character(`Mechanism Name`)) %>% 
    dplyr::mutate(`Operating Unit`= as.character(`Operating Unit`)) %>% 
    dplyr::mutate(`Prime Partner Name`=as.character(`Prime Partner Name`)) %>% 
    dplyr::mutate(`Fiscal Year`= as.character(`Fiscal Year`)) %>% 
    dplyr::mutate(`Planning Cycle`=as.character(`Planning Cycle`)) %>% 
    dplyr::mutate(`Interaction Type`=as.character(`Interaction Type`)) %>% 
    dplyr::mutate(`Record Type`=as.character(`Record Type`)) %>% 
    dplyr::mutate(`Program Area`=as.character(`Program Area`)) %>% 
    dplyr::mutate(`Sub Program Area`=as.character(`Sub Program Area`)) %>% 
    dplyr::mutate(`Beneficiary`=as.character(`Beneficiary`)) %>% 
    dplyr::mutate(`Sub Beneficiary`=as.character(`Sub Beneficiary`)) %>% 
    dplyr::mutate(`Funding Agency`=as.character(`Funding Agency`)) %>% 
    dplyr::mutate(`Digital Health Investments`=as.character(`Digital Health Investments`)) %>% 
    dplyr::mutate(`Is Indigenous Prime Partner`= as.character(`Is Indigenous Prime Partner`)) %>% 
    dplyr::mutate(`Prime Partner Type`= as.character(`Prime Partner Type`))
  
  #Convert Earmark budget into numeric
  df<-df%>%
    dplyr::mutate(`Total Planned Funding`=as.numeric(`Total Planned Funding`))
  
  #Add in agency category column to group agencies
  df<- df %>% dplyr::mutate(`Agency Category` = `Funding Agency`)%>%
    mutate(`Agency Category` = ifelse(`Agency Category` == "USAID", "USAID",
                                      ifelse(`Agency Category` == "HHS/CDC", "CDC",
                                             ifelse(`Agency Category` =="Dedup", "Dedup","Other")))) %>% 
    dplyr::mutate(`Agency Category`= as.character(`Agency Category`))
  #recode values to match naming in Budget-ER-MER Dataset
  df<- df %>%  dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "SD"= "Service Delivery")) %>%
    dplyr::mutate(`Interaction Type`= recode (`Interaction Type`, "NSD"= "Non Service Delivery"))
  
  return(df)
}
