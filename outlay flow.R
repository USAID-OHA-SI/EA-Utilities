# PURPOSE: Munge and Analysis of Budget Data
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-08-27
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
    library(readxl)
    library(gt)
    
  

  

# LOAD budget files DATA ============================================================================  

    
    
    df_fy21q3_report <- read_excel("~/Data/FY21Q3/FY21Q3 O&O USAID Submission 16.8.21.xlsx", 
                                   sheet = "OGAC Deliverable", skip = 1)%>%
      clean_names()%>%
      dplyr::mutate_at(vars(available_adjustments),~replace_na(.,0))%>%
      mutate("available_net"=available+available_adjustments)
      
   
    
    df_fy21q2_report<-read_excel("~/Data/FY21Q3/FY21 Q2 O&O USAID Submission 21.05.24 (1).xlsx",
                                 sheet = "OGAC Deliverable")%>%
      clean_names() 
     
    
    df_fy20q4_report<-read_excel("~/Data/FY21Q3/FY20 Q4 O&O USAIDv2.xlsx",
                                 sheet = "OGAC Deliverable")%>%
      clean_names() 
    df_fy21q1_report<-read_excel("~/Data/FY21Q3/FY21 Q1 O&O USAID Submission 21.03.04.xlsx")%>%
      clean_names()
      
# MUNGE budget team data ============================================================================
  
   
    df_fy20q4_report<-df_fy20q4_report%>%
      pivot_longer(cols=available:pipeline_check,
                   names_to="type",
                   values_to="FY20Q4")
    
    df_fy21q2_report<-df_fy21q2_report%>%
      pivot_longer(cols=available:pipeline_total,
                   names_to="type",
                   values_to="FY21Q2") 
    
    df_fy21q3_report<-df_fy21q3_report%>%
      pivot_longer(cols=available:available_net,
                   names_to="type",
                   values_to="FY21Q3")
    
    df_fy21q1_report<-df_fy21q1_report%>%
      pivot_longer(cols=available:pipeline_total,
                   names_to="type",
                   values_to="FY21Q1")
    
    df_all_data<-full_join(df_fy21q3_report,df_fy21q2_report,by = c("fund_year", "fund", "program", "country_program", "type"))%>%
      full_join(df_fy20q4_report,by = c("fund_year", "fund", "program", "country_program", "type"))%>%
      full_join(df_fy21q1_report,by = c("fund_year", "fund", "program", "country_program", "type"))
  
df_all_data<-df_all_data%>%
  mutate_at(vars(starts_with("FY")),~replace_na(.,0))%>%
  dplyr::mutate(country_program = dplyr::case_when(country_program    == "Democratic Republic of Congo"    ~"Democratic Republic of the Congo", #rename DRC
                                                   
                                                   TRUE ~country_program))

#get outlay OU  data ============================================================
df_all_data_ou_oultays<-df_all_data%>%
pivot_longer(cols=FY21Q3:FY21Q1,
               names_to="period",
               values_to="value")%>%
  dplyr::group_by(fund, program, country_program,period,type)%>%
  summarise_at(vars(value), sum, na.rm = TRUE)%>%
  pivot_wider(
  
  names_from=period,
  values_from=value
)%>%
   dplyr::mutate("FY21Q3"=FY21Q3-FY20Q4)%>%
  dplyr::mutate("FY21Q2"=FY21Q2-FY20Q4)%>%
  dplyr::mutate("FY20Q4"=FY20Q4)%>%
  dplyr::mutate("FY21Q1"=FY21Q1-FY20Q4)%>%
    #dplyr::select(-c(fy20q4,fy21q2,fy21q3,fy21q1))
  #df_all1<-df_all1%>%
    pivot_longer(cols=FY20Q4:FY21Q3,
                 names_to="period",
                 values_to="value")
 
    

# munge SCM to get COP budget ============================================================================
  df_scm<-si_path()%>%
    return_latest("Matrix")%>%
    read.csv()%>%
    clean_names()
  df_scm<-df_scm%>%
    mutate(implementation_year=as.character(implementation_year))%>%
    mutate(total_planned_funding=as.numeric(total_planned_funding))%>%
    dplyr::mutate_at(vars(total_planned_funding),~replace_na(.,0))%>%
    #dplyr::mutate(
      #funding_agency = case_when(
       # funding_agency == "USAID/WCF"~ "USAID",
        #TRUE ~ funding_agency ))
    #dplyr::filter(funding_agency==c("USAID","USAID/WCF"))%>%
    dplyr::select(operating_unit,implementation_year, total_planned_funding,)%>%
    group_by(operating_unit,implementation_year,)%>%
    summarise_at(vars(total_planned_funding), sum, na.rm = TRUE)%>%
    mutate("period"= (case_when(
      implementation_year=="FY 2022" ~"FY22",
      implementation_year=="FY 2021"~"FY21",
      TRUE ~ implementation_year )))%>%
    dplyr::filter(implementation_year=="FY 2021")%>%
    mutate("type"="total planned funding")%>%
    mutate("program"="PEPFAR")%>%
    rename("country_program"=operating_unit,
           "value"= total_planned_funding)

  #bind budget outlays and SCM  
df_ou_outlay_full<-bind_rows( df_all_data_ou_oultays, df_scm)%>%
  rename(fund_year= implementation_year)%>%
  mutate(fund_year= (case_when(
    period=="FY21Q1" ~"FY21",
    period=="FY21Q2" ~"FY21",
    period=="FY21Q3" ~"FY21",
    period=="FY21Q4" ~"FY21",
    period=="FY20Q4" ~"FY20",
    TRUE ~ period)))

  write.csv(df_ou_outlay_full, paste0("Budget Outlay files", "-",Sys.Date(), ".csv"))
  
  
  ##get fund year and fund type data==========================================================
  df_fund_year_data<-df_all_data%>%
    pivot_longer(cols=FY20Q4:FY21Q3,
                 names_to="period",
                 values_to="value")%>%
    dplyr::group_by(fund_year,period,type)%>%
    summarise_at(vars(value), sum, na.rm = TRUE)%>%
    ungroup()%>%
    dplyr::filter(period=="FY21Q3")%>%
    pivot_wider(
      
      names_from="type",
      values_from=value)%>%
    
    ##Availabel+expired-obligations=unobligated
    #  unliqd=obs-outlays
    #total pipe=avail+expired-outlays
    #unliuquid=obs-outlays
    
    dplyr::mutate("unobligated"=available_net+expired-obligations)%>%
    dplyr::mutate("unliquidated"=obligations-outlays)%>%
    dplyr::mutate("pipeline_perct"=pipeline_total/available_net)%>%
    dplyr::relocate(pipeline_perct, .before = unobligated)%>%
    pivot_longer(cols=available:unliquidated,
                 names_to="type",
                 values_to="value")%>%
  dplyr::rename("Program"="fund_year")%>%
    dplyr::mutate("data_stream"="fund year")
  
  df_fund_type_data<-df_all_data%>%
    pivot_longer(cols=FY20Q4:FY21Q3,
                 names_to="period",
                 values_to="value")%>%
    dplyr::group_by(program,period,type)%>%
    summarise_at(vars(value), sum, na.rm = TRUE)%>%
    ungroup()%>%
    #dplyr::filter(period=="fy21q3")%>%
    filter(!program=="NA")%>%
    pivot_wider(
      
      names_from="type",
      values_from=value)%>%
    
    ##Availabel+expired-obligations=unobligated
    #  unliqd=obs-outlays
    #total pipe=avail+expired-outlays
    #unliuquid=obs-outlays
    
    dplyr::mutate("unobligated"=available_net+expired-obligations)%>%
    dplyr::mutate("unliquidated"=obligations-outlays)%>%
    dplyr::mutate("pipeline_perct"=pipeline_total/available_net)%>%
    dplyr::relocate(pipeline_perct, .before = unobligated)%>%
    pivot_longer(cols=available:unliquidated,
                 names_to="type",
                 values_to="value")%>%
    dplyr::rename("Program"="program")%>%
    dplyr::mutate("data_stream"="program")
  
  #write.csv(df_fund_year_data, paste0("Fund year data", "-",Sys.Date(), ".csv"))
  
  df_budget_fund_type<-bind_rows(df_fund_type_data,df_fund_year_data)
  
  write.csv(df_budget_fund_type, paste0("Budget type data", "-",Sys.Date(), ".csv"))
  
 
  
  
      
   
## table functions============================    
  
#Use the below to print tables by FY and program type. Select the period which you'd like in order for this to work  
  
  
get_pretty_budget_tables<-function(df,data_streams=c("fund year,program"), periods=c("FY20Q4","FY21Q1","FY21Q2","FY21Q3","FY21Q4","FY22Q1")){
  df<-df%>%
  
    dplyr::filter(period%in% periods)%>%
    dplyr::filter(data_stream %in% data_streams)%>%
    pivot_wider(
      
      names_from="type",
      values_from=value)%>%
    
      dplyr::select(-(period))%>%
  gt()%>% 
  cols_hide(
    column = c(
      "available":"pipeline_check","data_stream"
    ))%>%
  fmt_percent(
    columns = c(`pipeline_perct`),
    decimals = 2)%>%
  fmt_currency( # add dolar signs
    columns = c(`pipeline_total`,`unobligated`,`unliquidated`),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  
  cols_width(
    everything() ~ px(140))%>%
  cols_label(
    #program = "Program",
    pipeline_total = "Pipeline Total",
   pipeline_perct = "Pipeline Percent of Total Available",
    unobligated="Unobligated",
   unliquidated = "Unliquidated"
  )%>%
    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1.5),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))%>%
  tab_header(
    title = glue::glue("{periods} Account Financial Performance Summary"))%>%
  gt::tab_source_note(paste0("Created by the Budget Branch in Conjunction with the EA Branch on"," ", Sys.Date()))
return(df)
}


