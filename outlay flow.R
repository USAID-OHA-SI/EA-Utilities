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
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  

# LOAD DATA ============================================================================  

    
    
    df_fy21q3_report <- read_excel("~/Data/FY21Q3/FY21Q3 O&O USAID Submission 16.8.21.xlsx", 
                                   sheet = "OGAC Deliverable", skip = 1)%>%
      clean_names()%>%
      dplyr::mutate_at(vars(available_adjustments),~replace_na(.,0))%>%
      mutate("available_net"=available+available_adjustments)
      
     # mutate("period"="fy21q3")
    
    df_fy21q2_report<-read_excel("~/Data/FY21Q3/FY21 Q2 O&O USAID Submission 21.05.24 (1).xlsx",
                                 sheet = "OGAC Deliverable")%>%
      clean_names() 
      #mutate("period"="fy21q2")
    
    df_fy20q4_report<-read_excel("~/Data/FY21Q3/FY20 Q4 O&O USAIDv2.xlsx",
                                 sheet = "OGAC Deliverable")%>%
      clean_names() 
    df_fy21q1_report<-read_excel("~/Data/FY21Q3/FY21 Q1 O&O USAID Submission 21.03.04.xlsx")%>%
      clean_names()
      #mutate("period"="fy20q4")
# MUNGE budget team data ============================================================================
  
   
    df_fy20q4_report<-df_fy20q4_report%>%
      pivot_longer(cols=available:pipeline_check,
                   names_to="type",
                   values_to="fy20q4")
    
    df_fy21q2_report<-df_fy21q2_report%>%
      pivot_longer(cols=available:pipeline_total,
                   names_to="type",
                   values_to="fy21q2") 
    
    df_fy21q3_report<-df_fy21q3_report%>%
      pivot_longer(cols=available:available_net,
                   names_to="type",
                   values_to="fy21q3")
    
    df_fy21q1_report<-df_fy21q1_report%>%
      pivot_longer(cols=available:pipeline_total,
                   names_to="type",
                   values_to="fy21q1")
    
    df_all_data<-full_join(df_fy21q3_report,df_fy21q2_report,by = c("fund_year", "fund", "program", "country_program", "type"))%>%
      full_join(df_fy20q4_report,by = c("fund_year", "fund", "program", "country_program", "type"))%>%
      full_join(df_fy21q1_report,by = c("fund_year", "fund", "program", "country_program", "type"))
  
df_all_data<-df_all_data%>%
  mutate_at(vars(fy21q3,fy21q2,fy20q4,fy21q1),~replace_na(.,0))%>%
  dplyr::mutate(country_program = dplyr::case_when(country_program    == "Democratic Republic of Congo"    ~"Democratic Republic of the Congo", #rename DRC
                                                   
                                                   TRUE ~country_program))
#get outlay OU  data 
df_all_data_ou_oultays<-df_all_data%>%
pivot_longer(cols=fy21q3:fy21q1,
               names_to="period",
               values_to="value")%>%
  dplyr::group_by(fund, program, country_program,period,type)%>%
  summarise_at(vars(value), sum, na.rm = TRUE)%>%
  pivot_wider(
  
  names_from=period,
  values_from=value
)%>%
   dplyr::mutate("fy21_q3"=fy21q3-fy20q4)%>%
  dplyr::mutate("fy21_q2"=fy21q2-fy20q4)%>%
  dplyr::mutate("fy20_q4"=fy20q4)%>%
  dplyr::mutate("fy21_q1"=fy21q1-fy20q4)

  df_all1<-df_all_data_ou_oultays%>%
    dplyr::select(-c(fy20q4,fy21q2,fy21q3,fy21q1))
  df_all1<-df_all1%>%
    pivot_longer(cols=fy21_q3:fy21_q1,
                 names_to="period",
                 values_to="value")
 
    

# munge SCM ============================================================================
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
      implementation_year=="FY 2022" ~"fy22",
      implementation_year=="FY 2021"~"fy21",
      TRUE ~ implementation_year )))%>%
    dplyr::filter(implementation_year=="FY 2021")%>%
    mutate("type"="total planned funding")%>%
    mutate("program"="PEPFAR")%>%
    rename("country_program"=operating_unit,
           "value"= total_planned_funding)
df_ou_outlay_full<-bind_rows(df_all1, df_scm)%>%
  rename(fund_year= implementation_year)%>%
  mutate(fund_year= (case_when(
    period=="fy21_q1" ~"fy21",
    period=="fy21_q2" ~"fy21",
    period=="fy21_q3" ~"fy21",
    period=="fy21_q4" ~"fy21",
    period=="fy20_q4" ~"fy20",
    TRUE ~ period)))
  
  
   
    
#write.csv(df_all1, "budget files.8.27.2021.csv")
  write.csv(df_ou_outlay_full, "Budget files.9.2.2021.csv")
  
  
  ##=======get fund year data
  df_fund_year_data<-df_all_data%>%
    pivot_longer(cols=fy21q3:fy21q1,
                 names_to="period",
                 values_to="value")%>%
    dplyr::group_by(fund_year,period,type)%>%
    summarise_at(vars(value), sum, na.rm = TRUE)%>%
    ungroup()%>%
    dplyr::filter(period=="fy21q3")%>%
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
    dplyr::relocate(pipeline_perct, .before = unobligated)
  
  get_pretty_budget_tables_fund_year<-function(df){
    df_fund_year_data%>%
      dplyr::select(-(period))%>%
      gt()%>% 
      cols_hide(
        column = c(
          "available":"pipeline_check"
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
        fund_year = "FY",
        pipeline_total = "Pipeline Total",
        pipeline_perct = "Pipeline Percent of Total Available",
        unobligated="Unobligated",
        unliquidated = "Unliquidated"
      )%>%
      tab_header(
        title = glue::glue(" Fiscal Year 2021 Q2  Financial Performance Summary"))%>%
      gt::tab_source_note("Created by EA Branch and Budget Branch")
    return(df)
  }

    
    ##=======get fund type data
    df_fund_type_data<-df_all_data%>%
      pivot_longer(cols=fy21q3:fy21q1,
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
      dplyr::relocate(pipeline_perct, .before = unobligated)
    
    ## for tableau
    df_fund_type_data<-df_all_data%>%
      pivot_longer(cols=fy21q3:fy21q1,
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
                   values_to="value")
      
    write.csv(df_fund_type_data,"fy21q2 fund type data.csv")
    
get_pretty_budget_tables_fund_type<-function(df, periods=c("fy20q4","fy21q1","fy21q2","fy21q3","fy21q4")){
  df<-df_all_data%>%
    pivot_longer(cols=fy21q3:fy21q1,
                 names_to="period",
                 values_to="value")%>%
    dplyr::group_by(program,period,type)%>%
    summarise_at(vars(value), sum, na.rm = TRUE)%>%
    ungroup()%>%
    dplyr::filter(period=="fy21q3" )%>%
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
      dplyr::select(-(period))%>%
  gt()%>% 
  cols_hide(
    column = c(
      "available":"pipeline_check"
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
    program = "Program",
    pipeline_total = "Pipeline Total",
   pipeline_perct = "Pipeline Percent of Total Available",
    unobligated="Unobligated",
   unliquidated = "Unliquidated"
  )%>%
  tab_header(
    title = glue::glue(" Fiscal Year {periods} Account Financial Performance Summary"))%>%
  gt::tab_source_note("Created by EA Branch and Budget Branch")
return(df)
}


