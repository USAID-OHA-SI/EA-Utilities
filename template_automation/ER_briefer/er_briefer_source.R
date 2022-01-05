# Version 2.0
# Source code for ER briefer
# 2.0 update: Removed code that will be placed in the markdown code or

# Prep DF for 
fsd_prep_budget_ex <- function(df){
  df_out <- df %>%   
    #mutate data type double into integer to have round numbers
    dplyr::mutate_if(is.double, as.integer)%>%
    #drop NA for numeric amounts
    mutate_at(vars(cop_budget_total:expenditure_amt),~replace_na(.,0)) %>%
    #recode values to match naming in Financial Integrated Dataset
    dplyr::mutate(`interaction_type`= recode (`interaction_type`, 
                                              "Service Delivery"= "SD",
                                              "Non Service Delivery"= "NSD")) %>%
    dplyr::mutate(`interaction_type`  = dplyr::case_when(program == "PM" ~"PM",
                                                         TRUE ~`interaction_type`)) %>%
    
    # Produce alternative agency category that combines non-CDC/non-USAID as Other
    agency_category() %>%
    
    #mutating & calculating budget execution
    group_by(operatingunit, countryname, fundingagency, agency_category, fiscal_year,
             primepartner,mech, mech_code, program, interaction_type) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()
  return(df_out)
}

# Use df that is already subset to the ou level
# For group_col, use NA, "program", or "mech", or c("mech", "program)
gen_budget_exec <- function(df, group_col = NA){
  # Vector of strings that are column names to select
  group_cols <- c("fundingagency", "fiscal_year", "cop_budget_total", 
                  "expenditure_amt")
  if (!is.na(group_col)){
    group_cols <- append(group_cols, group_col, 1)
  }
  # head function drops last two elements, "cop_budget" and "expenditure" from vector
  group_by_cols <- head(group_cols, -2)
  
  # If grouping by OU x Program Area, use modified agency category variable
  condition <- group_col == "program"
  if (!is.na(condition)){
    if (condition){
      df <- df %>%
        dplyr::select(-fundingagency) %>%
        rename(fundingagency = agency_category)
    }
  }
  
  
  df_out <-df %>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    dplyr::select(group_cols) %>%
    mutate(fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    group_by_(.dots = group_by_cols)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup() %>%
    
    pivot_wider(names_from = fiscal_year,
                values_from = cop_budget_total:budget_execution, 
                values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020)
  return(df_out)
}

# FSD for merging with tgt data on MSD
progs<-c("HTS", "C&T","OVC")
gen_fsd_tgt <- function(df){
  df_out <- df %>% 
    filter(fiscal_year=="2021")%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    dplyr::mutate(program = dplyr::case_when(beneficiary == "OVC"~"OVC", 
                                             TRUE ~program))%>%
    group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, 
             primepartner, program) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()%>%
    filter(program %in% progs)%>%
    mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))
  return(df_out)
}


########################################################

### Unit Expenditure #################################### NO PREPFSD
gen_ue <- function(fsd_tgt, msd_tgt){
  #join datasets together 
  df_ue<-left_join(fsd_tgt, msd_tgt) %>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, primepartner,indicator)%>%
    pivot_longer(expenditure_amt:cop_budget_total,
                 names_to ="financial",
                 values_to="amount")%>%
    pivot_longer(cumulative:targets,
                 names_to ="programatic",
                 values_to="value")%>%
    ungroup()%>%
    
    filter(programatic=="cumulative")%>%
    pivot_wider(names_from = financial, values_from=amount) %>%
    pivot_wider(names_from = programatic, values_from=value) %>%
    
    dplyr::mutate(unit_expenditure=percent_clean(expenditure_amt,cumulative))%>%
    filter(fiscal_year=="2021")%>%
    
    select(operatingunit,fundingagency,mech_code, mech_name, primepartner,program, 
           indicator, unit_expenditure, cumulative)%>%
    pivot_wider(names_from =indicator, values_from=cumulative:unit_expenditure) %>%
    
    select(operatingunit,fundingagency,mech_code, mech_name, primepartner,
           cumulative_HTS_TST,cumulative_HTS_TST_POS,
           cumulative_TX_CURR,cumulative_TX_NEW,
           unit_expenditure_HTS_TST,unit_expenditure_HTS_TST_POS,
           unit_expenditure_TX_CURR,unit_expenditure_TX_NEW) %>%
    
    group_by(operatingunit,fundingagency,mech_code, mech_name, primepartner,)%>%
    summarise_at(vars(cumulative_HTS_TST: unit_expenditure_TX_NEW), 
                 sum, na.rm = TRUE) %>%
    
    mutate(primepartner=dplyr::case_when(primepartner=="FHI Development 360 LLC"~"FHI360",
                                         primepartner=="Family Health International"~"FHI360",
                                         primepartner=="Abt Associates, Inc."~"Abt Associates Inc",
                                         TRUE ~primepartner))%>%
    
    dplyr::mutate( mech = paste(mech_code,"-", mech_name)) %>%
    dplyr::relocate(mech, .before = cumulative_HTS_TST) %>%
    dplyr::relocate(unit_expenditure_HTS_TST , .before = cumulative_HTS_TST) %>%
    dplyr::relocate( unit_expenditure_HTS_TST_POS, .before = cumulative_HTS_TST_POS)%>%
    dplyr::relocate(unit_expenditure_TX_CURR, .before = cumulative_TX_CURR)%>%
    dplyr::relocate(unit_expenditure_TX_NEW, .before = cumulative_TX_NEW)%>%  
    dplyr::rename("cumulative_TST_POS"="cumulative_HTS_TST_POS")%>%
    dplyr::rename("unit_expenditure_TST_POS"="unit_expenditure_HTS_TST_POS")%>%
    ungroup%>%
    filter(unit_expenditure_HTS_TST>0 | cumulative_HTS_TST> 0 
           |unit_expenditure_TST_POS>0 |cumulative_TST_POS>0
           |unit_expenditure_TX_CURR>0 |cumulative_TX_CURR >0 
           |unit_expenditure_TX_NEW>0| cumulative_TX_NEW  >0) %>%
    select(fundingagency,mech:cumulative_TX_NEW)
  
  return(df_ue)
}



### Local Partner % #################################### NO PREP_FSD
gen_local <- function(df){
  df %>%
    dplyr::mutate(`mech_code`=as.character(`mech_code`))%>%
    glamr::apply_partner_type() %>%
    glamr::remove_sch("SGAC")%>%
    dplyr::filter(fundingagency=="USAID")
}

gen_local_table <- function(df){
  df %>% dplyr::filter(fiscal_year=="2021", 
                       partner_type_usaid_adjusted=="Local")%>%
    dplyr::select (c(mech,program,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(mech,program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    
    dplyr::mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,
                values_from = cop_budget_total:budget_execution, 
                values_fill = 0)%>%
    
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    filter(cop_budget_total_2021>0 | expenditure_amt_2021>0)
}

gen_graph_tbl <- function(df) {
  df %>% dplyr::filter(partner_type_usaid_adjusted=="Local" | partner_type_usaid_adjusted=="International",
                       !fiscal_year=="2022")%>%
    dplyr::select (c(partner_type_usaid_adjusted, fiscal_year,expenditure_amt))%>%
    group_by(partner_type_usaid_adjusted,fiscal_year)%>%
    summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
    pivot_wider(names_from = partner_type_usaid_adjusted, 
                values_from = expenditure_amt) %>%
    mutate(total = Local+International, #pivot to get totals and shares
           lp_share = Local / total*100,
           ip_share=International/total*100) %>%
    pivot_longer(cols = International:Local, names_to = "partner_type")
}
  
plot_local <- function(df){
  ggplot(df, aes(fiscal_year, value)) +
    geom_col(aes( fill = partner_type))+
    scale_y_continuous(labels=label_number_si(),
                       position = "left", expand = c(.005, .005)) +
    geom_text(aes(label=ifelse(partner_type=="Local",
                               paste0(round(lp_share),
                                      "%"),
                               " "),
                  vjust = 2))+

    scale_fill_manual(values = c(siei_dgrey, old_rose)) +
    scale_alpha_identity() +
    labs(x = "Fiscal Year", y = "Expenditure", fill = "Partner Type",
         title = glue("Expenditure by Partner Type: ou"),
         caption =  glue("Source: {source}-Excluding M&O and Commodities",
         )) +
    # si_style does NOT work in Rmarkdown
    # si_style_nolines()+
    theme(legend.position="bottom")
}

# glamr::load_secrets()
# 
# df_local<- gen_local(df_fsd)
# local_table <- gen_local_table(df_local)
# local_graph <- gen_graph_tbl(df_local)
# plot_local(local_graph)


### Breakdown by Service Delivery ####################################
gen_serv <- function(df){
  df %>% 
    glamr::remove_sch("SGAC")%>%
    filter(fundingagency=="USAID"|fundingagency=="CDC")%>%
    filter(!fiscal_year=="2022")%>%
    dplyr::select (c(fundingagency,interaction_type, fiscal_year,expenditure_amt))%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    group_by(fundingagency, interaction_type, fiscal_year)%>% 
    summarise(across(expenditure_amt, sum, na.rm = TRUE)) %>% 
    ungroup() %>%
    
    pivot_wider(names_from = interaction_type, values_from = expenditure_amt)%>%
    
    ##### MISSING PM AND SD/NSD are named incorrectly in code
    mutate(total = NSD+SD+PM, #pivot to get totals and shares
           nsd_share=NSD/total ,
           sd_share = SD / total ,
           pm_share=PM/total)%>%
    pivot_longer(cols = NSD:SD, names_to = "interaction_type")%>%
    group_by(fiscal_year,fundingagency,nsd_share, pm_share,sd_share,interaction_type,total)%>%
    summarise(value)%>%
    mutate(cumulative=cumsum(value))%>%
    ungroup%>%
    mutate(share=case_when(interaction_type=="NSD"~ nsd_share,
                           interaction_type=="SD"~sd_share,
                           interaction_type=="PM"~pm_share,
                           TRUE ~as.numeric(interaction_type)))%>%
    select(fundingagency,fiscal_year,interaction_type,cumulative,total,share) %>%
    
    ggplot(aes(fiscal_year,cumulative)) + #can add value to geom_col-use cumulative sum
    geom_col(aes(y = cumulative, fill = fct_rev(interaction_type))) +
    facet_wrap("fundingagency", scales='free_x')+
    scale_y_continuous(labels=label_number_si(),
                       position = "left", expand = c(.005, .005)) +
    scale_fill_manual(values = c(genoa,golden_sand,denim )) +
    geom_text(aes(y=cumulative, label = percent(share, 1)), 
              position=position_stack(vjust=.8))+
    
    scale_alpha_identity() +
    labs(x = "Fiscal Year", y = "Expenditure", fill = "Interaction Type",
         title = glue("Expenditure by Interaction Type (Excluding Commodities): {params$ou}"),
         caption =  glue("Source: {source}",
         )) +
    # si_style_nolines()+
    theme(legend.position="bottom")
}

# gen_serv(df_budget_exec)

### HIV Testing  ####################################
gen_ue_indiv <- function(fsd, msd){
  # additional transformations on msd before merge
  msd_tgt_indiv <- msd %>%
    mutate(achievement=round (cumulative/targets*100))%>%
    select(operatingunit,fundingagency,fiscal_year,mech_code,mech_name,program,
           indicator,achievement)%>%
    pivot_wider(names_from = indicator, values_from=achievement)
  
  #join datasets together 
  df_ue_indiv<-left_join(fsd, msd_tgt_indiv)%>%
    dplyr::mutate( mech = paste(mech_code,"-", mech_name)) %>%
    relocate(expenditure_amt, .before= cop_budget_total)%>%
    relocate(mech, .before= expenditure_amt)%>%
    select(operatingunit,fundingagency,program,mech:OVC_SERV)
  
  return(df_ue_indiv)
}

#Use below function to get HTS, C&T, OVC info for a specific OU
get_program_specific<-function(df_ue_temp, ou="operatingunit",programs=c("HTS","C&T","OVC")){
  df<-df_ue_temp%>%
    mutate_at(vars(expenditure_amt:OVC_SERV),~replace_na(.,0)) %>%
    filter(program %in% programs)%>%
    select_if(~!( all(. == 0)))%>%
    select(-(operatingunit))
  return(df)
}

# df_ue_indiv <- gen_ue_indiv(fsd_tgt, msd_tgt)
# hts_ue <- get_program_specific(df_ue_indiv, programs="HTS")
# 
# ### Treatment ####################################
# hts_ue <- get_program_specific(df_ue_indiv, programs="C&T")
# 
# ### OVC ####################################
# hts_ue <- get_program_specific(df_ue_indiv, programs="OVC")


### Commodities #################################### NO PREP FSD
psm_murder<-function(df){
  df_out<-df%>%
    filter(fiscal_year=="2021") %>%
    filter(str_detect(mech_name, 'GHSC'))%>%

    group_by(operatingunit, mech, program,sub_program) %>%
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()%>%
    select(mech,program: expenditure_amt)%>%
    relocate(expenditure_amt, .before= cop_budget_total)%>%
    relocate(mech, .before= program)%>%
    adorn_totals("row",,,, -mech)%>%
    mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))%>%
  return(df_out)
}

# df_mozambique<-psm_murder(df_fsd)

### HRH

### Mechanism Program Area table
# mechXpa <- gen_budget_exec(df_fsd, group_col = c("mech", "program"))
