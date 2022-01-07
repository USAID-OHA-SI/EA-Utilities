# Version 2.1
# Source code for ER briefer
# 2.1 update: Removed code that will be placed in the markdown code or

# install.packages("extrafont")
# install.packages("fontcm")
# library(extrafont)
# font_install("fontcm")

# remotes::install_version("Rttf2pt1", version = "1.3.8")

# Generates an ordered dataframe for using as indices for grouping rows in Kable
ordered_table <- function(dataframe, col){
  output<- dataframe %>% count(across(col))%>%
    pivot_wider(names_from=matches(col), values_from=n)
  # uniq <- unique(dataframe)
  # output <- rbind(label=uniq,
  #                 count=sapply(uniq,function(x)sum(dataframe==x))) %>%
  #   data.frame()%>%
  #   slice(2)%>%
  #   mutate_all(as.numeric)
  return(output)
}

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

plot_local <- function(df, ou){
  ggplot(df, aes(fiscal_year, value)) +
    geom_col(aes( fill = partner_type))+
    scale_y_continuous(labels=label_number_si(),
                       position = "left", expand = c(.005, .005)) +
    geom_text(aes(label=ifelse(partner_type=="Local",
                               paste0(round(lp_share),
                                      "%"),
                               " "),
                  vjust = 1.5),
              size=3)+

    scale_fill_manual(values = c(siei_dgrey, old_rose)) +
    scale_alpha_identity() +
    labs(x = "Fiscal Year", y = "Expenditure", fill = "Partner Type",
         title = glue("Expenditure by Partner Type: {ou}"),
         caption =  glue("Source: {source}-Excluding M&O and Commodities",
         )) +
    # si_style does NOT work in Rmarkdown
    # si_style_nolines()+
    theme(legend.position="bottom",
          text=element_text(family="Goudy Old Style"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
          )
}


# glamr::load_secrets()
# 
# df_local<- gen_local(df_fsd)
# local_table <- gen_local_table(df_local)
# local_graph <- gen_graph_tbl(df_local)
# plot_local(local_graph, "Mozambique")


### Breakdown by Service Delivery ####################################
gen_serv <- function(df, ou){
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
              position=position_stack(vjust=0.5),
              size = 2)+
    
    scale_alpha_identity() +
    labs(x = "Fiscal Year", y = "Expenditure", fill = "Interaction Type",
         title = glue("Expenditure by Interaction Type (Excluding Commodities): {ou}"),
         caption =  glue("Source: {source}",
         )) +
    # si_style_nolines()+
    theme(legend.position="bottom",
          text=element_text(family="Goudy Old Style"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
          )
}

### HIV Testing  ####################################
gen_ue_indiv <- function(fsd, msd){
  # additional transformations on msd before merge
  msd_tgt_indiv <- msd %>%
    mutate(achievement=round (cumulative/targets*100))%>%
    select(operatingunit,fundingagency,fiscal_year,mech_code,mech_name,program,
           indicator,achievement)%>%
    pivot_wider(names_from = indicator, values_from=achievement)

  # Must check if OVC_SERV is included in the ou 
  if(!"OVC_SERV" %in% names(msd_tgt_indiv)){
    msd_tgt_indiv["OVC_SERV"] = 0
    }
  #join datasets together 
  df_ue_indiv<-left_join(fsd, msd_tgt_indiv)%>%
    dplyr::mutate( mech = paste(mech_code,"-", mech_name)) %>%
    relocate(expenditure_amt, .before= cop_budget_total)%>%
    relocate(mech, .before= expenditure_amt)%>%
    select(operatingunit,fundingagency,program,mech,
           expenditure_amt, cop_budget_total, budget_execution,
           HTS_TST, HTS_TST_POS, TX_CURR, TX_NEW, OVC_SERV)
  
  return(df_ue_indiv)
}

#Use below function to get HTS, C&T, OVC info for a specific OU
get_program_specific<-function(df_ue_temp, programs=c("HTS","C&T","OVC")){
  df<-df_ue_temp%>%
    mutate_at(vars(expenditure_amt:OVC_SERV),~replace_na(.,0)) %>%
    filter(program %in% programs)%>%
    select_if(~!( all(. == 0)))%>%
    select(-any_of(c("operatingunit")))
  return(df)
}


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

### HRH ======================================================================
## First table
gen_hrh_main <- function(df_hrh, df_fsd){
  df_hrh1<-df_hrh%>%
    group_by(operatingunit,fundingagency,fiscal_year,)%>%
    summarise_at(vars(annual_fte,individual_count,actual_annual_spend), 
                 sum, na.rm = TRUE)%>%
    ungroup()
  
  df_fsd_temp <- df_fsd %>% 
    filter(fiscal_year == "2021") %>%
    group_by(operatingunit,fundingagency,fiscal_year, )%>%
    summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
    ungroup()
    
  
  hrh_fsd1<-left_join(df_hrh1,df_fsd_temp)%>%
    select(-c(fiscal_year,operatingunit))%>%
    adorn_totals("row",,,, -fundingagency)%>%
    mutate(hrh_share=round(actual_annual_spend/expenditure_amt *100))
  return(hrh_fsd1)
}

fncols <- function(data, cname) {
  add <-cname[!cname%in%names(data)]
  
  if(length(add)!=0) data[add] <- 0
  data
}

## Generate HRH staff breakdown table
gen_staff <- function(df_hrh){
  df_hrh_mod <- df_hrh%>%
    dplyr::mutate(interaction_type=dplyr::case_when(interaction_type=="Service Delivery"~"SD",
                                                    interaction_type=="Non Service Delivery"~"NSD",
                                                    TRUE ~interaction_type))%>%
    mutate(er_staff=glue("{er_category}-{interaction_type}"))%>%
    group_by(operatingunit,fundingagency,fiscal_year,er_staff,)%>%
    summarise_at(vars(actual_annual_spend,annual_fte), sum, na.rm = TRUE)%>%
    ungroup%>%
    pivot_longer(actual_annual_spend:annual_fte, names_to="key", values_to="value")%>%
    pivot_wider(names_from = c(er_staff, key), 
                values_from = value,
                values_fill=0)
  
  
  # https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
  full_staff_cols <- c("Other Staff-NSD_actual_annual_spend",
                       "Other Staff-NSD_annual_fte",
                       "Program Management-NSD_actual_annual_spend",
                       "Program Management-NSD_annual_fte",
                       "HCW: Clinical-SD_actual_annual_spend",
                       "HCW: Clinical-SD_annual_ft",
                       "HCW: Ancillary-SD_actual_annual_spend",
                       "HCW: Ancillary-SD_annual_fte"
                       )
  
  df_hrh_mod <- fncols(df_hrh_mod, full_staff_cols)


  df_hrh_mod %>%
    select(-c(fiscal_year,operatingunit))%>%
    adorn_totals("row",,,, -fundingagency,)%>%
    dplyr::rowwise() %>%
    mutate(total_spend=sum(across(matches("annual_spend"), na.rm = T)))%>%
    mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
    mutate( #pivot to get totals and shares
      other_staff_spend_share=round(`Other Staff-NSD_actual_annual_spend`/total_spend*100) ,
      other_staff_fte_share=round(`Other Staff-NSD_annual_fte`/total_fte*100) ,
      pm_spend_share=round(`Program Management-NSD_actual_annual_spend`/total_spend*100),
      pm_fte_share=round(`Program Management-NSD_annual_fte` /total_fte*100) ,
      hcw_clinial_spend_share=round(`HCW: Clinical-SD_actual_annual_spend`/total_spend*100) ,
      hcw_clinical_fte_share=round(`HCW: Clinical-SD_annual_fte`/total_fte*100) ,
      hcw_ancillary_spend_share=round(`HCW: Ancillary-SD_actual_annual_spend`/total_spend*100) ,
      hcw_ancillary_fte_share=round(`HCW: Ancillary-SD_annual_fte`/total_fte*100))%>% 
    select(-c(total_spend,total_fte,))%>%
    
    dplyr::relocate(pm_spend_share, .after = `Program Management-NSD_actual_annual_spend`)%>%
    dplyr::relocate(pm_fte_share, .after = `Program Management-NSD_annual_fte`)%>%
    dplyr::relocate(other_staff_spend_share, .after = `Other Staff-NSD_actual_annual_spend`)%>%
    dplyr::relocate(other_staff_fte_share, .after = `Other Staff-NSD_annual_fte`)%>%
    dplyr::relocate(hcw_clinial_spend_share, .after = `HCW: Clinical-SD_actual_annual_spend`)%>%
    dplyr::relocate(hcw_clinical_fte_share, .after = `HCW: Clinical-SD_annual_fte`)%>%
    dplyr::relocate(hcw_ancillary_spend_share, .after = `HCW: Ancillary-SD_actual_annual_spend`)%>%
    dplyr::relocate(hcw_ancillary_fte_share, .after = `HCW: Ancillary-SD_annual_fte`)
} 

gen_hrh_mer <- function(df_hrh, df_msd){
  df_hrh3<-df_hrh%>%
    dplyr::mutate(interaction_type= dplyr::case_when(interaction_type == "Service Delivery"~"SD",
                                                     interaction_type == "Non Service Delivery"~"NSD",
                                                     TRUE ~interaction_type))%>%
    mutate(pa_level=glue("{interaction_type}-{program}"))%>%
    group_by(fundingagency,operatingunit,fiscal_year,pa_level)%>%
    summarise_at(vars(actual_annual_spend,annual_fte,), sum, na.rm = TRUE)%>%
    ungroup%>%
    pivot_longer(actual_annual_spend:annual_fte, names_to="key", values_to="value")%>%
    pivot_wider(names_from = c(pa_level, key), 
                values_from = value,
                values_fill=0)%>%
    dplyr::rowwise() %>%
    mutate(total_spend=sum(across(matches("annual_spend"), na.rm = T)))%>%
    mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
    mutate( #pivot to get totals and shares
      sd_ct_spend_share=round(`SD-C&T_actual_annual_spend`/total_spend*100) ,
      sd_ct_fte_share=round(`SD-C&T_annual_fte`/total_fte*100),
      nsd_ct_spend_share=round(`NSD-C&T_actual_annual_spend`/total_spend*100) ,
      nsd_ct_fte_share=round(`NSD-C&T_annual_fte`/total_fte*100))%>% 
    
    select(fiscal_year, operatingunit,fundingagency,`SD-C&T_actual_annual_spend`,
           sd_ct_spend_share,`SD-C&T_annual_fte`,sd_ct_fte_share,`NSD-C&T_actual_annual_spend`,
           nsd_ct_spend_share,`NSD-C&T_annual_fte`, nsd_ct_fte_share)
  
  df_msd2<-df_msd%>%
    filter(indicator=="TX_CURR")%>%
    mutate( fundingagency = fct_relevel(fundingagency,"USAID","CDC"))%>%
    group_by(fiscal_year,operatingunit,fundingagency,indicator)%>%
    summarise_at(vars(targets,cumulative), sum, na.rm=TRUE)%>%
    ungroup()%>%
    mutate(achievement=round(cumulative/targets*100))%>%
    select(-c(targets,indicator))
  
  # final MER and HRH join
  df_merhrh<-full_join(df_hrh3,df_msd2)%>%
    mutate( fundingagency = fct_relevel(fundingagency,"USAID","CDC"))%>%
    select(-c(fiscal_year,operatingunit))
  return(df_merhrh)
}

### Mechanism Program Area table
# mechXpa <- gen_budget_exec(df_fsd, group_col = c("mech", "program"))
