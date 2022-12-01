# Version 2.1
# Source code for ER briefer
# 2.1 update: Removed code that will be placed in the markdown code
# Author: David Song
# Date: 2022/01/04

#### NOTE: All functions are only used in the er_briefer_rmd.Rmd, and never used in iterate_briefer.R
#### Note: I have no idea why the original GT code used strings instead of numbers for fiscal year,
####       but faithfully converted all integers to strings for fiscal_yr
#### Advice: Do not hard code fiscal year into your code

# List of agencies in our desired order with USAID first. Use this for re-leveling dataframes
agency_lvls <- c("USAID", "CDC", "AF", "DoD","EAP","EUR","HRSA","PC","PRM",
                 "SAMHSA","SGAC","WHA","Other")
# Column names used multiple times for budget execution tables
be_order_cols <- c("expenditure_amt_2021", "cop_budget_total_2021", "budget_execution_2021",
                  "expenditure_amt_2022", "cop_budget_total_2022", "budget_execution_2022")

# Purpose: Generates an ordered dataframe for using as indices for grouping rows in Kable
### Input: df: data.frame that we want grouped rows for
###        col: string. Column with categorical variables that we want to group rows by
##              In this case, we want to group funding agencies by row
### Output: 1 row dataframe, with row group names as column headers and integers indicating 
###         how many rows each "group" spans. Example: USAID|2 means USAID spans 2 rows then CDC spans 3 rows after USAID
###                                                    CDC  |3
ordered_table <- function(df, col){
  output <- df %>% 
    # We must convert string into code that dplyr can understand, so we use the syms function
    group_by(!!! rlang::syms(col)) %>% 
    # count number of rows 
    count() %>%
    # Arrange by our preferred order set by agency_lvls, so USAID is ordered first
    arrange(factor(!!! rlang::syms(col), levels= agency_lvls), 
            desc= (!!! rlang::syms(col))) %>%
    # pivot_wider to make it a 1 row dataframe, rather than a 1 column dataframe. Kable reads this better
    pivot_wider(names_from = (!!! rlang::syms(col)), values_from = n)
  return(output)
}

# Purpose: Clean FSD for any tables with budget execution
### Input: data.frame. FSD
### Output: cleaned data.frame
fsd_prep_budget_ex <- function(df){
  df_out <- df %>%   
    #mutate data type double into integer to have round numbers. 
    ### Song Note: This code is identical to GT code, but I would recommend using 
    ###            round(., digits=0) as a better rounding function than as.integer
    ###            because as.integer just cuts off the decimals
    dplyr::mutate_if(is.double, as.integer)%>%
    #drop NA for numeric amounts
    mutate_at(vars(cop_budget_total:expenditure_amt),~replace_na(.,0)) %>%
    #recode values to match naming in Financial Integrated Dataset
    dplyr::mutate(`interaction_type`= recode (`interaction_type`, 
                                              "Service Delivery"= "SD",
                                              "Non Service Delivery"= "NSD")) %>%
    dplyr::mutate(`interaction_type`  = dplyr::case_when(program == "PM" ~"PM",
                                                         TRUE ~`interaction_type`)) %>%
    dplyr::mutate( mech = paste(mech_code,"-", mech_name)) %>%
    
    # Produce alternative agency category that combines non-CDC/non-USAID as Other
    agency_category() %>%
    
    #mutating & calculating budget execution
    group_by(operatingunit, country, funding_agency, agency_category, fiscal_year,
             prime_partner_name,mech_name, mech_code,mech, program, interaction_type) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()
  return(df_out)
}


# Purpose: Clean FSD for any tables with budget execution
### Input: data.frame. FSD
### Output: cleaned data.frame
gen_fsd_kp <- function(df){
  df_out <- df %>%   
    #mutate data type double into integer to have round numbers. 
    ### Song Note: This code is identical to GT code, but I would recommend using 
    ###            round(., digits=0) as a better rounding function than as.integer
    ###            because as.integer just cuts off the decimals
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
    filter(beneficiary=="Key Pops")%>%
    
    #mutating & calculating budget execution
    group_by(operatingunit, country, funding_agency, agency_category, fiscal_year,
             prime_partner_name,mech_name, mech_code, program, interaction_type) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()
  return(df_out)
}

# Purpose: calculate budget execution at the OU, program, mech, or program x mech levels
### Inputs: df: data.frame that only has one OU
###         group_col: String or list of string. 
###                    Select NA, "program", "mech", or c("mech, "program")
### Output: cleaned data.frame with calculated budget execution at desired level of granularity
gen_budget_exec <- function(df, group_col = NA, fiscal_yr=fiscal_yr){
  # Vector of strings that are column names for the select function
  group_cols <- c("funding_agency", "fiscal_year", "cop_budget_total", 
                  "expenditure_amt")
  
  # If group_col is not NA, then append the string(s) to the group_cols list
  if (!is.na(group_col)){
    group_cols <- append(group_cols, group_col, 1)
  }
  
  # Selects columns for the group_by function
  ### Note: head function drops last two elements, "cop_budget" and "expenditure" from vector
  group_by_cols <- head(group_cols, -2)
  
  # If grouping by OU x Program Area, use modified agency category variable
  condition <- group_col == "program"
  # We have double if-statement to handle if NA
  if (!is.na(condition)){
    if (condition){
      df <- df %>%
        dplyr::select(-funding_agency) %>%
        # replace funding_agency with agency_category
        rename(funding_agency = agency_category)
    }
  }
  
  df_out <-df %>%
   dplyr::filter((fiscal_year %in% periods))%>%
     # dplyr::filter(fiscal_year== (fiscal_yr_prev) | fiscal_year== (fiscal_yr))%>%
    dplyr::select(group_cols) %>%
    
    #### changed from group_by_ to group_by_at
    group_by_at(group_by_cols)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup() %>%
    
    # Pivot table to correct shape to match GT table
    pivot_wider(names_from = fiscal_year,
                values_from = cop_budget_total:budget_execution, 
                values_fill = 0)%>%
    dplyr::select(c(head(group_by_cols,1), be_order_cols)) %>%
    # Change order so USAID is first
    arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))
  return(df_out)
}

progs<-c("HTS", "C&T","OVC", "PrEP")
# Purpose: Clean FSD for merging with target data from MSD. Subset HTS, C&T, and OVC
#          since those are the only programs with clear MER counterparts
### Input: FSD data.frame
### Output: cleaned data.frame
gen_fsd_tgt <- function(df){
  df_out <- df %>% 
    filter(fiscal_year==as.character(fiscal_yr))%>%
    dplyr::mutate(program = dplyr::case_when(beneficiary == "OVC"~"OVC", 
                                             TRUE ~program))%>%
    dplyr::mutate(program = dplyr::case_when(grepl("PrEP", sub_program) ~ "PrEP", 
                                             
                                             TRUE ~program))%>%
    # dplyr::mutate(program = dplyr::case_when(beneficiary == "Key Pops"~"KP", 
    #                                          TRUE ~program))%>%
    group_by(operatingunit,funding_agency,fiscal_year, mech_code, mech_name, 
             prime_partner_name, program) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()%>%
    # Only select HTS, C&T, OVC, PREP programs
    filter(program %in% progs)%>%
    mutate(budget_execution=round(expenditure_amt/cop_budget_total*100)) %>%
    # Order USAID first
    arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))
  return(df_out)
}
#################################################################################

### Unit Expenditure #################################### Does not use prep_fsd
### NOTE: code is identical to GT table code, except front parts are cut off since 
###       iterate_briefer.R already pre-processed the FSD and MSD for merger
gen_ue <- function(fsd_tgt, msd_tgt){
  #join datasets together 
  df_ue<-left_join(fsd_tgt, msd_tgt) %>%
    group_by(operatingunit,funding_agency,fiscal_year, mech_code, mech_name, prime_partner_name,indicator)%>%
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
    filter(fiscal_year==as.character(fiscal_yr))%>%
    
    select(operatingunit,funding_agency,mech_code, mech_name, prime_partner_name,program, 
           indicator, unit_expenditure, cumulative)%>%
    pivot_wider(names_from =indicator, values_from=cumulative:unit_expenditure) %>%
    
    select(operatingunit,funding_agency,mech_code, mech_name, prime_partner_name,
           cumulative_HTS_TST,cumulative_HTS_TST_POS,
           cumulative_TX_CURR,cumulative_TX_NEW,
           unit_expenditure_HTS_TST,unit_expenditure_HTS_TST_POS,
           unit_expenditure_TX_CURR,unit_expenditure_TX_NEW) %>%
    
    group_by(operatingunit,funding_agency,mech_code, mech_name, prime_partner_name,)%>%
    summarise_at(vars(cumulative_HTS_TST: unit_expenditure_TX_NEW), 
                 sum, na.rm = TRUE) %>%
    
    mutate(prime_partner_name=dplyr::case_when(prime_partner_name=="FHI Development 360 LLC"~"FHI360",
                                         prime_partner_name=="Family Health International"~"FHI360",
                                         prime_partner_name=="Abt Associates, Inc."~"Abt Associates Inc",
                                         TRUE ~prime_partner_name))%>%
    
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
    select(funding_agency,mech:cumulative_TX_NEW) %>%
    arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))
  
  return(df_ue)
}



### Local Partner % #################################### Does not use prep_fsd
### Note: Code has been optimized from its GT table form to run cleaner and with fewer repetition

# Purpose: Generate data.frame from FSD that feeds into both the table and the graph
### Input: FSD data.frame
### Output: cleaned data.frame
gen_local <- function(df){
  df %>%
    dplyr::mutate(`mech_code`=as.character(`mech_code`))%>%
    apply_partner_type() %>%
    remove_sch("SGAC")%>%
    dplyr::filter(funding_agency=="USAID")%>%
    filter(!fiscal_year=="2018")
}

# Purpose: Generate clean table of local mechanisms data
### Input: data.frame cleaned by gen_local function
### Output: data.frame table ready for Kable formatting
gen_local_table <- function(df){
  df %>% dplyr::filter(fiscal_year== as.character(fiscal_yr), 
                       partner_type_usaid_adjusted=="Local")%>%
    dplyr::select (c(mech,program,fiscal_year,cop_budget_total,expenditure_amt))%>%
    group_by(mech,program,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    
    dplyr::mutate(budget_execution=round(expenditure_amt/cop_budget_total*100))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,
                values_from = cop_budget_total:budget_execution, 
                values_fill = 0)%>%
    
    dplyr::relocate(expenditure_amt_2022, .before = cop_budget_total_2022) %>%
    dplyr::relocate(budget_execution_2022, .after = cop_budget_total_2022)%>%
    filter(cop_budget_total_2022>0 | expenditure_amt_2022>0)
}

# Purpose: Generate dataframe that ggplot of local vs. international requires
### Input: data.frame cleaned by gen_local function
### Output: data.frame for graphing
gen_graph_tbl <- function(df) {
  df %>% dplyr::filter(partner_type_usaid_adjusted=="Local" | partner_type_usaid_adjusted=="International",
                       !fiscal_year==as.character(fiscal_yr+1))%>%
    dplyr::select (c(partner_type_usaid_adjusted, fiscal_year,expenditure_amt))%>%
    group_by(partner_type_usaid_adjusted,fiscal_year)%>%
    summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
    pivot_wider(names_from = partner_type_usaid_adjusted, 
                values_from = expenditure_amt) %>%
    
    # must ignore NA to sum correctly
    mutate(total = rowSums(across(c(Local, International)), na.rm=TRUE), #pivot to get totals and shares
           lp_share = Local / total*100,
           ip_share=International/total*100) %>%
    # Song Note: There are probably better ways to caclulate totals and shares than double pivots
    pivot_longer(cols = International:Local, names_to = "partner_type")
}

# Purpose: Plot the ggplot of local vs. international
### Input: data.frame from gen_graph_tbl
### Output: Nothing. Produces ggplot
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
    # # si_style does NOT work in Rmarkdown, so I commented it out
    # si_style_nolines()+
    ### Recreation of si_style_nolines function
    theme(legend.position="bottom",
          # closest font that I could get from Windows to R Markdown font
          text=element_text(family="Source Sans Pro"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
          )
}

### Breakdown by Service Delivery ####################################
### NOTE: code is identical to GT table code, except front parts are cut off since 
###       iterate_briefer.R already pre-processed the FSD
gen_serv <- function(df, ou){
  df %>% 
    remove_sch("SGAC")%>%
    filter(funding_agency=="USAID"|funding_agency=="CDC")%>%
    filter(!fiscal_year=="2023")%>%
    filter(!fiscal_year=="2019")%>%
    filter(!fiscal_year=="2018")%>%
    dplyr::select (c(funding_agency,interaction_type, fiscal_year,expenditure_amt))%>%
    mutate( funding_agency = fct_relevel(funding_agency, "USAID","CDC"))%>%
    group_by(funding_agency, interaction_type, fiscal_year)%>% 
    summarise(across(expenditure_amt, sum, na.rm = TRUE)) %>% 
    ungroup() %>%
    
    pivot_wider(names_from = interaction_type, values_from = expenditure_amt)%>%
    
    mutate(total = NSD+SD+PM, #pivot to get totals and shares
           nsd_share=NSD/total ,
           sd_share = SD / total ,
           pm_share=PM/total)%>%
    pivot_longer(cols = NSD:SD, names_to = "interaction_type")%>%
    group_by(fiscal_year,funding_agency,nsd_share, pm_share,sd_share,interaction_type,total)%>%
    summarise(value)%>%
    mutate(cumulative=cumsum(value))%>%
    ungroup%>%
    mutate(share=case_when(interaction_type=="NSD"~ nsd_share,
                           interaction_type=="SD"~sd_share,
                           interaction_type=="PM"~pm_share,
                           TRUE ~as.numeric(interaction_type)))%>%
    select(funding_agency,fiscal_year,interaction_type,cumulative,total,share) %>%
    
    # Note: I combined plotting into the same function, unlike the GT code, since I will
    #       never use this dataframe again besides for generating this plot
    ggplot(aes(fiscal_year,cumulative)) + #can add value to geom_col-use cumulative sum
    geom_col(aes(y = cumulative, fill = fct_rev(interaction_type))) +
    facet_wrap("funding_agency", scales='free_x')+
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
    # I had to recreate si_style_nolines manually with a font that tries to match R Markdown's
    theme(legend.position="bottom",
          text=element_text(family="Source Sans Pro"),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()
    )
}
    

### HIV Testing  ####################################
# Purpose: Generates the single dataframe required for any of the 3 FSD program vs. MER tables
#          Used by HTS, OVC, and C&T
### Inputs: FSD and MSD dataframes
### Output: cleaned dataframe that can be filtered for HTS, C&T, or OVC or PREP tables
gen_ue_indiv <- function(fsd, msd){
  # Additional transformations on MSD before merge are required
  msd_tgt_indiv <- msd %>%
    mutate(achievement=round (cumulative/targets*100))%>%
    select(operatingunit,funding_agency,fiscal_year,mech_code,mech_name,program,
           indicator,achievement)%>%
    pivot_wider(names_from = indicator, values_from=achievement)

  # Must check if OVC_SERV is included in the OU. 
  ### Why: Some OUs do not have OVC_SERV. Since we create our table via pivot,
  ###      if we are missing OVC_SERV, we get the wrong number of columns and it 
  ###      messes up the table
  ### Warning: If any OU ever lacks C&T or HTS in the future (highly unlikely, but possible),
  ###          then we have to repeat this code but for C&T or HTS indicators (e.g. TX_CURR)
  if(!"OVC_SERV" %in% names(msd_tgt_indiv)){
    msd_tgt_indiv["OVC_SERV"] = 0
  }

  if(!"PrEP_NEW" %in% names(msd_tgt_indiv)){
    msd_tgt_indiv["PrEP_NEW"] = 0
  }
  
  # Join datasets together 
  df_ue_indiv<-left_join(fsd, msd_tgt_indiv)%>%
    dplyr::mutate( mech = paste(mech_code,"-", mech_name)) %>%
    relocate(expenditure_amt, .before= cop_budget_total)%>%
    relocate(mech, .before= expenditure_amt)%>%
    select(operatingunit,funding_agency,program,mech,
           expenditure_amt, cop_budget_total, budget_execution,
           HTS_TST, HTS_TST_POS, TX_CURR, TX_NEW, OVC_SERV, PrEP_NEW) %>%
    arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))%>%
  
  return(df_ue_indiv)
}

# Use below function to get HTS, C&T, OVC, Prep info for a specific OU
### Input: df_ue_temp: data.frame created by gen_ue_indiv function
###        programs: string or list of strings. Pick only one: "HTS", "C&T", or "OVC" 
get_program_specific<-function(df_ue_temp, programs=c("HTS","C&T","OVC", "PrEP")){
  df<-df_ue_temp%>%
    mutate_at(vars(expenditure_amt:PrEP_NEW),~replace_na(.,0)) %>%
    filter(program %in% programs)%>%
     select_if(~!( all(. == 0)))%>%
    # special for FY22 Q4 removing all ER and budget with no funding
   # filter((expenditure_amt>0 | cop_budget_total> 0))%>%
    select(-any_of(c("operatingunit")))
  return(df)
}

### KP beneficiary by program area
fsd_prep_budget_ex_kp <- function(df){
  df_out_kp <- df_out %>%   
    #mutate data type double into integer to have round numbers. 
    ### Song Note: This code is identical to GT code, but I would recommend using 
    ###            round(., digits=0) as a better rounding function than as.integer
    ###            because as.integer just cuts off the decimals
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
    filter(beneficiary=="Key Pops")%>%
    
    #mutating & calculating budget execution
    group_by(operatingunit, country, funding_agency, agency_category, fiscal_year,
             prime_partner_name,mech_name, mech_code, program, interaction_type) %>% 
    summarise_at(vars(cop_budget_total, expenditure_amt), sum, na.rm = TRUE) %>% 
    ungroup()
  return(df_out_kp)
}



### Commodities #################################### Does not use prep_fsd
### NOTE: code is identical to GT table code, except front parts are cut off since 
###       iterate_briefer.R already pre-processed the FSD
psm_murder<-function(df){
  df_out<-df%>%
    # Slight additional processing of FSD
    filter(fiscal_year==as.character(fiscal_yr)) %>%
    filter(str_detect(mech_name, 'GHSC'))%>%
    
    # Rest of the GT code
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


### HRH ======================================================================
### NOTE: code is identical to GT table code, except front parts are cut off since 
###       iterate_briefer.R already pre-processed the FSD
##### RECOMMENDATION: I had the least time streamlining the HRH code, so there is 
#####                 highly room to further improve it from its original GT version

## First table
gen_hrh_main <- function(df_hrh, df_fsd){
  df_hrh1<-df_hrh%>%
    group_by(operatingunit,funding_agency,fiscal_year,)%>%
    summarise_at(vars(annual_fte,individual_count,actual_annual_spend), 
                 sum, na.rm = TRUE)%>%
    ungroup()
  
  df_fsd_temp <- df_fsd %>% 
    filter(fiscal_year == as.character(fiscal_yr)) %>%
    group_by(operatingunit,funding_agency,fiscal_year)%>%
    summarise_at(vars(expenditure_amt), sum, na.rm = TRUE)%>%
    ungroup()
    
  
  hrh_fsd1<-left_join(df_hrh1,df_fsd_temp)%>%
    select(-c(fiscal_year,operatingunit))%>%
    adorn_totals("row",,,, -funding_agency)%>%
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
  df_hrh_mod<-df_hrh%>%
  filter(fiscal_year %in% fiscal_yr)%>%
    dplyr::mutate(interaction_type=dplyr::case_when(interaction_type=="Service Delivery"~"SD",
                                                    interaction_type=="Non Service Delivery"~"NSD",
                                                    TRUE ~interaction_type))%>%
    mutate(er_staff=glue("{er_category}-{interaction_type}"))%>%
    group_by(operatingunit,funding_agency,fiscal_year,er_staff,)%>%
    summarise_at(vars(actual_annual_spend,annual_fte), sum, na.rm = TRUE)%>%
    ungroup%>%
    pivot_longer(actual_annual_spend:annual_fte, names_to="key", values_to="value")%>%
    pivot_wider(names_from = c(er_staff, key), 
                values_from = value,
                values_fill=0)
  
  
  # https://stackoverflow.com/questions/45857787/adding-column-if-it-does-not-exist
  full_staff_cols <- c("HCW: Ancillary-Direct Service Delivery_actual_annual_spend",
                       "HCW: Ancillary-Direct Service Delivery_annual_fte",
                       "Implementing Mechanism Program Management Staff-Non-Service Delivery_actual_annual_spend",
                       "Implementing Mechanism Program Management Staff-Non-Service Delivery_annual_fte",
                       "Other Staff-Non-Service Delivery_annual_fte",
                       "Other Staff-Non-Service Delivery_actual_annual_spend",
                       "HCW: Clinical-Direct Service Delivery_actual_annual_spend",
                       "HCW: Clinical-Direct Service Delivery_annual_fte")
  
  df_hrh_mod <- fncols(df_hrh_mod, full_staff_cols)
  
  df_hrh_mod %>%
    # Correctly order columns
    select(c("funding_agency", full_staff_cols)) %>%
    #select(-c(fiscal_year,operatingunit))%>%
    adorn_totals("row",,,, -funding_agency,)%>%
    dplyr::rowwise() %>%
    mutate(total_spend=sum(across(matches("actual_annual_spend"), na.rm = T)))%>%
    mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
    mutate( #pivot to get totals and shares
      other_staff_spend_share=round(`Other Staff-Non-Service Delivery_actual_annual_spend`/total_spend*100) ,
      other_staff_fte_share=round(`Other Staff-Non-Service Delivery_annual_fte`/total_fte*100) ,
      pm_spend_share=round(`Implementing Mechanism Program Management Staff-Non-Service Delivery_actual_annual_spend`/total_spend*100),
      pm_fte_share=round(`Implementing Mechanism Program Management Staff-Non-Service Delivery_annual_fte` /total_fte*100) ,
      hcw_clinial_spend_share=round(`HCW: Clinical-Direct Service Delivery_actual_annual_spend`/total_spend*100) ,
      hcw_clinical_fte_share=round(`HCW: Clinical-Direct Service Delivery_annual_fte`/total_fte*100) ,
      hcw_ancillary_spend_share=round(`HCW: Ancillary-Direct Service Delivery_actual_annual_spend`/total_spend*100) ,
      hcw_ancillary_fte_share=round(`HCW: Ancillary-Direct Service Delivery_annual_fte`/total_fte*100))%>% 
    select(-c(total_spend,total_fte,))%>%
    
    dplyr::relocate(pm_spend_share, .after = `Implementing Mechanism Program Management Staff-Non-Service Delivery_actual_annual_spend`)%>%
    dplyr::relocate(pm_fte_share, .after = `Implementing Mechanism Program Management Staff-Non-Service Delivery_annual_fte`)%>%
    dplyr::relocate(other_staff_spend_share, .after = `Other Staff-Non-Service Delivery_actual_annual_spend`)%>%
    dplyr::relocate(other_staff_fte_share, .after = `Other Staff-Non-Service Delivery_annual_fte`)%>%
    dplyr::relocate(hcw_clinial_spend_share, .after = `HCW: Clinical-Direct Service Delivery_actual_annual_spend`)%>%
    dplyr::relocate(hcw_clinical_fte_share, .after = `HCW: Clinical-Direct Service Delivery_annual_fte`)%>%
    dplyr::relocate(hcw_ancillary_spend_share, .after = `HCW: Ancillary-Direct Service Delivery_actual_annual_spend`)%>%
    dplyr::relocate(hcw_ancillary_fte_share, .after = `HCW: Ancillary-Direct Service Delivery_annual_fte`)
  
} 


gen_com <- function(df_hrh){
  df_com<-df_hrh%>%
    filter(fiscal_year%in% fiscal_yr)%>%
    select(operatingunit,funding_agency,is_community_primarily,individual_count,annual_fte,actual_annual_spend)%>%
    mutate(actual_annual_spend=as.numeric(actual_annual_spend))%>%
    mutate(annual_fte=as.numeric(annual_fte))%>%
    group_by(operatingunit,funding_agency,is_community_primarily)%>%
    summarise_at(vars(annual_fte,actual_annual_spend), sum, na.rm=TRUE)%>%
    ungroup()%>%
    pivot_longer(c(annual_fte,actual_annual_spend), names_to="key", values_to="value")%>%
    pivot_wider(names_from = c(is_community_primarily, key), 
                values_from = value,
                values_fill=0
    )%>%
    
    # filter(fiscal_year=="2021",
    # operatingunit %in% ou)%>%
     # select(-c(fiscal_year,))%>%
    select(-c(operatingunit))%>%
    adorn_totals("row",,,, -funding_agency,)%>%
    dplyr::rowwise() %>%
    # mutate(total_count=sum(across(matches("individual_count"), na.rm = T)))%>%
    mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
    mutate(total_spend=sum(across(matches("actual_annual_spend"), na.rm = T)))%>%
    mutate( #pivot to get totals and shares
      # comm_staff_count_share=round(`Community Based_individual_count`/total_count*100) ,
      comm_staff_fte_share=round(Yes_annual_fte/total_fte*100) ,
      # noncomm_spend_share=round(`Non-Community Based_individual_count`/total_count*100),
      noncomm_fte_share=round(No_annual_fte /total_fte*100) ,
      noncomm_spend_share=round(No_actual_annual_spend/ total_spend*100),
      com_spend_share=round(Yes_actual_annual_spend/ total_spend*100))%>%
     dplyr::relocate(total_fte, .after=funding_agency)%>%
     dplyr::relocate(total_spend, .after=noncomm_fte_share)%>%
    dplyr::relocate(comm_staff_fte_share, .after=Yes_annual_fte)%>%
    dplyr::relocate(noncomm_fte_share , .after=No_annual_fte)%>%
    dplyr::relocate(No_actual_annual_spend , .after=total_spend)%>%
    dplyr::relocate(noncomm_spend_share, .after=No_actual_annual_spend)%>%
    dplyr::relocate(Yes_actual_annual_spend, .after=noncomm_spend_share)%>%
    dplyr::relocate(com_spend_share , .after=Yes_actual_annual_spend)%>%
     select(-c(total_spend,total_fte,))
  return(df_com)
} 




merge_hrh_mer <- function(df_hrh){
  df_hrh_out<-df_hrh%>%
    filter(fiscal_year ==fiscal_yr)%>%
    dplyr::mutate(interaction_type= dplyr::case_when(interaction_type == "Service Delivery"~"SD",
                                                     interaction_type == "Non Service Delivery"~"NSD",
                                                     TRUE ~interaction_type))%>%
    mutate(pa_level=glue("{interaction_type}-{program}"))%>%
    group_by(funding_agency,operatingunit,fiscal_year,pa_level)%>%
    summarise_at(vars(actual_annual_spend,annual_fte,), sum, na.rm = TRUE)%>%
    ungroup%>%
    pivot_longer(actual_annual_spend:annual_fte, names_to="key", values_to="value")%>%
    pivot_wider(names_from = c(pa_level, key), 
                values_from = value,
                values_fill=0)
}

gen_hrh_mer <- function(df_hrh_mer_merged, df_msd){
  pivot_cols <- c("Direct Service Delivery-C&T_actual_annual_spend", "Direct Service Delivery-C&T_annual_fte", 
                  "Non-Service Delivery-C&T_actual_annual_spend", "Non-Service Delivery-C&T_annual_fte")
  
  df_hrh_mer_fixed <-fncols(df_hrh_mer_merged, pivot_cols)
  
  df_hrh3 <- df_hrh_mer_fixed%>%
    dplyr::rowwise() %>%
    mutate(total_spend=sum(across(matches("actual_annual_spend"), na.rm = T)))%>%
    mutate(total_fte=sum(across(matches("annual_fte"), na.rm = T)))%>%
    mutate( #pivot to get totals and shares
      sd_ct_spend_share=round(`Direct Service Delivery-C&T_actual_annual_spend`/total_spend*100) ,
      sd_ct_fte_share=round(`Direct Service Delivery-C&T_annual_fte`/total_fte*100),
      nsd_ct_spend_share=round(`Non-Service Delivery-C&T_actual_annual_spend`/total_spend*100) ,
      nsd_ct_fte_share=round(`Non-Service Delivery-C&T_annual_fte`/total_fte*100))%>% 
    
    
    select(fiscal_year, operatingunit,funding_agency,`Direct Service Delivery-C&T_actual_annual_spend`,
           sd_ct_spend_share,`Direct Service Delivery-C&T_annual_fte`,sd_ct_fte_share,`Non-Service Delivery-C&T_actual_annual_spend`,
           nsd_ct_spend_share,`Non-Service Delivery-C&T_annual_fte`, nsd_ct_fte_share)
  
  df_msd2<-df_msd%>%
    filter(indicator=="TX_CURR")%>%
    filter(fiscal_year ==fiscal_year)%>%
    # mutate( funding_agency = fct_relevel(funding_agency,"USAID","CDC"))%>%
    group_by(fiscal_year,operatingunit,funding_agency,indicator)%>%
    summarise_at(vars(targets,cumulative), sum, na.rm=TRUE)%>%
    ungroup()%>%
    mutate(achievement=round(cumulative/targets*100))%>%
    select(-c(targets,indicator)) %>%
    arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))
  
  
  # final MER and HRH join
  df_merhrh<-full_join(df_hrh3,df_msd2)%>%
    filter(fiscal_year == as.character(fiscal_yr))%>%
    # mutate( funding_agency = fct_relevel(funding_agency,"USAID","CDC"))%>%
    arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))%>%
    select(-c(fiscal_year,operatingunit))
  return(df_merhrh)
}

gen_budget_exec_bk <- function(df, group=c("funding_agency","program", "mech", "mech-program"), fiscal_yr=fiscal_yr){
  
  if(group=="funding_agency"){
    group_cols <- c("funding_agency", "fiscal_year", "cop_budget_total", 
                    "expenditure_amt")
    group_by_cols <- head(group_cols, -2)
    df_out <-df %>%
      dplyr::filter((fiscal_year %in% periods))%>%
      # dplyr::filter(fiscal_year== (fiscal_yr_prev) | fiscal_year== (fiscal_yr))%>%
      dplyr::select(group_cols) %>%
      
      #### changed from group_by_ to group_by_at
      group_by_at(group_by_cols)%>%
      summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
      dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
      ungroup() %>%
      
      # Pivot table to correct shape to match GT table
      pivot_wider(names_from = fiscal_year,
                  values_from = cop_budget_total:budget_execution, 
                  values_fill = 0)%>%
      dplyr::select(c(head(group_by_cols,1), be_order_cols)) %>%
      # Change order so USAID is first
      arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))
    return(df_out)
    
  }
  
  if(group=="program"){
    group_cols <- c("funding_agency","agency_category","program", "fiscal_year", "cop_budget_total", 
                    "expenditure_amt")
    group_by_cols <- head(group_cols, -2)
    df_out <-df %>%
      dplyr::filter((fiscal_year %in% periods))%>%
      # dplyr::filter(fiscal_year== (fiscal_yr_prev) | fiscal_year== (fiscal_yr))%>%
      dplyr::select(group_cols) %>%
      
      #### changed from group_by_ to group_by_at
      group_by_at(group_by_cols)%>%
      summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
      dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
      ungroup() %>%
      
      # Pivot table to correct shape to match GT table
      pivot_wider(names_from = fiscal_year,
                  values_from = cop_budget_total:budget_execution, 
                  values_fill = 0)%>%
      dplyr::select(c(head(group_by_cols,3), be_order_cols)) %>%
      # Change order so USAID is first
      arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))%>%
      select(-c(funding_agency))
    return(df_out)
    
  }
  if(group=="mech"){
    group_cols <- c("funding_agency","mech", "fiscal_year", "cop_budget_total", 
                    "expenditure_amt")
    group_by_cols <- head(group_cols, -2)
    df_out <-df %>%
      dplyr::filter((fiscal_year %in% periods))%>%
      # dplyr::filter(fiscal_year== (fiscal_yr_prev) | fiscal_year== (fiscal_yr))%>%
      dplyr::select(group_cols) %>%
      
      #### changed from group_by_ to group_by_at
      group_by_at(group_by_cols)%>%
      summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
      dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
      ungroup() %>%
      
      # Pivot table to correct shape to match GT table
      pivot_wider(names_from = fiscal_year,
                  values_from = cop_budget_total:budget_execution, 
                  values_fill = 0)%>%
      dplyr::select(c(head(group_by_cols,2), be_order_cols)) %>%
      # Change order so USAID is first
      arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))
    return(df_out)
    
  }  
  
  if(group=="mech-program"){
    group_cols <- c("funding_agency","mech","program", "fiscal_year", "cop_budget_total", 
                    "expenditure_amt")
    group_by_cols <- head(group_cols, -2)
    df_out <-df %>%
      dplyr::filter((fiscal_year %in% periods))%>%
      # dplyr::filter(fiscal_year== (fiscal_yr_prev) | fiscal_year== (fiscal_yr))%>%
      dplyr::select(group_cols) %>%
      
      #### changed from group_by_ to group_by_at
      group_by_at(group_by_cols)%>%
      summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
      dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
      ungroup() %>%
      
      # Pivot table to correct shape to match GT table
      pivot_wider(names_from = fiscal_year,
                  values_from = cop_budget_total:budget_execution, 
                  values_fill = 0)%>%
      dplyr::select(c(head(group_by_cols,3), be_order_cols)) %>%
      # Change order so USAID is first
      arrange(factor(funding_agency, levels= agency_lvls), desc(funding_agency))
    return(df_out)
    
  }
  
  return(df_out)
}


