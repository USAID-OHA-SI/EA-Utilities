## Source code for functions
## version 2.0
## Based on data_quality_tables_v4-0_kable

######## Global Variables
# Columns needed for analysis
# NOTE: "Program Area Modified" is only used during CDV check in Allison's original
fsd_cols <- c('operatingunit', 'primepartner', 'mech_code', 'mech_name',
              'program', 'sub_program', 'beneficiary', 'sub_beneficiary',
              'program_mod', 'intervention', 'interaction_type', 
              'cost_category', 'sub_cost_category',
              'cop_budget_total', 'expenditure_amt')

msd_cols = c("operatingunit", "primepartner", "mech_code", "mech_name", 
             "indicator", 'cumulative')

######## Functions #############################################################
# Checks if budget == exp exactly and flags as error if so, print in df
### NOTE: do NOT use summary_rows func in GT to render this; summary rows are
###       already included, labelled " TOTAL intervention" under "intervention".
###       Use conditional formatting to format these TOTAL rows
check_budg_exp <- function(df){
  df_sum_interven <- df %>% 
    group_by(mech_code, mech_name, intervention) %>%
    summarize(sum_fast = sum(cop_budget_total, na.rm=T),
              sum_exp = sum(expenditure_amt, na.rm = T)) %>%
    mutate(diff = sum_exp - sum_fast)
  
  df_sum_mech <- df %>% 
    group_by(mech_code, mech_name) %>%
    summarize(sum_fast = sum(cop_budget_total, na.rm=T),
              sum_exp = sum(expenditure_amt, na.rm = T)) %>%
    mutate(diff = sum_exp - sum_fast)
  
  exp_budget_mech_check <- df_sum_mech %>%
    filter(diff == 0 ) %>%
    mutate(intervention = " TOTAL interventions")
  
  exp_budget_interv_check <- df_sum_interven %>%
    filter(diff == 0)
  
  results_exp_budget <- rbind(exp_budget_interv_check, exp_budget_mech_check) %>%
    arrange(mech_code, intervention)
}

percent_other_check <- function(df){
  perc_other <- df %>%
    group_by(operatingunit, mech_code, mech_name) %>%
    mutate(mech_exp_sum = sum(expenditure_amt, na.rm = T)) %>%
    group_by(operatingunit, mech_code, mech_name, cost_category, sub_cost_category) %>%
    summarize(perc_exp = sum(expenditure_amt, na.rm=T) / first(mech_exp_sum)) %>%
    filter(cost_category == "Other",
           sub_cost_category == "Other",
           perc_exp > 0.1)
}

percent_pm_check <- function(df){
  perc_pm <- df %>%
    group_by(mech_code, mech_name) %>%
    mutate(mech_exp_sum = sum(expenditure_amt, na.rm = T)) %>%
    group_by(mech_code, mech_name, program) %>%
    summarize(perc_exp = sum(expenditure_amt, na.rm=T) / first(mech_exp_sum)) %>%
    filter(program == "PM") %>%
    filter(perc_exp >0.4)
}

# check expenditure's percentage of the budget at program area level
exp_perc_budg_check_pa <- function(df){
  exp_perc_budg <- df %>%
    group_by(mech_code, mech_name, program) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm=T),
              sum_fast = sum(cop_budget_total, na.rm=T)) %>%
    #    mutate(perc_exp =  sum_fast/sum_exp ) %>%
    
    mutate(perc_exp =  sum_exp/sum_fast ) %>%
    mutate(perc_exp = na_if(perc_exp, Inf)) %>%
    filter(perc_exp < 0.8 | perc_exp >1.2 | is.na(perc_exp)) %>%
    arrange(mech_code, program)
}

# check expenditure's percentage of the budget at IM level
# Input: df_fsd
exp_perc_budg_check_im <- function(df){
  exp_perc_mech_level <- df %>%
    group_by(mech_code, mech_name) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm=T),
              sum_fast = sum(cop_budget_total, na.rm=T)) %>%
    mutate(perc_exp = sum_exp/sum_fast) %>%
    mutate(perc_exp = na_if(perc_exp, Inf)) %>%
    filter(perc_exp < 0.8 | perc_exp >1.2 | is.na(perc_exp)) %>%
    arrange(mech_code)
}

# Select filter function and check if indicator result and expenditure entries
# both exist in the MSD and FSD respectively
# Inputs:
#         msd_filter: dyplr::filter() function with preset filters for MSD
#         fsd_filter: filter() function with preset filters for FSD
cross_data_check <- function(msd, fsd, msd_filter_str, 
                             cond, fsd_filter, fsd_str){
  sum_indicators <- msd %>%
    filter(indicator == msd_filter_str) %>%
    group_by(indicator, mech_code, mech_name) %>%
    summarize(sum_results = sum(cumulative, na.rm = T)) %>%
    filter(sum_results > cond)
  
  sum_exp <- fsd %>%
    fsd_filter %>%
    group_by(mech_code, mech_name) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm = T))
  
  # We only do left join because we are asking: "If results exist, do expenditures exist?"
  indic_exp <- merge(x=sum_indicators, y = sum_exp, by=c("mech_code", "mech_name"),
                     all.x = TRUE)
  
  cross_data_fail <- indic_exp[is.na(indic_exp$sum_exp) | (indic_exp$sum_exp ==0),] %>%
    filter(sum_results != 0) %>%
    mutate(indicator = msd_filter_str) %>%
    mutate(fsd_cat = fsd_str) %>%
    select(-sum_exp)
  
  return(cross_data_fail)
}

########  Set filter functions##################################################
filter_tx_fsd <- function(x){filter(x, program == "C&T")}

filter_pmtct_fsd <- function(x){filter(x,program == "C&T" &
                                         beneficiary == 'Pregnant & Breastfeeding Women')}

filter_kpmat_fsd <- function(x){filter(x,program == "PREV" &
                                         sub_program == "Medication assisted treatment" &
                                         beneficiary == 'Key Pops' &
                                         sub_beneficiary == 'People who inject drugs')}

filter_kpprev_fsd <- function(x){filter(x,program == "PREV" &
                                          beneficiary == 'Key Pops')}

filter_ovc_fsd <- function(x){filter(x, beneficiary == 'OVC' |
                                       sub_beneficiary == "Young women & adolescent females")}

filter_pp_fsd <- function(x){filter(x, beneficiary == 'Priority Pops')}

filter_prep_fsd <- function(x){filter(x, program_mod == 'PrEP')}

filter_vmmc_fsd <- function(x){filter(x, program_mod == 'VMMC' &
                                        beneficiary == 'Males')}

filter_hts_fsd <- function(x){filter(x, program_mod == 'HTS')}

fsd_str_lst <- list("C&T", "C&T for PBFW",
                    "PREV: Medication assisted treatment for KP and PWID",
                    "PREV for KP", "OVC for AGYW", "Priority Pops",
                    "PrEP", "VMMC for Males", "HTS")

msd_str_lst <- list("TX_CURR", "PMTCT_ART", "KP_MAT", 
                       "KP_PREV", "OVC_SERV", "PP_PREV", 
                       "PrEP_NEW", "VMMC_CIRC", "HTS_TST")

msd_cond <- list(200,50,10,100,50,50,100,100,100)

fsd_filter_lst <- list(filter_tx_fsd, filter_pmtct_fsd, filter_kpmat_fsd,
                       filter_kpprev_fsd, filter_ovc_fsd, filter_pp_fsd,
                       filter_prep_fsd, filter_vmmc_fsd, filter_hts_fsd)