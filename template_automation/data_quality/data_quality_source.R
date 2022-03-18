## Source code for functions
## version 2.3
## Based on data_quality_tables_v5-0_hrh
## Added HRH check
## Added code in "abridge" function to remove illegal characters

### Table of Contents
#
# 1) Global Variables - Line 15
# 2) Functions - Line 26
# 3) Error Checking functions (these do the actual data quality check) - Line 48
# 4) Filter functions and lists to iterate through MER/FSD comparison check - Line 174

######## Global Variables
# Columns needed for analysis
# NOTE: "Program Area Modified" is only used during CDV check in Allison's original analysis
fsd_cols <- c('operatingunit', 'primepartner', 'mech_code', 'mech_name',
              'program', 'sub_program', 'beneficiary', 'sub_beneficiary',
              'program_mod', 'intervention', 'interaction_type', 
              'cost_category', 'sub_cost_category',
              'cop_budget_total', 'expenditure_amt')

msd_cols = c("operatingunit", "primepartner", "mech_code", "mech_name", 
             "indicator", 'cumulative')

######## Functions #############################################################
# Abridge a string so that it fits in Excel's sheet name
### Input: strng: String
###        max_len: integer. Maximum length allowed 
### Output: String
abridge <- function(strng, max_len=90){
  # Remove all special characters that crash R Markdown
  ### See https://stackoverflow.com/questions/9934856/removing-non-ascii-characters-from-data-files
  strng <- gsub('[^\x20-\x7E]', "", strng)
  len_str <- nchar(strng)
  
  # Check if string is longer than max allowable lengtrh
  if (len_str > max_len){
    # Takes the first part and last part of a string, and abridges the middle with ellipses
    start <- substr(strng, 1, (max_len-9))
    end <- substr(strng, len_str-6, len_str)
    return(glue("{start}...{end}"))
  } else {
    return(strng)
  }
}

######## Error Checking Functions ###############################################

# Purpose: Check expenditure's percentage of the budget at IM level. Show in output
#          data.frame if this percentage is < 80 percent or > 120 percent
# Input: data.frame. In this case, FSD data at the OU-level
# Output: data.frame. Dataframe of OUs with this error, and their % budget execution
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

# Purpose: Checks if mechanism FAST budget equals expenditure exactly, then flags as error. 
#          Errors are outputted as a dataframe
### Input: data.frame. In this case, the OU-level FSD
### Output: data.frame of mechanisms that have this error 
check_budg_exp <- function(df){
  df_sum_interven <- df %>% 
    group_by(mech_code, mech_name, intervention) %>%
    summarize(sum_fast = sum(cop_budget_total, na.rm=T),
              sum_exp = sum(expenditure_amt, na.rm = T)) %>%
    mutate(diff = sum_exp - sum_fast) %>%
    # Only select mechanisms that have no difference between expenditure and FAST budget
    filter(diff == 0) %>%
    arrange(mech_code, intervention)
}

# Purpose: Checks if mechanisms have more than 10% of total expenditure in the category "Other: Other"
### Input: data.frame. OU-level FSD
### Output: data.frame of OUs that trigger this error check
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

# Purpose: Checks if mechanisms have more than 40% of expenditures reported as program management
### Input: data.frame. OU-level FSD
### Output: data.frame of OUs that trigger this error check
percent_pm_check <- function(df){
  perc_pm <- df %>%
    group_by(mech_code, mech_name) %>%
    mutate(mech_exp_sum = sum(expenditure_amt, na.rm = T)) %>%
    group_by(mech_code, mech_name, program) %>%
    summarize(perc_exp = sum(expenditure_amt, na.rm=T) / first(mech_exp_sum)) %>%
    filter(program == "PM") %>%
    filter(perc_exp >0.4)
}

# Purpose: Check if indicator result and expenditure entries both exist in the MSD 
#          and FSD respectively. Based on a minimum threshold of MER results. 
# Inputs:
#         msd: data.frame. OU-level MSD 
#         fsd: data.frame. OU-level FSD
#         msd_filter_str: string. MSD indicator to select
#         fsd_filter: dplyr::filter() function with preset conditional filters for FSD
#         cond: integer. If sum of MER results is greater than cond, then the MER result "counts"
#               as an entry that should have FSD data
#         fsd_str: string. A human-friendly label for the table for FSD columns
cross_data_check <- function(msd, fsd, msd_filter_str, 
                             cond, fsd_filter, fsd_str){
  sum_indicators <- msd %>%
    # filter by indicator name, msd_filter_str
    filter(indicator == msd_filter_str) %>%
    group_by(indicator, mech_code, mech_name) %>%
    summarize(sum_results = sum(cumulative, na.rm = T)) %>%
    # filter: Only keep MSD indicator if it is greater than cond. Otherwise, results are 
    #         too small and probably don't have expenditures connected to them
    filter(sum_results > cond)
  
  sum_exp <- fsd %>%
    # filter by specific FSD conditions (i.e. specific beneficiary/program combos)
    fsd_filter %>%
    group_by(mech_code, mech_name) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm = T))
  
  # We only do left join because we are asking: "If results exist, do expenditures exist?"
  ### In other words: If results don't exist, we don't care and it shouldn't be in the dataframe, 
  ###                 even if expenditure exists
  indic_exp <- merge(x=sum_indicators, y = sum_exp, by=c("mech_code", "mech_name"),
                     all.x = TRUE)
  
  # Check if expenditures are NA or 0, then munge table a bit for printing later
  cross_data_fail <- indic_exp[is.na(indic_exp$sum_exp) | (indic_exp$sum_exp ==0),] %>%
    filter(sum_results != 0) %>%
    mutate(indicator = msd_filter_str) %>%
    mutate(fsd_cat = fsd_str) %>%
    select(-sum_exp)
  
  return(cross_data_fail)
}

# Purpose: Checks if mechanisms that reported ER expenditures also reported in HRH
### Input: data.frame. OU-level FSD
###        data.frame. OU-level HRH data
### Output: data.frame of mechanisms that had ER expenditure but no HRH data
hrh_check <- function(fsd, hrh){
  # dataframe at mechanism-level aggregation of expenditure
  sum_exp <- fsd %>%
    group_by(mech_code, mech_name) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm = T))
  
  # dataframe at mechanism-level aggregation, of HRH annual_fte
  sum_hrh <- hrh %>% 
    group_by(mech_code, mech_name) %>%
    summarize(sum_fte = sum(annual_fte, na.rm = T))
  
  # We only do left join because we are asking: "If expenditures exist, do HRH data exist?"
  hrh_exp <- merge(x=sum_exp, y = sum_hrh , by=c("mech_code", "mech_name"),
                   all.x = TRUE)
  
  # If HRH's aggregation is NA or 0, we select it in the data.frame as a possible error
  hrh_fail <- hrh_exp[is.na(hrh_exp$sum_fte) | (hrh_exp$sum_fte ==0),] %>%
    filter(sum_exp != 0) %>%
    select(-sum_fte)
  
  return(hrh_fail)
}

# Purpose: Check expenditure's percentage of the budget at program area level
### Input: data.frame. OU-level FSD
### Output: data.frame of mechanisms that have failed the % expenditure check at 
###         the program-area level. Meaning, % budget execution <80% or >120% for a program area
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

########  Set filter functions##################################################
# Purpose: We set up these filter functions ahead of time so that we can place them in 
#          a list, so that we can for-loop/iterate through them for the MER vs. FSD check
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

# These are additional lists for the MSD vs. FSD check, for iterating via for-loops
## List of program areas relevant for the check
fsd_str_lst <- list("C&T", "C&T for PBFW",
                    "PREV: Medication assisted treatment for KP and PWID",
                    "PREV for KP", "OVC for AGYW", "Priority Pops",
                    "PrEP", "VMMC for Males", "HTS")

## List of MER indicators that have FSD intervention counterparts
msd_str_lst <- list("TX_CURR", "PMTCT_ART", "KP_MAT", 
                       "KP_PREV", "OVC_SERV", "PP_PREV", 
                       "PrEP_NEW", "VMMC_CIRC", "HTS_TST")

## List of integers for the cut-off for MER results to be allowed in this MER vs. FSD check
msd_cond <- list(200,50,10,100,50,50,100,100,100)

fsd_filter_lst <- list(filter_tx_fsd, filter_pmtct_fsd, filter_kpmat_fsd,
                       filter_kpprev_fsd, filter_ovc_fsd, filter_pp_fsd,
                       filter_prep_fsd, filter_vmmc_fsd, filter_hts_fsd)