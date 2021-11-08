## version 3.3: add further filters to cdv, to ensure minimum results

# LOCALS & SETUP ===============================================================
## IMPORTANT NOTE: set working directory to Source File for best results
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#devtools::install_github("haozhu233/kableExtra")
#install.packages('formattable')
library(stringr)

# Libraries
library(kableExtra)
library(glamr)
library(gophr)
library(glue)
library(tidyverse)
library(formattable)

######## Global Variables ######################################################
fiscal_yr <- 2020

######## Functions #############################################################
# Checks if budget == exp exactly and flags as error if so
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
    mutate(intervention = "TOTAL interventions")
  
  exp_budget_interv_check <- df_sum_interven %>%
    filter(diff == 0)
  
  results_exp_budget <- rbind(exp_budget_interv_check, exp_budget_mech_check) %>%
    arrange(mech_code, desc(intervention))
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
    filter(program == "PM")
}

# check expenditure's percentage of the budget 
### NOTE: do NOT use summary_rows func in GT to render this; summary rows are
###       already included, labelled "TOTAL" under "program".
###       Use conditional formatting to format these TOTAL rows
exp_perc_budg_check <- function(df){
  exp_perc_budg <- df %>%
    group_by(mech_code, mech_name, program) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm=T),
              sum_fast = sum(cop_budget_total, na.rm=T)) %>%
    mutate(perc_exp =  sum_exp/sum_fast ) %>%
    mutate(perc_exp = na_if(perc_exp, Inf))
  
  exp_perc_mech_level <- exp_perc_budg %>%
    group_by(mech_code, mech_name) %>%
    summarize(sum_exp = sum(sum_exp),
              sum_fast = sum(sum_fast)) %>%
    mutate(perc_exp = sum_exp/sum_fast) %>%
    mutate(perc_exp = na_if(perc_exp, Inf)) %>%
    mutate(program = "TOTAL")
  
  results_exp_perc <- rbind(exp_perc_budg, exp_perc_mech_level) %>%
    arrange(mech_code, desc(program))
}

# Select filter function and check if indicator result and expenditure entries
# both exist in the MSD and FSD respectively
# Inputs:
#         msd_filter: dyplr::filter() function with preset filters for MSD
#         fsd_filter: filter() function with preset filters for FSD
cross_data_check <- function(msd, fsd, msd_filter_str, 
                             cond, fsd_filter){
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
    mutate(indicator = msd_filter_str)
  
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


msd_str_lst <- list("TX_CURR", "PMTCT_ART", "KP_MAT", 
                       "KP_PREV", "OVC_SERV", "PP_PREV", 
                       "PrEP_NEW", "VMMC_CIRC", "HTS_TST")

msd_cond <- list(200,50,10,100,50,50,100,100,100)

fsd_filter_lst <- list(filter_tx_fsd, filter_pmtct_fsd, filter_kpmat_fsd,
                       filter_kpprev_fsd, filter_ovc_fsd, filter_pp_fsd,
                       filter_prep_fsd, filter_vmmc_fsd, filter_hts_fsd)

########  Load Data ############################################################

# Columns needed for analysis
# NOTE: "Program Area Modified" is only used during CDV check in Allison's original
fsd_cols <- c('operatingunit', 'primepartner', 'mech_code', 'mech_name',
          'program', 'sub_program', 'beneficiary', 'sub_beneficiary',
          'program_mod', 'intervention', 'interaction_type', 
          'cost_category', 'sub_cost_category',
          'cop_budget_total', 'expenditure_amt')

msd_cols = c("operatingunit", "primepartner", "mech_code", "mech_name", 
             "indicator", 'cumulative')

# Assign pointer to original df that is read in, so that gc() can find and dispose of it
df_msd_full <- si_path() %>%
  return_latest("OU_IM") %>%
  gophr::read_msd()

df_msd <- df_msd_full %>%
  filter(fundingagency == "USAID", 
         fiscal_year == fiscal_yr,
         disaggregate == "Total Numerator") %>%
  select(msd_cols) %>%
  # You MUST copy data.frame, else new pointer just points to the original 1.2GB data file
  data.table::copy()
rm(df_msd_full)
gc()

df_fsd_full <- si_path() %>%
  return_latest("Fin") %>% 
  gophr::read_msd() 

df_fsd <- df_fsd_full %>% 
  filter(fundingagency == "USAID",
         fiscal_year == fiscal_yr) %>%
  remove_mo() %>%
  mutate(program_mod = dplyr::case_when(beneficiary == 'OVC' ~'OVC',
                                               sub_program == 'VMMC' ~'VMMC',
                                               sub_program == 'PrEP' ~'PrEP',
                                               TRUE ~ program)) %>%
  mutate(intervention =  glue("{.$program}: {.$beneficiary}")) %>%
  dplyr::select(fsd_cols) %>%
  data.table::copy()
rm(df_fsd_full)
gc()


# TEST =========================================================================
test_ou <- "Uganda" # for perc_pm_test
test_ou <- "Kenya" # for OVC_SERV
test_ou <- "Nigeria" # for PrEP_NEW
test_ou <- "Tanzania" # for PMTCT_ART, KP_PREV, PP_PREV, VMMC_CIRC and HTS_TST

fsd_test <- df_fsd %>% 
  filter(operatingunit == test_ou)

msd_test <- df_msd %>% filter(operatingunit == test_ou)

# produce original dataframes=================================================
df_budg_exp <- fsd_test %>%
  check_budg_exp() 

df_other <- df_fsd %>%
  percent_other_check()

df_pm <- percent_pm_check(fsd_test)

df_perc_exp <- exp_perc_budg_check(fsd_test) 

# df_cdv <- 
#   map2(.x = msd_str_lst, .y = fsd_filter_lst, 
#        ~cross_data_check(msd_test, fsd_test, msd_filter = .x, fsd_filter = .y)) %>%
#   bind_rows() %>%
#   group_by(indicator)


df_cdv <- 
  pmap(list(msd_str_lst, msd_cond, fsd_filter_lst), 
       ~cross_data_check(msd_test, fsd_test, msd_filter = ..1, 
                         cond = ..2, fsd_filter = ..3)) %>%
  bind_rows() %>%
  group_by(indicator)

# Kable Tests ==================================================================
col_names <- c("Mechanism code", "Mechanism Name", "Intervention",
               "Total Planned Budget", "Total Expenditure", "Difference")
df_budg_exp %>% ungroup() %>%
  mutate(sum_fast = currency(sum_fast)) %>%
  mutate(sum_exp = currency(sum_exp)) %>%
  mutate(intervention = cell_spec(intervention, 
                                  bold=df_budg_exp$intervention == "TOTAL interventions")) %>%
  mutate(sum_fast = cell_spec(sum_fast, 
                                  bold=df_budg_exp$intervention == "TOTAL interventions")) %>%
  mutate(sum_exp = cell_spec(sum_exp, 
                                  bold=df_budg_exp$intervention == "TOTAL interventions")) %>%
  kbl(col.names = col_names, escape=FALSE) %>%
  kable_paper() %>% 
  collapse_rows (columns = 1:2, valign="top")

col_other <- c("Operating Unit", "Mechanism code", "Mechanism Name",
               "% of Expenditure")
df_other %>% 
  ungroup() %>%
  select(-cost_category, -sub_cost_category) %>%
  mutate(perc_exp = percent(perc_exp)) %>%
  kbl(col.names = col_other) %>%
  kable_paper() %>% 
  collapse_rows (columns = 1:2, valign="top")

col_pm <- c("Mechanism Code", "Mechanism Name", "% of Expenditure")
df_pm %>%
  ungroup() %>%
  select(-program) %>%
  mutate(perc_exp = percent(perc_exp)) %>%
  # must change col to characters, to change NaN into hyphen
  mutate(perc_exp = as.character(perc_exp)) %>%
  mutate(perc_exp = case_when(perc_exp == 'NaN'~'-',
                              TRUE ~perc_exp)) %>%
  kbl(col.names = col_pm) %>%
  kable_paper() %>%
  collapse_rows(columns = 1:2, valign="top")


condition_func <- function(x){
  if(is.na(x)){
    return("white")
  }else if(x >= 0.9 & x <=1.1){
    return("lime")
  } else {
    return('white') 
  }
}
col_perc_exp <- c("Mechanism Code", "Mechanism Name", "Program Area",
                  "Total Expenditure", "Total Budgeted", "% of Budget Spent")

class(ungroup(df_perc_exp))

styled_null_str <-"<span style=\"     border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: white !important;\" > NA</span>"
df_perc_exp %>% ungroup() %>%
  mutate(sum_fast = currency(sum_fast)) %>%
  mutate(sum_exp = currency(sum_exp)) %>%  
  mutate(perc_exp = percent(perc_exp)) %>%
  # https://community.rstudio.com/t/conditional-formatting-with-column-spec-within-a-dplyr-chain/84347/2
  # mutate(perc_exp = cell_spec(perc_exp, 
  #                             background=sapply(perc_exp, 
  #                                               condition_func))) %>%
  # mutate(perc_exp = as.character(perc_exp)) %>%
  # mutate(perc_exp = case_when(perc_exp == styled_null_str~'-',
                              # TRUE ~perc_exp)) %>%
  kbl(col.names = col_perc_exp, booktabs = T,
      longtable = T, escape=FALSE) %>%

  # https://stackoverflow.com/questions/44486493/produce-a-table-spanning-multiple-pages-using-kable
  kable_paper(latex_options = c("hold_position", "repeat_header")) %>%
  row_spec(which(df_perc_exp$program == "TOTAL"), bold = T) %>%
  column_spec(6, background = sapply(df_perc_exp$perc_exp, condition_func)) %>%
  collapse_rows(columns = 1:2, valign="top")
  


col_cdv <- c("Mechanism Code", "Mechanism Name", " Indicator", 
             "Total Expenditure", "Results")
df_cdv %>%
  ungroup() %>%
  mutate(sum_exp = currency(sum_exp)) %>%  
  kbl(col.names = col_cdv, booktabs = T,
      longtable = T, escape=FALSE) %>%
  kable_paper(latex_options = c("hold_position", "repeat_header")) %>%
  # https://stackoverflow.com/questions/55353452/how-to-group-rows-in-kableextra-with-a-grouping-variable-pack-rows
  pack_rows(index = setNames(rle(df_cdv$indicator)[[1]], 
                             rle(df_cdv$indicator)[[2]]),
            label_row_css = "background-color: #666; color: #fff;") %>%
  remove_column(columns = 3)

df_cdv %>%
  filter(str_detect(mech_name, "/"))
df_perc_exp %>%
  filter(str_detect(mech_name, '/'))




