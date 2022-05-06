library(gophr)
library(glamr)
library(glue)
library(tidyverse)

# Select filter function and check if indicator result and expenditure entries
# both exist in the MSD and FSD respectively
# Inputs:
#         msd_filter: dyplr::filter() function with preset filters for MSD
#         fsd_filter: filter() function with preset filters for FSD
cross_data_check <- function(msd_filter_str, cond, fsd_filter, fsd_str, 
                             msd = df_msd, fsd=df_fsd){
  sum_indicators <- msd %>%
    filter(indicator == msd_filter_str) %>%
    group_by(fundingagency, operatingunit, countryname, indicator, mech_code, 
             mech_name) %>%
    summarize(sum_results = sum(cumulative, na.rm = T)) %>%
    filter(sum_results > cond) %>%
    ungroup()
  
  sum_exp <- fsd %>%
    fsd_filter %>%
    group_by(fundingagency, operatingunit, countryname, mech_code, mech_name) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm = T)) %>%
    ungroup()
  
  # We only do left join because we are asking: "If results exist, do expenditures exist?"
  indic_exp <- merge(x=sum_indicators, y = sum_exp, by=c("operatingunit", "mech_code", "mech_name"),
                     all.x = TRUE)
  
  cross_data_fail <- indic_exp[is.na(indic_exp$sum_exp) | (indic_exp$sum_exp ==0),] %>%
    filter(sum_results != 0) %>%
    mutate(indicator = msd_filter_str) %>%
    mutate(fsd_cat = fsd_str) %>%
    select(-c("sum_exp", "fundingagency.y", "countryname.y")) %>%
    rename(fundingagency=fundingagency.x,
           countryname=countryname.x)
  return(cross_data_fail)
}

#################################################################################

fiscal_yr <- 2021

fsd_cols <- c('fundingagency', 'operatingunit', 'countryname',
              'primepartner', 'mech_code', 'mech_name',
              'program', 'sub_program', 'beneficiary', 'sub_beneficiary',
              'program_mod', 'intervention', 'interaction_type', 
              'cost_category', 'sub_cost_category',
              'cop_budget_total', 'expenditure_amt')

msd_cols = c("fundingagency", "operatingunit", "countryname", "primepartner", 
             "mech_code", "mech_name", "indicator", 'cumulative')

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


##########################################################################

df_msd_full <- si_path() %>%
  return_latest("OU_IM") %>%
  gophr::read_msd()

df_msd <- df_msd_full %>%
  filter(fiscal_year == fiscal_yr,
         disaggregate == "Total Numerator") %>%
  select(msd_cols) %>%
  data.table::copy()
rm(df_msd_full)
gc()


df_fsd_full <- si_path() %>%
  return_latest("Fin") %>% 
  gophr::read_msd() 

df_fsd <- df_fsd_full %>% 
  filter(fiscal_year == fiscal_yr) %>%
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
#################################################################
# Data Quality checks for all Targets, at OU-level

df_tx <- cross_data_check(msd_filter="TX_CURR", cond=200, fsd_filter=filter_tx_fsd, 
                          fsd_str="C&T")

df_pmtct <- cross_data_check(msd_filter = "PMTCT_ART",cond = 50, 
                             fsd_filter = filter_pmtct_fsd, fsd_str = "C&T and PBFW")

df_kp_mat <- cross_data_check("KP_MAT", 10, filter_kpmat_fsd, 
                              "PREV: Medication assisted treatment for KP and PWID")

df_kp_prev <- cross_data_check("KP_PREV", 100, filter_kpprev_fsd, "PREV for KP")

df_ovc <- cross_data_check("OVC_SERV", 50, filter_ovc_fsd, "OVC for AGYW")

df_pp <- cross_data_check("PP_PREV", 50, filter_pp_fsd, "Priority Pops")

df_prep <- cross_data_check("PrEP_NEW", 100, filter_prep_fsd, "PrEP")

df_vmmc <- cross_data_check("VMMC_CIRC", 100, filter_vmmc_fsd, "VMMC for Males")

df_hts <- cross_data_check("HTS_TST", 100, filter_hts_fsd, "HTS")

###################################################################
# PEPFAR_wide


# By country

# By agency

# By country, only USAID