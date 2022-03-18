library(gophr)
library(glamr)
library(glue)
library(tidyverse)

# Select filter function and check if indicator result and expenditure entries
# both exist in the MSD and FSD respectively
# Inputs:
#         msd_filter: dyplr::filter() function with preset filters for MSD
#         fsd_filter: filter() function with preset filters for FSD
cross_data_check <- function(msd, fsd, msd_filter_str, 
                             cond, fsd_filter, fsd_str){
  sum_indicators <- msd %>%
    filter(indicator == msd_filter_str) %>%
    group_by(operatingunit, indicator, mech_code, mech_name) %>%
    summarize(sum_results = sum(cumulative, na.rm = T)) %>%
    filter(sum_results > cond)
  
  sum_exp <- fsd %>%
    fsd_filter %>%
    group_by(operatingunit, mech_code, mech_name) %>%
    summarize(sum_exp = sum(expenditure_amt, na.rm = T))
  
  # We only do left join because we are asking: "If results exist, do expenditures exist?"
  indic_exp <- merge(x=sum_indicators, y = sum_exp, by=c("operatingunit", "mech_code", "mech_name"),
                     all.x = TRUE)
  
  cross_data_fail <- indic_exp[is.na(indic_exp$sum_exp) | (indic_exp$sum_exp ==0),] %>%
    filter(sum_results != 0) %>%
    mutate(indicator = msd_filter_str) %>%
    mutate(fsd_cat = fsd_str) %>%
    select(-sum_exp)
  
  return(cross_data_fail)
}

#################################################################################

fiscal_yr <- 2021

fsd_cols <- c('operatingunit', 'primepartner', 'mech_code', 'mech_name',
              'program', 'sub_program', 'beneficiary', 'sub_beneficiary',
              'program_mod', 'intervention', 'interaction_type', 
              'cost_category', 'sub_cost_category',
              'cop_budget_total', 'expenditure_amt')

msd_cols = c("operatingunit", "primepartner", "mech_code", "mech_name", 
             "indicator", 'cumulative')

########  Set filter functions##################################################
filter_pmtct_fsd <- function(x){filter(x,program == "C&T" &
                                         beneficiary == 'Pregnant & Breastfeeding Women')}

# filter_pmtct_fsd <- function(x){filter(x,beneficiary == 'Pregnant & Breastfeeding Women')}

fsd_str_lst <- list("C&T", "C&T for PBFW",
                    "PREV: Medication assisted treatment for KP and PWID",
                    "PREV for KP", "OVC for AGYW", "Priority Pops",
                    "PrEP", "VMMC for Males", "HTS")

msd_str_lst <- list("TX_CURR", "PMTCT_ART", "KP_MAT", 
                    "KP_PREV", "OVC_SERV", "PP_PREV", 
                    "PrEP_NEW", "VMMC_CIRC", "HTS_TST")

msd_cond <- list(200,50,10,100,50,50,100,100,100)



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

df_cdv <- cross_data_check(df_msd, df_fsd, msd_filter = "PMTCT_ART",
                           cond = 50, fsd_filter = filter_pmtct_fsd, fsd_str = "C&T and PBFW")

write.csv(df_cdv, "pmtct_art_no_er_lst.csv")

# df_cdv <- 
#   pmap(list(msd_str_lst, msd_cond, fsd_filter_lst, fsd_str_lst), 
#        ~cross_data_check(df_msd, df_fsd, msd_filter = ..1, 
#                          cond = ..2, fsd_filter = ..3, fsd_str = ..4)) %>%
#   bind_rows() %>%
#   group_by(indicator)