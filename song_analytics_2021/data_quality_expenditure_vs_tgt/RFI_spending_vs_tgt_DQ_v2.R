### If statement checks to see if openxlsx is installed and has the correct version
packageurl <- "https://cran.r-project.org/src/contrib/Archive/openxlsx/openxlsx_4.2.3.tar.gz"
if(!require(openxlsx)){install.packages('openxlsx')#packageurl, repos=NULL, type="source")
} else if (packageVersion('openxlsx') != "4.2.3"){
  detach("package:openxlsx", unload=TRUE)
  install.packages(packageurl, repos=NULL, type="source")}

library(openxlsx)
library(gophr)
library(glamr)
library(glue)
library(tidyverse)

#################################################################################
# Global Variables
fiscal_yr <- 2021

fsd_cols <- c('fundingagency', 'operatingunit', 'countryname',
              'primepartner', 'mech_code', 'mech_name',
              'program', 'sub_program', 'beneficiary', 'sub_beneficiary',
              'program_mod', 'intervention', 'interaction_type', 
              'cost_category', 'sub_cost_category',
              'cop_budget_total', 'expenditure_amt')

msd_cols = c("fundingagency", "operatingunit", "countryname", "primepartner", 
             "mech_code", "mech_name", "indicator", 'cumulative')

fsd_str_lst <- list("C&T", "C&T for PBFW",
                    "PREV: Medication assisted treatment for KP and PWID",
                    "PREV for KP", "OVC or AGYW", "Priority Pops",
                    "PrEP", "VMMC for Males", "HTS")

msd_str_lst <- list("TX_CURR", "PMTCT_ART", "KP_MAT", 
                    "KP_PREV", "OVC_SERV", "PP_PREV", 
                    "PrEP_NEW", "VMMC_CIRC", "HTS_TST")

msd_cond <- list(200,50,10,100,50,50,100,100,100)

# levels for ordering the fundingagency column in a dataframe
agency_lvls <- c("USAID", "CDC", "HHS/CDC", "State/AF", "DoD","EAP","EUR","HHS/HRSA",
                 "PC","State/PRM", "SAMHSA","SGAC","WHA","Other")
###########################################################################
##### Function
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

# Helper function: Counting and pivoting wider
# Input: dataframe generated from cross_data_check function
# Output: dataframe of results for count of IMs that failed threshold check
count_pivot <- function(df){
  df %>% 
    summarize(Total_IMs = n()) %>%
    pivot_wider(names_from = Indicator, values_from=Total_IMs) %>%
    ungroup() %>%
    mutate(Total = rowSums(across(where(is.numeric)), na.rm=TRUE))
}

########  Set filter functions##################################################
# Filters are fed into the cross_data_check function via list
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

# Set list of filters for mapping
fsd_filter_lst <- list(filter_tx_fsd, filter_pmtct_fsd, filter_kpmat_fsd,
                       filter_kpprev_fsd, filter_ovc_fsd, filter_pp_fsd,
                       filter_prep_fsd, filter_vmmc_fsd, filter_hts_fsd)
##########################################################################
#### LOAD DATA
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
df_cdv <- 
  pmap(list(msd_str_lst, msd_cond, fsd_filter_lst, fsd_str_lst), 
       ~cross_data_check(msd_filter = ..1, cond = ..2, fsd_filter = ..3, 
                         fsd_str = ..4)) %>%
  bind_rows() %>%
  arrange(indicator) %>%
  mutate(fundingagency=case_when(fundingagency=="USAID"~fundingagency,
                                 fundingagency=="HHS/CDC"~"CDC",
                                 TRUE ~ "Other")) %>%
  arrange(fundingagency, operatingunit, countryname, mech_code) %>%
  relocate(countryname) %>%
  relocate(operatingunit) %>%
  relocate(fundingagency) %>%
  rename(Agency = fundingagency,
         OU = operatingunit,
         Country = countryname,
         Indicator = indicator,
         `FSD_category` = fsd_cat
         )

###################################################################
#### Counting IMs that failed threshold

# PEPFAR_wide
results_pepfar <- df_cdv %>%
  group_by(Indicator, FSD_category) %>%
  summarize(Total_IMs=n()) %>%
  ungroup()
  
# By country
results_country <- df_cdv %>%
  group_by(Country, Indicator) %>%
  count_pivot()

# By agency
results_agency <- df_cdv %>%
  group_by(Agency, Indicator) %>%
  count_pivot() %>%
  arrange(factor(Agency, levels= agency_lvls), desc(Agency))

# By country x agency
results_ag_country <- df_cdv %>%
  group_by(Country, Agency, Indicator) %>%
  count_pivot() %>%
  arrange(Country, factor(Agency, levels= agency_lvls), desc(Agency)) 

# USAID by country
usaid_ag_country <- results_ag_country %>% filter(Agency=="USAID")


#########################################################
### Save results in Excel workbook
# https://ycphs.github.io/openxlsx/articles/Introduction.html
wb <- openxlsx::loadWorkbook("dataquality_im_counts_reference.xlsx")
writeData(wb, sheet = "PEPFAR-wide", x = results_pepfar, 
          colNames=T, withFilter=T)

writeData(wb, sheet = "By country", x= results_country, colNames=T, withFilter=T)

writeData(wb, sheet="By agency", x = results_agency, colNames=T, withFilter=T)

writeData(wb, sheet = "By country x agency", x = results_ag_country,
          colNames=T, withFilter=T)

writeData(wb, sheet = "By USAID x country", x = usaid_ag_country, 
          colNames=T, withFilter=T)

writeData(wb, sheet="All IMs", x= df_cdv, colNames=T, withFilter=T)

saveWorkbook(wb, "dataquality_im_counts.xlsx", overwrite = TRUE)

