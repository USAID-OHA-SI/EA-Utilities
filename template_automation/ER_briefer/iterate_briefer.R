# Version 1.0
# Iterator for ER Briefer
# Author: David Song
# Date: 2022/01/04

### Libraries ===============================================
library(scales)
library(glitr)
library(glamr)
library(gophr)

library(janitor)
library(glue)
library(tidyverse)
library(googledrive)

library(formattable)
library(kableExtra)
library(pander)
library(extrafont)
extrafont::font_import(prompt=F, pattern="GOUDOS.TTF")
extrafont::loadfonts(device = "win")

### Set Paths to download correct sources ====================================
# Path where you want to save your PDF outputs, log files, and temp files
working_dir <- "C:/Users/davidsong/Desktop/USAID/TEST"

# Path where GitHub folder can be found
git_dir <- "C:/Users/davidsong/Desktop/USAID"

# Set Google Drive directory (destination)
drive_path <- "1669ALUvY1jNS9c0Shm6_YXA9sTnb7cVJ"

# Set local directory names for Outputs and Log files
output_dir <- "ER briefers"
log_dir <- "logs"

# Manually set list of OUs to subset (list of names or of numeric index positions)
# Note: Only use if you want to subset; otherwise leave it an empty vector
ou_choice <- c()  # ex. c(1:5) or c("Angola", "Asia Region", "Cameroon")

### Global =====================================================================
fsd_cols <- c("record_type", "operatingunit", "countryname", "fundingagency", "fiscal_year",
              "primepartner", "mech_code", "mech_name", "program", "sub_program", 
              "interaction_type", "beneficiary","cop_budget_total", "expenditure_amt")
msd_cols <- c("operatingunit", "countryname", "fundingagency", "fiscal_year",
              "mech_code", "mech_name", "primepartner", "indicator", 
              "standardizeddisaggregate", "cumulative", "targets")
indics<-c("HTS_TST","HTS_TST_POS", "TX_CURR", "TX_NEW","OVC_SERV")

### Functions ==============================================
# universal prep_fsd
fsd_selector <- function(df, cols) {
  df_out <- df %>% 
    dplyr::select(cols) %>%
    glamr::remove_mo() %>%
    glamr::clean_agency()%>%
    # Changes USAID/WCF fundingagency categories to "USAID" 
    dplyr::mutate(fundingagency=dplyr::case_when(fundingagency== "WCF"~"USAID", 
                                                 TRUE ~fundingagency))%>%
    ##concatenate mech id and mech name
    dplyr::mutate( mech = paste(mech_code,"-", mech_name))
  return(df_out)
}

# MSD for use for UE and individual target data tables
gen_msd_tgt <- function(df){
  df_out <- df %>%
    filter(fiscal_year=="2021")%>%
    dplyr::select(msd_cols) %>%
    filter(standardizeddisaggregate=="Total Numerator")%>%
    filter(indicator %in% indics)%>%
    clean_agency()%>%
    # Changes USAID/WCF fundingagency categories to "USAID" 
    dplyr::mutate(fundingagency=dplyr::case_when(fundingagency== "WCF"~"USAID", 
                                                 TRUE ~fundingagency))%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    group_by(operatingunit,fundingagency,fiscal_year, mech_code, mech_name, 
             primepartner,indicator) %>% 
    summarise_at(vars(cumulative,targets), sum, na.rm = TRUE) %>% 
    ungroup()%>%
    dplyr::mutate(program = dplyr::case_when(indicator    == "TX_CURR"    ~"C&T", 
                                             indicator    == "TX_NEW"    ~"C&T",
                                             indicator =="HTS_TST" ~"HTS",
                                             indicator == "HTS_TST_POS" ~"HTS",
                                             indicator    == "OVC_SERV"    ~"OVC",
                                             
                                             TRUE ~indicator))%>%
    filter(!fundingagency=="DEDUP") 
  # %>% dplyr::filter(targets>0)
  return(df_out)
}


gen_hrh <- function(df){
  df_out<-df %>%
    filter(fiscal_year=="2021") %>%
    rename(operatingunit=operating_unit,
           fundingagency=funding_agency_fing)%>%
    clean_agency()%>%
    # Changes USAID/WCF fundingagency categories to "USAID" 
    dplyr::mutate(fundingagency=dplyr::case_when(fundingagency== "WCF"~"USAID", 
                                                 TRUE ~fundingagency))%>%
    mutate( fundingagency = fct_relevel(fundingagency,"USAID","CDC"))%>%
    mutate(annual_fte=as.numeric(annual_fte),
           individual_count=as.numeric(individual_count),
           annual_expenditure=as.numeric(annual_expenditure),
           actual_annual_spend=as.numeric(actual_annual_spend),
           annual_fringe=as.numeric(annual_fringe),)%>%
    group_by(operatingunit,fundingagency,fiscal_year, mech_code, 
             mech_name,interaction_type, program ,er_category)%>%
    summarise_at(vars(annual_fte,individual_count,annual_expenditure,
                      actual_annual_spend, annual_fringe), sum, na.rm = TRUE)%>%
    ungroup()
  return(df_out)
}

### Souce in some additional functions
# Source in gdrive uploader function and utilities functions
source(glue("{git_dir}/GitHub/EA-Utilities/upload_dir_to_gdrive.R"))
source(glue("{git_dir}/GitHub/stacks-of-hondos/scripts/utilities.R"))

# Path of the iterate_briefer.R file
src_path <- dirname(rstudioapi::getSourceEditorContext()$path)
source(glue("{src_path}/er_briefer_source.R"))

### MAIN ====================================================
# Add path to where folders should be placed
output_dir <- glue("{working_dir}/{output_dir}")
log_dir <- glue("{working_dir}/{log_dir}")
temp_dir <- glue("{working_dir}/temp")

# Build necessary directories if they are not present
dir.create(output_dir, showWarning=F)
dir.create(log_dir, showWarning=F)
dir.create(temp_dir, showWarning=F)

### Load data ==============================
# Assign pointer to original df that is read in, so that gc() can find and dispose of it
df_fsd_full <- si_path() %>% return_latest("Fin") %>% gophr::read_msd()
df_fsd_temp <- fsd_selector(df_fsd_full, fsd_cols) %>%
  # You MUST copy data.frame, else new pointer just points to the original 
  data.table::copy()

df_msd_full <- si_path() %>% return_latest("OU_IM") %>% gophr::read_msd()
msd_tgt_temp<- gen_msd_tgt(df_msd_full) %>%
  data.table::copy()

df_hrh_full <- si_path()%>% return_latest("HRH")%>% gophr::read_msd()
df_hrh_temp <- gen_hrh(df_hrh_full) %>%
  data.table::copy()

# Select OUs
ou <- unique(df_fsd_temp$operatingunit)
# ou <- append(ou, "Central Mechanisms")

# Subsets OUs if user chooses to subset OUs
if(!is.null(ou_choice)){
  if(is.character(ou_choice)){ou <- ou_choice}
  else{ou <- ou[ou_choice]}
}

# Remove full files and garbage collect to empty R memory
rm(df_fsd_full)
rm(df_msd_full)
rm(df_hrh_full)
gc()

# # Create temporary csv's for knitting
# write.csv(df_fsd, glue("{temp_dir}/df_fsd.csv"),  row.names = F)
# write.csv(msd_tgt, glue("{temp_dir}/msd_tgt.csv"), row.names = F)
# write.csv(df_hrh, glue("{temp_dir}/df_hrh.csv"), row.names = F)

df_fsd_full <- df_fsd_temp %>% data.table::copy()
df_msd_full <- msd_tgt_temp %>% data.table::copy()
df_hrh_full <- df_hrh_temp %>% data.table::copy()

##########################################################
### Iterate ============================================
reports <- tibble(
  filename = str_c("COP22 Briefer_", ou, ".pdf"), #Ben fixed output name
  params = map(ou, ~list(ou = .))
)

reports %>%
  select(output_file = filename, params) %>%
  # https://stackoverflow.com/questions/56606299/in-rstudio-knit-always-works-but-rmarkdownrender-fails-on-second-run-bu
  pwalk(rmarkdown::render, 
        input = glue("{src_path}/er_briefer_rmd.Rmd"), 
        output_dir = glue("{output_dir}/"),
        intermediates_dir = temp_dir)

### Move log files elsewhere, to avoid cluttering working directory
# selects files in current working directory
curr_files <- list.files(src_path)
curr_files <- curr_files[str_detect(curr_files, ".log")]
new_log_loc <- paste0(log_dir,sep="/", curr_files)
file.rename(from = curr_files,
            to = new_log_loc)


# ##### Upload to Google Drive ##################################
### UNCOMMENT THIS WHEN YOU WANT TO UPLOAD #####################
load_secrets()

upload_dir_to_gdrive(output_dir, drive_path)
