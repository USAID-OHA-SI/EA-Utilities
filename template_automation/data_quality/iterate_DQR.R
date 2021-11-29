# Purpose: Iterates Data Quality Report templates for all OUs
# Author: David Song
# Date: 2021 Nov 8
# Version: 2.0

library(stringr)
library(gophr)
library(glamr)
library(glue)
library(tidyverse)
library(googledrive)

source('data_quality_source.R')
home_dir <- "C:/Users/davidsong/Desktop/USAID"
uploader_path <- "/GitHub/EA-Utilities/upload_dir_to_gdrive.R"
source(glue("{home_dir}{uploader_path}"))


## Global ===============================================
fiscal_yr <- 2021
# path to Google Drive folder
# drive_path <- '1lopD-1ADm2tEiSRb7wY5bl1iStBEz129'
drive_path <- '1z96BoanAi1Q8hCkcZfOCmd14oquk-kpw'

### MAIN ====================================================
# Build necessary directories if they are not present
temp_dir <- "temp"
output_dir <- "output"
log_dir <- "logs"
dir.create(temp_dir, showWarning=F)
dir.create(output_dir, showWarning=F)
dir.create(log_dir, showWarning=F)

########  Load Data ################################################
# Assign pointer to original df that is read in, so that gc() can find and dispose of it
df_msd_full <- si_path() %>%
  return_latest("OU_IM") %>%
  gophr::read_msd()

df_msd <- df_msd_full %>%
  filter(fundingagency == "USAID", 
         fiscal_year == fiscal_yr,
         disaggregate == "Total Numerator") %>%
  select(msd_cols) %>%
  # You MUST copy data.frame, else new pointer just points to the 
  # original   1.2GB data file
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

hrh_path <- "C:/Users/davidsong/Desktop/USAID/Documents/Data/HRH_Structured_Datasets_Site_IM_FY21_20211112.txt"
df_hrh_full <- read.csv(hrh_path, sep="\t", header=T)
names(df_hrh_full)

hrh_cols <- c("operating_unit", 'country', 'funding_agency', 'mech_code', 'mech_name', 
              'fiscal_year', 'annual_fte')
df_hrh <- df_hrh_full %>% 
  select(hrh_cols) %>%
  filter(funding_agency =="USAID",
         fiscal_year == fiscal_yr)

rm(df_hrh_full)
gc()

# Create temporary csv's for knitting
write.csv(df_msd, glue("{temp_dir}/df_msd.csv"), row.names = F)
write.csv(df_fsd, glue("{temp_dir}/df_fsd.csv"),  row.names = F)
write.csv(df_hrh, glue("{temp_dir}/df_hrh.csv"), row.names = F)

# Select OUs
ou <- unique(df_fsd$operatingunit)
ou <- append(ou, "Central Mechanisms")

# Delete from memory
rm(df_msd)
rm(df_fsd)
rm(df_hrh)
gc()

# ######## REMOVE FOR THE REAL DEAL ########################
# # for test, just choose 5 ous
# ou <- ou[1:5]
##########################################################

reports <- tibble(
  filename = str_c("data_quality_", ou, ".pdf"),
  params = map(ou, ~list(ou = .))
)

reports %>%
  select(output_file = filename, params) %>%
  # https://stackoverflow.com/questions/56606299/in-rstudio-knit-always-works-but-rmarkdownrender-fails-on-second-run-bu
  pwalk(rmarkdown::render, 
        input = "data_quality_report_template.Rmd", 
        output_dir = glue("{output_dir}/"),
        intermediates_dir = temp_dir)

### Move log files elsewhere, to avoid cluttering working directory
# selects files in current working directory
curr_files <- list.files()
curr_files <- curr_files[str_detect(curr_files, ".log")]
new_log_loc <- paste0(log_dir,sep="/", curr_files)
file.rename(from = curr_files,
            to = new_log_loc)


##### Upload to Google Drive ##################################
### UNCOMMENT THIS WHEN YOU WANT TO UPLOAD #####################
load_secrets()

upload_dir_to_gdrive(output_dir, drive_path)


