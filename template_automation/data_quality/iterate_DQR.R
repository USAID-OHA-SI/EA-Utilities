# Purpose: Iterates Data Quality Report templates for all OUs
# Author: David Song
# Date: 2022 March 15
# Version: 3.0

# Make sure the following files are in the same folder as this code:
###      - data_quality_report_template.Rmd
###      - data_quality_source.R
###      - Horizontal_RGB_294.png (this is the USAID logo)

# See Urban Institute's tutorial and GitHub to better understand structure:
# https://urban-institute.medium.com/iterated-fact-sheets-with-r-markdown-d685eb4eafce

library(stringr)
library(gophr)
library(glamr)
library(glue)
library(tidyverse)
library(googledrive)
library(pander)
library(kableExtra)

## Global ===============================================
# Fiscal year for FSD, MSD, and HRH
fiscal_yr <- 2021
# path to Google Drive folder
drive_path <- '1z96BoanAi1Q8hCkcZfOCmd14oquk-kpw'

# Path where you want to save your PDF outputs, log files, and temp files
save_dir <- "C:/Users/davidsong/Desktop/USAID/TEST2"

# Path where GitHub folder can be found
git_dir <- "C:/Users/davidsong/Desktop/USAID"

# Where HRH data is stored
hrh_path <- "C:/Users/davidsong/Desktop/USAID/Documents/Data/HRH_Structured_Datasets_Site_IM_FY21_not_redacted_20211217_v2_1.txt"

# Name of Central Mechanisms csv, with the list of all mechanisms that are "central"
central_mechs_path <- "2021 Central Mechs.csv"

# Name for temporary, output, and log files. Do NOT use full path names. 
output_dir <- "output"
temp_dir <- "temp"
log_dir <- "logs"

# Manually set list of OUs to subset (list of names or of numeric index positions)
### Note: Only use if you want to subset; otherwise leave it an empty vector
ou_choice <- c()  # ex. c(1:5) or c("Angola", "Asia Region", "Cameroon")

### PATHS AND DIRECTORIES ====================================================
# Load the custom Google uploader function from GitHub location
uploader_path <- "/GitHub/EA-Utilities/upload_dir_to_gdrive.R"
source(glue("{git_dir}{uploader_path}"))

# Path where source code for Data Quality Report exists.
### Note: Assumes source code is in same folder as the iterate_DQR.R code
src_path <- dirname(rstudioapi::getSourceEditorContext()$path)
# Load Data Quality Report functions
source(glue('{src_path}/data_quality_source.R'))

# Add path to where folders should be placed
output_dir <- glue("{save_dir}/{output_dir}")
log_dir <- glue("{save_dir}/{log_dir}")
temp_dir <- glue("{save_dir}/temp")

# Build necessary directories if they are not present
dir.create(temp_dir, showWarning=F)
dir.create(output_dir, showWarning=F)
dir.create(log_dir, showWarning=F)

########  Load Data ################################################
# Read in list of all central mechanisms (assumes it's in same folder as code)
df_centrals <- read_csv(glue('{src_path}/{central_mechs_path}'))

# Read in MSD and munge
## Assign pointer to original df that is read in, so that gc() can find and dispose of it
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

# Read in FSD and munge
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

# Read in HRH and munge
df_hrh_full <- read.csv(hrh_path, sep="\t", header=T)
names(df_hrh_full)

hrh_cols <- c("operating_unit", 'country', 'funding_agency', 'mech_code', 'mech_name', 
              'fiscal_year', 'annual_fte')

df_hrh <- df_hrh_full %>% 
  rename(funding_agency = funding_agency_fing)%>%
  select(hrh_cols) %>%
  # Must convert WCF to USAID after the 2022 update
  dplyr::mutate(funding_agency=dplyr::case_when(funding_agency== "USAID/WCF"~"USAID", 
                                               TRUE ~funding_agency))%>%
  filter(funding_agency =="USAID",
         fiscal_year == fiscal_yr)

rm(df_hrh_full)
gc()

# Select OUs
ou <- unique(df_fsd$operatingunit)
ou <- append(ou, "Central Mechanisms")

# Subsets OUs if user chooses to subset OUs
if(!is.null(ou_choice)){
  if(is.character(ou_choice)){ou <- ou_choice}
  else{ou <- ou[ou_choice]}
}

# A tibble with 2 columns and # rows = # of OUs
### First column: string for pdf file name
### Second column: column of lists, with lists containing all parameters fed into the R Markdown
reports <- tibble(
  output_file = str_c("data_quality_", ou, ".pdf"),
  params = map(ou, ~list(ou = .))
)

# Use pwalk to iterate through the tibble with the "render" function
reports %>%
  # https://stackoverflow.com/questions/56606299/in-rstudio-knit-always-works-but-rmarkdownrender-fails-on-second-run-bu
  pwalk(rmarkdown::render, 
        input = glue("{src_path}/data_quality_report_template_v6-0.Rmd"), 
        output_dir = glue("{output_dir}/"),
        intermediates_dir = temp_dir)

### Move log files elsewhere, to avoid cluttering working directory
# selects files in current working directory
curr_files <- list.files()
curr_files <- curr_files[str_detect(curr_files, ".log")]
new_log_loc <- paste0(log_dir,sep="/", curr_files)
### Note: renaming a file with a new path is akin to moving it
file.rename(from = curr_files,
            to = new_log_loc)


# ##### Upload to Google Drive ##################################
# ### UNCOMMENT THIS WHEN YOU WANT TO UPLOAD #####################
# load_secrets()
# 
# upload_dir_to_gdrive(output_dir, drive_path)


