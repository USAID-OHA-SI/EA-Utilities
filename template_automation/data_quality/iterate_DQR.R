# Purpose: Iterates Data Quality Report templates for all OUs
# Author: David Song
# Date: 2021 Nov 8

# NOTE: Upload to drive

library(stringr)
library(gophr)
library(glamr)
library(glue)
library(tidyverse)
library(googledrive)

source('data_quality_source.R')


## Global ===============================================
fiscal_yr <- 2020
# path to Google Drive folder
drive_path <- '1lopD-1ADm2tEiSRb7wY5bl1iStBEz129'


## Functions =============================================
# Helper Func: Check if file exists before uploading
# Reason: Google API allows for duplicate file name uploads
drive_upload_uniq <- function(path_name,
                              existing_files,
                              drive_dir){
  file_name <- basename(path_name)
  if(!(file_name %in% existing_files)){
    drive_upload(path_name, path = as_id(drive_dir),
                 name = file_name)
  }
}

# Func: Upload directory to Google Drive
upload_dir <- function(local_dir, drive_path){
  # check if output directory already exists in drive, and if not,
  # then makes it
  if(!(local_dir %in% drive_ls(path = as_id(drive_path))$name)){
    drive_mkdir(local_dir,
                # path is to your desired Google drive directory
                path = as_id(drive_path))
  }
  
  # Get path for created directory
  drive_ids <- drive_ls(path = as_id(drive_path))
  drive_dir <- drive_ids$id[drive_ids$name == local_dir]
  
  lst_files <- list.files(path = local_dir)
  lst_paths <- paste0(glue('{local_dir}/'), lst_files, sep='')
  
  existing_files <-drive_ls(path = as_id(drive_dir))$name
  
  # Upload pdfs to Drive
  walk(lst_paths,
       ~ drive_upload_uniq(.x,
                           existing_files = existing_files,
                           drive_dir = drive_dir))
}



### MAIN ====================================================
# Build necessary directories if they are not present
temp_dir <- "temp"
output_dir <- "output"
dir.create(temp_dir, showWarning=F)
dir.create(output_dir, showWarning=F)

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

# Create temporary csv's for knitting
write.csv(df_msd, glue("{temp_dir}/df_msd.csv"), row.names = F)
write.csv(df_fsd, glue("{temp_dir}/df_fsd.csv"),  row.names = F)

# Select OUs
ou <- unique(df_fsd$operatingunit)

# Delete from memory
rm(df_msd)
rm(df_fsd)
gc()

######## REMOVE FOR THE REAL DEAL ########################
# for test, just choose 5 ous
ou <- ou[1:5]
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
        output_dir = glue("{output_dir}/"))

# This command will only work if within RStudio
.rs.restartR()
# RStudio must reset before csv's can be deleted
unlink("./temp/df_msd.csv")
unlink("./temp/df_fsd.csv")


##### Upload to Google Drive ##################################
### UNCOMMENT THIS WHEN YOU WANT TO UPLOAD #####################
load_secrets()

upload_dir(output_dir, drive_path)

