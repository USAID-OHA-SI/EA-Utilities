## Version 3.0: 
## Changes:
##        * Sort into directories by OU, not one big folder
## Version 2.1 Changes:
##        * Use new ARPA category columns
##        * Reads in style through xlsx template rather than hard coding
##              - version 2.1 uses ARPA_ER_template_v2.xlsx
##        * Must use openxlsx version 4.2.3--latest version does not import 
##          workbook formatting correctly (as of 4.2.4), see below URL
##          https://github.com/ycphs/openxlsx/issues/207

# LOCALS & SETUP =======================================================================

## IMPORTANT NOTE: set working directory to Source File for best results
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContWext()$path))

# Libraries
library(installr)
### If statement checks to see if openxlsx is installed and has the correct version
packageurl <- "https://cran.r-project.org/src/contrib/Archive/openxlsx/openxlsx_4.2.3.tar.gz"
if(!require(openxlsx)){install.packages('openxlsx')#packageurl, repos=NULL, type="source")
} else if (packageVersion('openxlsx') != "4.2.3"){
  detach("package:openxlsx", unload=TRUE)
  install.packages(packageurl, repos=NULL, type="source")}

if(!require(remotes)){install.packages("remotes")}
if(!require(httpuv)){install.packages('httpuv')}
if(!require(glamr)){remotes::install_github("USAID-OHA-SI/glamr", build_vignettes = TRUE)}
if(!require(gophr)){remotes::install_github("USAID-OHA-SI/gophr", build_vignettes = TRUE)}

library(openxlsx)
library(glamr)
library(gophr)
library(googledrive)
library(glue)
library(tidyverse)
library(fs)

# Use google drive uploader function
home_dir <- "C:/Users/davidsong/Desktop/USAID"
uploader_path <- "/GitHub/EA-Utilities/upload_dir_to_gdrive.R"
source(glue("{home_dir}{uploader_path}"))

# Global Variables======================================================================

# Drive path is to Partner_data_output folder. Change path name to select correct upload path
glbl_id <- '1_09hkYm5sbz3h5fOlhITUBWmdIIWqhbR'

# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
fisc_dir <- "ARPA_templates"

# Path where ARPA template is stored
templatePath <- "ARPA_ER_template_v2.xlsx"

# Path to google drive directory
gdrive_path <- "1alqyjq1IjEXlF6ZydLHe5pp0ZDzcbDGa"

# Select the fiscal year to use for the Quarterly Template
curr_year = 2021


# Functions ============================================================================
## Input: single string (NOT list of strings)
filter_one_mech <- function(mech, df = df_fsd){
  df %>% filter(mech_code == mech) 
}


## Generate data.frame with cost category, for Work Plan Budget and Expenditure
## Input: data.frame with only ONE unique mechanism_id
gen_df_arpa <- function(df){
  # Concat columns
  df <- df %>%
    add_column("program: sub_program" = glue("{.$program}: {.$sub_program}"),
               .before = "program")
  df <- df %>%
    add_column("beneficiary: sub_beneficiary" = glue("{.$beneficiary}: {.$sub_beneficiary}"),
               .before = "beneficiary")
  
  drop_cols_temp <- c("cop_budget_total", "operatingunit", "countryname",
                      "primepartner", "mech_name", "mech_code",
                      "program", "sub_program", "beneficiary", "sub_beneficiary",
                      "cost_category", "sub_cost_category",
                      "planning_cycle", "fiscal_year",
                      "workplan_budget_amt")
  
  df <- df[ , !names(df) %in% drop_cols_temp]
}



## Pipeline
## dr is the string pathway for the local directory for outputs. 
## Default dr is RStudio's working environment
wb_pipeline <- function(mech, dr = "", df = df_fsd){
  df_mech <- filter_one_mech(mech)
  df_arpa <- gen_df_arpa(df_mech)
  df_arpa <- df_arpa %>%
    add_column(ipc_cse = NA,
               ipc_hrh = NA,
               ipc_other = NA,
               vax_cse = NA,
               vax_hrh = NA,
               vax_other = NA,
               test_cse = NA,
               test_hrh = NA,
               test_other = NA,
               clinical_cse = NA,
               clinical_hrh = NA,
               clinical_other = NA,
               other_cse = NA,
               other_hrh = NA,
               other_other = NA,
               mitig_repair = NA,
               mitig_logistics = NA,
               mitig_lab = NA,
               mitig_other = NA,
               activity_description = NA)
  
  # Save country/OU name, prime name, and mechanism name/ID
  # NOTE: if column order changes, this code will break
  mech_id <- df_mech[1,c(1:5,14)]
  
  # For now, just store template in same folder as R code
  wb <- loadWorkbook(templatePath)

  writeData(wb, sheet = 1, x = mech_id, startRow = 4, colNames = FALSE, withFilter = FALSE)
  writeDataTable(wb, sheet = 2, x = df_arpa, startRow = 2)#, colNames = FALSE)
  
  # Set cell styles
  template_style <- createStyle(fgFill = "#dcdcdc")
  df_all_cell_style <- createStyle(border = "TopBottomLeftRight", borderStyle = 'thin')
  
  addStyle(wb, sheet = 2, template_style, rows = 3:200, cols = 1:4,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 2, df_all_cell_style, rows = 3:200, cols = 1:24, 
           gridExpand = TRUE, stack = T)
  
  file_name <- glue("{dr}{mech_id[2]}_{mech_id[4]}_ER_template.xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
  return(df_arpa)
}

# Progress tracker is a wrapper around the wb_pipeline function to track
# progress on the building of files
progress_tracker <- function(track_num, global_progress = global_progress,
                             total_files = total_files, func, ...){
  if (global_progress == 1){
    cat("Starting building files\n")
  }
  
  if (global_progress %% track_num == 0){
    cat("Building", global_progress, "out of", total_files, "files\n", sep = " ")
  }
  
  ### Can change function used here. Better to have chosen function be an
  ### argument of the Progress Tracker function, but that is annoying to code right now
  output <- func(...)
  
  global_progress <<- global_progress + 1
  
  if (global_progress == total_files){
    cat("Finished building all files.\n")
    global_progress <<- 1
  }
  return(output)
}



# Output folder ========================================================================

# LOAD DATA ============================================================================  
df_lst_arpa <- read.xlsx("PEPFAR ARPA Budgets by USAID Mechanism.xlsx", startRow=3)

df_fsd_full <-glamr::si_path()%>%
  return_latest("Fin")%>% 
  gophr::read_msd()

df_fsd <- df_fsd_full %>%
  filter(fundingagency == "USAID",
         fiscal_year == curr_year,
         mech_code %in% df_lst_arpa$Mechanism.ID
         ) %>%
  remove_mo()

rm(df_fsd_full)
gc()


# MUNG ===============================================================================

# List columns and drop unused columns. Ramona requested we keep interaction_type
###names(df_fsd)
drop_cols <- c("fundingagency", "prime_partner_duns", "prime_partner_org_type", 
               "is_indigenous_prime_partner", "procurement_type", "subrecipient_name",
               "subrecipient_duns", "award_number", "record_type",
               "cop_budget_new_funding", "cop_budget_pipeline")

df_fsd <- df_fsd[ , !(names(df_fsd) %in% drop_cols)]

# Drop anything before COP19 (requested by Ramona)
df_fsd <- df_fsd[(df_fsd$planning_cycle != 'COP18') & (df_fsd$planning_cycle != 'COP17'), ]

# Replace "Program Management" with "IM Program Management"
df_fsd$sub_program <- replace(df_fsd$sub_program, df_fsd$sub_program == "Program Management", "IM Program Management")


# # TEST FUNCTIONS =======================================================================
# Test one mechanism on function pipeline
test_mech <- "70212"

test_df <- df_fsd %>% filter(mech_code == test_mech)
# test_df_with_cost <- gen_df_arpa(test_df)

test_output <- wb_pipeline(test_mech)


# Generate Files=========================================================================

# Get total # of files to process (i.e. # of unique mechs)
lst_mech <- df_fsd %>% distinct(mech_code) %>% pull()
total_files <- length(lst_mech)

# Get list of unique OUs 
lst_ou <- df_fsd %>% distinct(operatingunit) %>% pull()

# ####### THIS SHORTENS LIST FOR TEST RUN #######
# lst_ou <- lst_ou[1:2]

#create output folders folders locally
dir_create(fisc_dir)

# Global Progress Tracking variables
global_progress <- 1

for (ou in lst_ou){
  ou_dir <- glue("{fisc_dir}/{ou}/")
  dir_create(ou_dir)
  df_ou <- df_fsd %>% filter(operatingunit == ou)
  ou_lst_mech <- df_ou %>% distinct(mech_code) %>% pull()
  # Create output budget files
  walk(ou_lst_mech,
       function(x) progress_tracker(track_num = 50,global_progress = global_progress,
                                    total_files = total_files,
                                    func = wb_pipeline,
                                    mech = x,dr = ou_dir)
       )
}

# UPLOAD==============================================================================
# load_secrets()

for (dir in list.dirs(fisc_dir)[-1]){
  upload_dir_to_gdrive(dir, gdrive_path)
}

