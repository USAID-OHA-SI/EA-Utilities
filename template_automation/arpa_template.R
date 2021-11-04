## Version 2.0: 
## Changes:
##        * Use new ARPA category columns
##        * Reads in style through xlsx template rather than hard coding
##        * Must use openxlsx version 4.2.3--latest version does not import 
##          workbook formatting correctly (as of 4.2.4), see below URL
##          https://github.com/ycphs/openxlsx/issues/207

# LOCALS & SETUP =======================================================================

## IMPORTANT NOTE: set working directory to Source File for best results
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
library(googlesheets4)
library(glue)
library(janitor)
library(tidyverse)
library(fs) #to create folders

# Global Variables======================================================================

# Drive path is to Partner_data_output folder. Change path name to select correct upload path
glbl_id <- '1mCR6yqGiaKxXPpl2QHsYR08B-5oaLsgE'

# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
fisc_dir = "ARPA_templates/"

# Select the fiscal year to use for the Quarterly Template
curr_year = 2022


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
    add_column(prev_ipc = NA,
               prev_vax = NA,
               prev_test = NA,
               prev_clinical = NA,
               prev_other = NA, 
               mitig_repair = NA,
               mitig_logistics = NA,
               mitig_lab = NA,
               mitig_other = NA)
  
  # Save country/OU name, prime name, and mechanism name/ID
  # NOTE: if column order changes, this code will break
  mech_id <- df_mech[1,c(1:5,14)]
  
  # For now, just store template in same folder as R code
  templatePath <- "ARPA_ER_template_v1.xlsx"
  wb <- loadWorkbook(templatePath)

  writeData(wb, sheet = 1, x = mech_id, startRow = 4, colNames = FALSE, withFilter = FALSE)
  writeDataTable(wb, sheet = 2, x = df_arpa, startRow = 2)#, colNames = FALSE)
  
  file_name <- glue("{dr}{mech_id[2]}_{mech_id[4]}_ER_template_TEST.xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  return(df_arpa)
}

# Progress tracker is a wrapper around the wb_pipeline function to track
# progress on the building of files
progress_tracker <- function(global_progress, mech_id, dr){
  if (global_progress == 1){
    cat("Starting building files\n")
  }
  
  if (global_progress %% 50 == 0){
    cat("Building", global_progress, "out of", total_files, "files\n", sep = " ")
  }
  
  ### Can change function used here. Better to have chosen function be an
  ### argument of the Progress Tracker function, but that is annoying to code right now
  wb_pipeline(mech_id, dr)
  
  global_progress <<- global_progress + 1
  
  if (global_progress == total_files){
    cat("Finished building all xlsx files.\n")
    global_progress <<- 1
  }
}


# Output folder ========================================================================

# LOAD DATA ============================================================================  
# Run glamr function "si_paths" to generate local paths based on organization's
# common folder structure for data stored locally.
# Then call latest file, read in data.frame and filter to select only USAID entries
df_fsd <-si_path()%>%
  return_latest("Fin")%>% 
  gophr::read_msd()%>% 
  filter(fundingagency == "USAID",
         fiscal_year == curr_year
         ) %>%
  remove_mo()

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

test_df <- filter_one_mech(test_mech)
test_df_with_cost <- gen_df_arpa(test_df)

test_output <- wb_pipeline(test_mech)



# Generate Files=========================================================================

# List of mechanisms
lst_mech <-
  df_fsd %>%
  distinct(mech_code) %>%
  pull()


####### THIS SHORTENS LIST FOR TEST RUN #######
lst_mech <- lst_mech[1:6]

# removes backlash for creation of actual directory
fisc_dir_name = substr(fisc_dir, 1, nchar(fisc_dir)-1)
#create output folders folders locally
dir_create(fisc_dir_name)

# Generate list from 1 to length of list
lst_positions <- seq(1, length(lst_mech))
# Global Progress Tracking variables
global_progress <- 1
total_files <- length(lst_mech)

# Create output budget files
walk(lst_mech,
      function(x, y) progress_tracker(global_progress = global_progress,
                                      mech_id = x,
                                      dr = fisc_dir
                                      ))

# Length of # of unique mechs matches the number of files created in ER directory
total_files == length(list.files(fisc_dir))



# UPLOAD==============================================================================
load_secrets()

drive_ids <- drive_ls(path = as_id(glbl_id))
# Check if folder with same name exists, then create folder for upload
if (!(fisc_dir_name %in% drive_ids$name)){
  drive_mkdir(fisc_dir_name,
              path = as_id(glbl_id))
  # update drive_ids with new directory
  drive_ids <- drive_ls(path = as_id(glbl_id))
}
# Get path for new directory in Drive

drive_fisc_dir <- drive_ids$id[drive_ids$name == fisc_dir_name]

#identify list of files stored locally to move to Drive
local_files <- list.files(fisc_dir_name, full.names = TRUE)

#push to drive
### NOTE: For future versions, build collision detection function, since Drive
###       does not overwrite files with the same name
walk(local_files,
     ~ drive_upload(.x,
                    path = as_id(drive_fisc_dir), #path is to the ER21 test file folder
                    name = basename(.x)))
                    #type = "spreadsheet"))

# #remove all local files
# unlink(fisc_dir_name, recursive = TRUE)
