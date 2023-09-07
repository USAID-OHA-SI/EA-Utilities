## version 2.4: 
## New Features in 2.4 include:
##            * Control openxlsx version used to 4.2.3
##              Latest version (4.2.4) does not preserve imported styling

# LOCALS & SETUP =======================================================================

## IMPORTANT NOTE: set working directory to Source File for best results
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
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
library(googlesheets4)
library(glue)
library(googledrive)
library(janitor)
library(tidyverse)
library(fs) #to create folders

# Global Variables======================================================================

# Drive path is to Partner_data_output folder. Change path name to select correct upload path
glbl_id <- '1xqUyuM7NNnVzuh509W5dmlPHykfBItyC'

# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
fisc_dir = "ER22_Financial_Programmatic/"


# Functions ============================================================================
## Input: single string (NOT list of strings)
filter_one_mech <- function(mech, df = df_fsd){
  df %>% filter(mech_code == mech) 
}

## Generate data.frame without cost category, so work plan and expenditure data
## are summed to the intervention level (i.e. aggregated)
## Input: data.frame with only ONE unique mechanism_id
gen_df_no_cost <- function(df, null_rm=TRUE){
  df <- 
    df %>% 
    group_by(planning_cycle, fiscal_year, program, sub_program, interaction_type, 
             beneficiary, sub_beneficiary) %>% 
    summarise_at(vars(cop_budget_total, ), 
                 sum, na.rm = null_rm) %>%
    ungroup()
  
  df[order(df$fiscal_year, decreasing=TRUE), ]
}
## Generate data.frame with cost category. Since Work Plans and ER data have cost
## category information, only FAST is excluded in this data.frame
gen_df_with_cost <- function(df){
  drop_cols_temp <- c("cop_budget_total", "operatingunit", "country", "funding_agency",
                      "prime_partner_name", "mech_name", "mech_code","funding_account","prime_partner_uei","subrecipient_uei")

  df<-df%>%
    filter(cost_category !="Not Specified")
  df <- df[ , !names(df) %in% drop_cols_temp]
  df[order(df$fiscal_year, decreasing=TRUE), ] %>%
    select(planning_cycle, fiscal_year, everything())
}


## Pipeline
## dr is the string pathway for the local directory for outputs. 
## Default dr is RStudio's working environment
template_path <- '~/GitHub/EA-Utilities/template_automation/Partner Engagement/partnerDataTemplateV1.xlsx'

wb_pipeline <- function(mech, dr = "", df = df_fsd){
  df_mech <- filter_one_mech(mech)
  df_no_cost <- gen_df_no_cost(df_mech)
  df_with_cost <- gen_df_with_cost(df_mech)
  
  # Save country/OU name, prime name, and mechanism name/ID
  # NOTE: if column order changes, this code will break
  # mech_id <- df_mech[1,1:5]
  mech_id<-df_mech%>%
    select(operatingunit,country,prime_partner_name,mech_code,mech_name)
   mech_id<- mech_id[1,1:5]
  
  wb <- loadWorkbook(template_path)
  
  writeData(wb, sheet = 1, x = mech_id, startRow = 4, colNames = FALSE, 
            withFilter = FALSE)
  writeData(wb, sheet = 2, x = df_no_cost, startRow = 3, colNames = FALSE,
                 withFilter = FALSE)
  writeData(wb, sheet = 3, x = df_with_cost, startRow = 3, colNames = FALSE,
                 withFilter = FALSE)
  
  addFilter(wb, sheet = 2, rows = 2, cols = 1:8) #give user ability to filter by categories
  addFilter(wb, sheet = 3, rows = 2, cols = 1:10)
  
  file_name <- glue("{dr}{mech_id[2]}_{mech_id[4]}_budget_reference_file.xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  return(list(df_no_cost, df_with_cost))
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
#load_secrets()

# LOAD DATA ============================================================================  
# si_path is a function in glamr that outputs saved string path to local Data
glamr::si_path()
#<- 'Financial_Structured_Datasets_COP17-21_20210917.txt'

# Run glamr function "si_paths" to generate local paths based on organization's
# common folder structure for data stored locally.
# Then call latest file, read in data.frame and filter to select only USAID entries
df_fsd <-si_path()%>%
  return_latest("Fin")%>% 
  gophr::read_msd()%>% 
  filter(str_detect(funding_agency, 'USAID'))%>%
  remove_mo()

# MUNG ===============================================================================

# List columns and drop unused columns. Ramona requested we keep interaction_type
###names(df_fsd)
drop_cols <- c("fundingagency", "prime_partner_duns", "prime_partner_org_type", 
               "is_indigenous_prime_partner", "procurement_type", "subrecipient_name",
               "subrecipient_duns", "award_number", "record_type",
               "cop_budget_new_funding", "cop_budget_pipeline","expenditure_amt")

df_fsd <- df_fsd[ , !(names(df_fsd) %in% drop_cols)]

# Keep only COP21
df_fsd <- df_fsd %>%
  filter(planning_cycle=="COP22")

# Replace "Program Management" with "IM Program Management"
df_fsd$sub_program <- replace(df_fsd$sub_program, df_fsd$sub_program == "Program Management", "IM Program Management")

# Next 3 lines of code check to see if "Program Management" is still in df_fsd or not
program_group <- split(df_fsd$sub_program, df_fsd$program)
uniq_subprogs <- lapply(program_group, unique)
uniq_subprogs[4]

# # TEST FUNCTIONS =======================================================================
# Test one mechanism on function pipeline
test_mech <- "84910"

test_df <- filter_one_mech(test_mech)
test_df_no_cost <- gen_df_no_cost(test_df)
test_df_with_cost <- gen_df_with_cost(test_df)

test_output <- wb_pipeline(test_mech)


# Generate Files=========================================================================

# # List of mechanisms
lst_mech <-
  df_fsd %>%
  distinct(mech_code) %>%
  pull()

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

# # Length of # of unique mechs matches the number of files created in ER directory
# total_files == length(list.files(fisc_dir))
# 
# 
# 
# # UPLOAD==============================================================================
# #### lines 212-217, 223-234 from Ben Kasdan's code "pull partner data.R"
# 
#create folder for upload
drive_mkdir(fisc_dir,
            path = as_id(glbl_id))

# Get path for created directory
drive_ids <- drive_ls(path = as_id(glbl_id))
drive_fisc_dir <- drive_ids$id[drive_ids$name == fisc_dir]

#identify list of
local_files <- list.files(fisc_dir, full.names = TRUE)

#push to drive
walk(local_files,
     ~ drive_upload(.x,
                    path = as_id(drive_fisc_dir), #path is to the ER21 test file folder
                    name = basename(.x),
                    type = "spreadsheet"))
# 
# #remove all local files
# unlink(fisc_dir_name, recursive = TRUE)
