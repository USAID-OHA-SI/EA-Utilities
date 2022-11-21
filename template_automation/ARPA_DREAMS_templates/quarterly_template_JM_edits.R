## Quarterly template compiler for: ARPA, Quarterly, and DREAMS supplemental data
## Version 4.0: 
## Purpose: Generate templates for mechanisms to fill out ARPA/DREAMS/Quarterly data. 
##          Organizes mechanism templates by OU folder.
##
## Author: David Song
## Date: 20220317
## Version 4.0 Changes:
##        * Add comments and clean code for clarity
##        * Combined ARPA, DREAMS, and Quarterly template code. 
##              - Added if-statements to allow easy selection between template types
## Version 3.2 Changes:
##        * Sort into directories by OU, not one big folder

# LOCALS & SETUP =======================================================================

## IMPORTANT NOTE: set working directory to Source File for best results
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(installr)

# If statement checks to see if openxlsx is installed and has the correct version
### Note: Must use openxlsx version 4.2.3--latest version does not import workbook 
###       formatting correctly (as of 4.2.4), see below URL
###       https://github.com/ycphs/openxlsx/issues/207
packageurl <- "https://cran.r-project.org/src/contrib/Archive/openxlsx/openxlsx_4.2.3.tar.gz"
if(!require(openxlsx)){install.packages(packageurl, repos=NULL, type="source")
} else if (packageVersion('openxlsx') != "4.2.3"){
  detach("package:openxlsx", unload=TRUE)
  install.packages(packageurl, repos=NULL, type="source")}

if(!require(remotes)){install.packages("remotes")}
if(!require(httpuv)){install.packages('httpuv')}
if(!require(glamr)){remotes::install_github("USAID-OHA-SI/glamr", build_vignettes = TRUE)}
if(!require(gophr)){remotes::install_github("USAID-OHA-SI/gophr", build_vignettes = TRUE)}

install.packages("openxlsx")
library(httpuv)
library(openxlsx)
library(glamr)
library(gophr)
library(googledrive)
library(glue)
library(tidyverse)
library(fs)

# Set path to where the where GitHub folder can be found
home_dir <- "C:/Users/jmontespenaloza/Documents"
uploader_path <- "GitHub/EA-Utilities/upload_dir_to_gdrive.R"
# Use google drive uploader function
source(glue("{home_dir}/{uploader_path}"))

# Global Variables======================================================================

# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
fisc_dir <- "C:/Users/jmontespenaloza/Documents/test_folder"#"ARPA_templates"

# Path where ARPA/DREAMS/Quarterly template is stored. 
### Note: Normally in the working directory, but adjust path as needed
 templatePath <- "ARPA_ER_template_v3.xlsx"
 templatePath <- "quarterly_template_CC_v2.xlsx" # Quarterly Template, made for CDI in Jan 2022
 templatePath <- "DREAMS_template_v2.xlsx" # DREAMS template, never used in the end
 templatePath <- "Quarterly_DREAMS_ARPA_CC_v4.xlsx"
 templatePath <- "Quarterly_DREAMS_ARPA__v1.xlsx"
 templatePath <- "USAID_Community_Expenditure_template_V3.xlsx"
 
# Path to google drive directory
gdrive_path <- "1_09hkYm5sbz3h5fOlhITUBWmdIIWqhbR"
gdrive_path <- "1w-RzE6V5mCn3fAJT8asL_2K-ieYYdVvK"

# Select the Fiscal Year to use for the Quarterly Template
curr_year = 2022

# Select if ARPA, DREAMS, or Quarterly
template_type <- "Quarterly" #"ARPA" # "DREAMS" #"Quarterly"

# Set to TRUE if you want to drop cost categories
drop_cost <- FALSE

# Set to TRUE if capturing data quarterly, not annually
add_quarter <- FALSE

# Set to TRUE if drop Workplan Budget data
drop_workplan <- FALSE


# Functions ============================================================================

# Set cell styles. See: https://ycphs.github.io/openxlsx/reference/createStyle.html
template_style <- createStyle(fgFill = "#dcdcdc")
df_all_cell_style <- createStyle(border = "TopBottomLeftRight", borderStyle = 'thin')
# Style Excel Workbook column to have currency sign
dollar_cell <- createStyle(numFmt = '_($* #,##0_);_($* (#,##0);_($* "-"??_);_(@_)') 


## Generate data.frame with cost category, for Work Plan Budget and Expenditure
## Input: df: data.frame with only ONE unique mechanism_id
##        drop_workplan: Boolean to drop workplan column. TRUE drops work plan
##        drop_cost: Boolean to drop cost category. TRUE drops cost category
gen_df_template <- function(df, drop_workplan, drop_cost){
  # Concat columns
  df_temp <- df %>%
    add_column("program: sub_program" = glue("{.$program}: {.$sub_program}"),
               .before = "program") %>%
    add_column("beneficiary: sub_beneficiary" = glue("{.$beneficiary}: {.$sub_beneficiary}"),
               .before = "beneficiary")
  
  if (drop_cost){
    df_temp <- df_temp %>%
      group_by(`program: sub_program`, interaction_type, `beneficiary: sub_beneficiary`) %>%
      summarize_at(vars(workplan_budget_amt, expenditure_amt), sum, na.rm=T)
  } else{
    drop_cols_cost <- c("cop_budget_total", "operatingunit", "countryname",
                        "primepartner", "mech_name", "mech_code",
                        "program", "sub_program", "beneficiary", "sub_beneficiary",
                        "cost_category", "sub_cost_category",
                        "planning_cycle", "fiscal_year")
    df_temp <- df_temp %>%
      add_column("cost_category: sub_cost_category" = glue("{.$cost_category}: {.$sub_cost_category}"),
                 .before = "cost_category") %>%
      select(-drop_cols_cost)
  }
  
  if (drop_workplan){
    df_temp <- df_temp %>% select(-workplan_budget_amt)
  }
  return(df_temp)
}

# Pipeline: Munges mechanism-level dataframe and saves it as an Excel workbook
### Inputs: mech: integer. Mechanism code
###         template_type: string. Choose "ARPA", "DREAMS", or "Quarterly"
###         dr: The string pathway for the local directory for outputs. 
###             Default dr is RStudio's working environment
###         df: data.frame. Default is FSD dataframe
wb_pipeline <- function(mech, templ_type, dr = "", df = df_fsd){
  # filter data.frame for one single mechanism
  df_mech <- df %>% dplyr::filter(mech_code == mech) 
  # Generate correct columns and group_by sums
  df_template <- gen_df_template(df_mech, drop_workplan, drop_cost)

  # 3 if-statements to test if the template_type is ARPA, DREAMS, or Quarterly
  if (templ_type=="ARPA"){
    # Add empty columns so that column headers correctly display in Excel
    df_template <- df_template %>%
      add_column(ipc_cse = NA,
                 ipc_hrh = NA,
                 ipc_other = NA)
  } else if(templ_type == "DREAMS"){
    df_template <- df_template %>%
      add_column(dreams_budget = NA,
                 expenditureeeee = NA,
                 expenditure_Q2 = NA,
                 dreams_budget_amt_Q2 = NA,
                 dreams_expenditure_amt_Q2 = NA,
                 expenditure_Q3 = NA,
                 dreams_budget_amt_Q3 = NA,
                 dreams_expenditure_amt_Q3 = NA,
                 expenditure_Q4 = NA,
                 dreams_budget_amt_Q4 = NA,
                 dreams_expenditure_amt_Q4 = NA,
                 Notes_Comments= NA,
                 Expenditure_total= NA)
  } else if(templ_type == "Quarterly"){
    # set all Expenditure amounts to null, as that is the column mechs will fill out quarterly
    df_template$expenditure_amt <- NA
  } else{
    stop("Error! Pick template_type that is ARPA, DREAMS, or Quarterly")
  }
  
  # Number of columns in the dataframe, to add formatting correctly later
  num_cols <- ncol(df_template)
  
  # Save country/OU name, prime name, and mechanism name/ID as a list, for later reference
  # NOTE: if column order changes, this code will break
  mech_id <- df_mech[1,c(1:5, 14)]
  # Add empty column for Quarter input. If TRUE, adds "Quarter" box to fill out on 1st page of template
  if (add_quarter){
    mech_id$Quarter <- ""
  }
  
  # For now, just store template in same folder as R code and load it. Otherwise, change templatePath
  wb <- loadWorkbook(templatePath)

  # Use openxlsx to write in mech ID and financidal data in the approrpiate sheets
  ### See: https://ycphs.github.io/openxlsx/reference/writeData.html
  ### Note: colNames and withFilter BOTH need to be FALSE to avoid column headers
  writeData(wb, sheet = 1, x = mech_id, startRow = 4, colNames = FALSE, withFilter = FALSE)
  writeData(wb, sheet = 2, x = df_template, startRow = 2, colNames = FALSE, withFilter=FALSE)
  
  # Set varying start column for formatting based on if using cost categories
  if (drop_cost){
    start_col <- 4
  } else{
    start_col <- 5
  }
  
  # Set varying end column for where to format grey background fill to, depending on template type
  if(template_type == "ARPA" | template_type=="Quarterly"){
    fill_col <- start_col
  }else if(template_type == "DREAMS"){
    fill_col <- start_col
  }
  
  # Add styles based on the styles we created earlier. See: https://ycphs.github.io/openxlsx/reference/addStyle.html
  addStyle(wb, sheet = 2, template_style, rows = 2:200, cols = 1:fill_col,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 2, df_all_cell_style, rows = 2:200, cols = 1:num_cols, 
           gridExpand = TRUE, stack = T)
  addStyle(wb, sheet = 2, dollar_cell, rows = 2:200, cols=start_col:num_cols, gridExpand=T, stack=T)
  
  # Save ARPA template
  #file_name <- glue("{dr}{mech_id[2]}_{mech_id[4]}_{template_type}_template.xlsx")
  file_name <- glue("{dr}{mech_id[2]}_{mech_id[4]}_Expenditure_Template.xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
  return(df_template)
}

# Role: Progress tracker is a wrapper around the wb_pipeline function to track
#       progress on the building of files. This tracker works in tandem with loops, such as walk
### Input: track_num: integer. The number of files to create before printing progress 
###                   (e.g. print progress every x files)
###        global_progress: integer. A global variable that increases each time a function is run
###        total_files: integer. Total number of files
###        func: the function that the progress tracker is tracking
###        ...: add as many additional arguments as necessary. These additional arguments
###             are passed onto func. See: https://www.burns-stat.com/the-three-dots-construct-in-r/
progress_tracker <- function(track_num, global_progress = global_progress,
                             total_files = total_files, func, ...){
  if (global_progress == 1){
    cat("Starting building files\n")
  }
  
  if (global_progress %% track_num == 0){
    cat("Building", global_progress, "out of", total_files, "files\n", sep = " ")
  }
  
  # Function that progress_tracker is wrapping goes here
  output <- func(...)
  
  global_progress <<- global_progress + 1
  
  if (global_progress == total_files){
    cat("Finished building all files.\n")
    global_progress <<- 1
  }
  return(output)
}


# LOAD DATA ============================================================================  
df_lst_arpa <- read.xlsx("PEPFAR ARPA Budgets by USAID Mechanism.xlsx", startRow=3)

df_fsd_full <-glamr::si_path()%>%
  return_latest("Fin")%>% 
  gophr::read_msd()


df_fsd <- df_fsd_full %>%
  dplyr::filter(fundingagency == "USAID" | fundingagency=="USAID/WCF",
         fiscal_year == curr_year) %>%
  remove_mo()

# Remove full FSD to empty memory 
rm(df_fsd_full)
gc()


# MUNG ===============================================================================
# List columns and drop unused columns. Ramona requested we keep interaction_type
drop_cols <- c("fundingagency", "prime_partner_duns", "prime_partner_org_type", 
               "is_indigenous_prime_partner", "procurement_type", "subrecipient_name",
               "subrecipient_duns", "award_number", "record_type",
               "cop_budget_new_funding", "cop_budget_pipeline")

# drop columns in the drop column. 
df_fsd <- df_fsd%>% select(-drop_cols)

# Drop anything before COP19 (requested by Ramona)
df_fsd <- df_fsd %>% filter(planning_cycle != 'COP18' & planning_cycle != 'COP17')

# Replace "Program Management" with "IM Program Management"
df_fsd$sub_program <- replace(df_fsd$sub_program, df_fsd$sub_program == "Program Management", "IM Program Management")

#df_fsd$cost_category <- replace(df_fsd$cost_category, df_fsd$cost_category == "Not Specified", "")
#df_fsd$sub_cost_category <- replace(df_fsd$sub_cost_category, df_fsd$sub_cost_category == "Not Specified", "")                                

df_fsd <- df_fsd%>% filter(cost_category != 'Not Specified')

df_fsd <- df_fsd %>% select(-c("expenditure_amt", "prime_partner_uei", "subrecipient_uei", "funding_account"))

# # # TEST FUNCTIONS =======================================================================
# # Test one mechanism on function pipeline
# test_mech <- 70069
# test_output <- wb_pipeline(test_mech)

# Generate Files=========================================================================

# Get total # of files to process (i.e. # of unique mechs)
lst_mech <- df_fsd %>% distinct(mech_code) %>% pull()
total_files <- length(lst_mech)

# Get list of unique OUs 
lst_ou <- df_fsd %>% distinct(operatingunit) %>% pull()

# ####### THIS SHORTENS LIST FOR TEST RUN #######
# lst_ou <- lst_ou[1:2]
 lst_ou <- c("Zimbabwe")

#create output folders folders locally
dir_create(fisc_dir)

# Global Progress Tracking variables
global_progress <- 1

# For-loop creates a folder for each unique OU
for (ou in lst_ou){
  ou_dir <- glue("{fisc_dir}/{ou}/")
  dir_create(ou_dir)
  df_ou <- df_fsd %>% filter(operatingunit == ou)
  # create list of unique mechanisms within an OU
  ou_lst_mech <- df_ou %>% distinct(mech_code) %>% pull()
  # Create output budget files by walking through all mechs in an OU and saving in the correct OU folder
  walk(ou_lst_mech,
       function(x) progress_tracker(track_num = 50,global_progress = global_progress,
                                    total_files = total_files,
                                    func = wb_pipeline,
                                    mech = x,
                                    templ_type = template_type,
                                    dr = ou_dir)
       )
}

# UPLOAD==============================================================================
# load_secrets()
# 
# # Must use for-loop to loop through all the folders in our fisc_dir folder
# for (dir in list.dirs(fisc_dir)[-1]){
#   upload_dir_to_gdrive(dir, gdrive_path)
# }

