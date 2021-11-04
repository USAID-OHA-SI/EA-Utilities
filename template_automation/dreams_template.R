## Version 2.0:
## New Features in 2.0 include:
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
glbl_id <- '10MUro75F5psphARCr0-0pnqpCMNswEIB'

# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
fisc_dir = "ER_quarterly_templates/"

# Select the fiscal year to use for the Quarterly Template
curr_year = 2022


# Functions ============================================================================
## Input: single string (NOT list of strings)
filter_one_mech <- function(mech, df = df_fsd){
  df %>% filter(mech_code == mech) 
}


## Generate data.frame with cost category, for Work Plan Budget and Expenditure
## Input: data.frame with only ONE unique mechanism_id
gen_df_with_cost <- function(df){
  # Concat columns
  df <- df %>%
    add_column("program: sub_program" = glue("{.$program}: {.$sub_program}"),
               .before = "program")
  df <- df %>%
    add_column("beneficiary: sub_beneficiary" = glue("{.$beneficiary}: {.$sub_beneficiary}"),
               .before = "beneficiary")
  df <- df %>%
    add_column("cost_category: sub_cost_category" = glue("{.$cost_category}: {.$sub_cost_category}"),
               .before = "cost_category")
  
  drop_cols_temp <- c("cop_budget_total", "operatingunit", "countryname",
                      "primepartner", "mech_name", "mech_code",
                      "program", "sub_program", "beneficiary", "sub_beneficiary",
                      "cost_category", "sub_cost_category",
                      "planning_cycle", "fiscal_year")
  
  df <- df[ , !names(df) %in% drop_cols_temp]
}



## Pipeline
## dr is the string pathway for the local directory for outputs. 
## Default dr is RStudio's working environment
wb_pipeline <- function(mech, dr = "", df = df_fsd){
  df_mech <- filter_one_mech(mech)
  df_with_cost <- gen_df_with_cost(df_mech)
  df_with_cost <- df_with_cost %>%
    add_column(dreams_budget_amt = NA,
               dreams_expenditure_amt = NA)
  
  # Save country/OU name, prime name, and mechanism name/ID
  # NOTE: if column order changes, this code will break
  mech_id <- df_mech[1,c(1:5,14)]
  # Add two empty columns for Month and Quarter input
  mech_id$Month <- ""
  mech_id$Quarter <- ""
  
  # Notes for end-user
  title1 <- "USAID Financial Quarterly Template"
  purpose1 <- data.frame(Purpose = paste0("The USAID Financial Quarterly template was developed by the OHA Expenditure Analysis Branch",
                                          "to provide USAID field teams with a standardized and adaptable framework to gather quarterly",
                                          "financial data if / as desired.   The template can be used at",
                                          "any level of detail desired to match the analytic questions of field teams. For analysis of",
                                          "data, the OHA EA branch can support aggregation of templates across partners into a structured dataset."))
  note1 <- data.frame(Note = paste0(" *Please ensure that there are no modifications made to the template (e.g. fonts, alignments, additional columns,",
                                    "or tabs). Doing so will prevent from aggregating data from multiple templates. "))
  note2 <- data.frame(Contact = "For questions please reach out to the EA Branch at oha.ea@usaid.gov")
  
  meta_names <- c("Operating Unit", "Country", "Partner Name", "Mech ID", "Mechanism Name", 
                  "Fiscal Year", "Month", "Quarter")
  meta_description <- c("The Agency, Region, Country, or multilateral institutions with an Operational Plan, responsible for executing a PEPFAR program or activity.",
                        "Name of the country, which can be different than the OU when the OU is a Region.",
                        "The name of the Implementing partner (IP), also known as the prime recipient, principal recipient.",
                        "Four-digit to six-digit numeric value uniquely identifying each mechanism in each OU",
                        "Name of the implementing mechanism that collected the data.",
                        "The USG fiscal year that the planned activity will be implemented.",
                        "",
                        "The Quarter in which the planned activity will be implemented in during the USG fiscal year."
                        )
  meta_len_names <- length(meta_names)
  matrix_meta_names <- matrix(meta_names, 1, meta_len_names)
  df_meta <- data.frame("Column Name" = meta_names, "Column Description" = meta_description)
  
  dict_col_names <- c("Program Area", "Interaction Type", "Beneficiary", "Cost Categories", 
                      "Workplan Budget", "Expenditure", "DREAMS Expenditure", "DREAMS Budget"
  )
########### QUESITON: DO WE NEED DEFINITION FOR WORKPLAN BUDGET AND QUARTER?
  dict_col_desc <- c(
                     "The Program Classification is the broadest aggregation of PEPFAR efforts stated as a general purpose",
                     "The interaction type classification describes the interaction with the beneficiary as either service delivery or non-service delivery.",
                     "The Beneficiary classification captures the group intended to be reached by the intervention, not necessarily the groups actually reached.",
                     "The Cost category classification identifies how PEPFAR funds will be used for the fiscal year.",
                     "",
                     "Indicates the USD dollar amount of the expenditures as collected in DATIM from Partner submissions.",
                     "Indicates the dollar amount amount spent on DREAMS of the Total Expenditure",
                     "Indicates the dollar amount of the allocated budget on DREAMS of the Total Work Plan Budget"
  )
  
  dict_lst_names <- c("Program Area", "Interaction Type", "Beneficiary", "Cost Categories", 
                 "Workplan Budget", "Expenditure", "DREAMS Budget", "DREAMS Expenditure")
  dict_len_names <- length(dict_lst_names)
  matrix_dict_names <- matrix(dict_lst_names, 1, dict_len_names)

  # Note: in the future, just read in the Excel and convert to data.frame, rather than hard code data dictionary in R
  datadict <- data.frame("Column Name" = dict_col_names, "Column Description" = dict_col_desc)
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "Notes and Data Dictionary")
  addWorksheet(wb, sheetName = "Cost Category-level IM data")
  setColWidths(wb, sheet = 1, cols = 1:5, widths = 22)
  setColWidths(wb, sheet = 1, cols = 6, widths = 10)
  setColWidths(wb, sheet = 2, cols = c(1, 2, 3, 4, 5, 6, 7, 8, 9), 
               widths = c(35, 22, 40, 40, 22, 25, 25, 25, 25))
  
  writeData(wb, sheet = 2, x = matrix_dict_names, startRow = 1, colNames = FALSE, withFilter = FALSE)
  writeDataTable(wb, sheet = 2, x = df_with_cost, startRow = 2, tableStyle = "TableStyleLight9")
  
  writeData(wb, sheet = 1, x = title1, startCol = 1, startRow = 1, colNames = TRUE, withFilter = FALSE)
  writeData(wb, sheet = 1, x = matrix_meta_names, startRow = 3, colNames = FALSE, withFilter = FALSE)
  writeData(wb, sheet = 1, x = mech_id, startRow = 4, colNames = FALSE, withFilter = FALSE)
  writeData(wb, sheet = 1, x = purpose1, startRow = 6, withFilter = FALSE)
  writeData(wb, sheet = 1, x = note1, startRow = 8, colNames = FALSE, withFilter = FALSE)
  writeData(wb, sheet = 1, x = note2, startRow = 9, colNames = FALSE, withFilter = FALSE)
  
  writeData(wb, sheet = 1, x = "Mechanism Data: Input Definitions", startRow = 11)
  writeData(wb, sheet = 1, x = df_meta, startRow = 12, withFilter = FALSE)
  writeData(wb, sheet = 1, x = "Column Name", startRow = 12, startCol =  1, withFilter = FALSE)
  writeData(wb, sheet = 1, x = "Column Description", startRow = 12, startCol =  2, withFilter = FALSE)
  
  writeData(wb, sheet = 1, x = "Data Dictionary: Input Template Column Definitions", startRow = 22)
  writeData(wb, sheet = 1, x = datadict, startRow = 23, withFilter = FALSE)
  writeData(wb, sheet = 1, x = "Column Name", startRow = 23, startCol =  1, withFilter = FALSE)
  writeData(wb, sheet = 1, x = "Column Description", startRow = 23, startCol =  2, withFilter = FALSE)
  
  # Merge rows for formatting
  merge_cols <- 1:5
  for (row_i in c(1:2, 6:11, 22)){
    mergeCells(wb, sheet = 1, cols = merge_cols, rows = row_i)
  }
  
  merge_df_cells <- 2:9
  for (row_i in c(12:21, 23:31)){
    mergeCells(wb, sheet = 1, cols = merge_df_cells, rows = row_i)
  }
  
  # Adjust row height
  setRowHeights(wb, 1, rows = c(1, 2, 7, 8), heights = c(26, 26, 60, 26))
  
  # Set cell styles
  wrap_style <- createStyle(wrapText = TRUE)
  title_style <- createStyle(fontName = "Calibri", fontSize = 18, textDecoration = "bold",
                             border = "bottom", borderStyle = "thin")
  mech_info_style <- createStyle(border = "TopBottomLeftRight", borderStyle = "medium",
                                 borderColour = "#C6E0B4")
  subtitle_style <- createStyle(fontName = "Calibri", fontSize = 12, textDecoration = "bold")
  purpose_txt_style <- createStyle(border = "TopBottomLeftRight", borderStyle = "thin")
  note_style <- createStyle(fontSize = 9, fontColour = "#FF0000",
                            textDecoration = c("bold", "underline"))
  
  df_all_cell_style <- createStyle(border = "TopBottomLeftRight", borderStyle = 'thin')
  df_top_style <- createStyle(fgFill = "#d3d3d3", halign = "center", textDecoration = "bold")
  df_name_style <- createStyle(textDecoration = 'bold')
  df1_style <- createStyle(fgFill = "#88C2E6")
  df2_style <- createStyle(fgFill = "#F4B084")
  df3_style <- createStyle(fgFill = "#C6E0B4")
  template_style <- createStyle(fgFill = "#dcdcdc")
  blank_style <- createStyle(fgFill = "#FFFFFF")
  
  
  # Apply styles to correct cells
  # length of all columns, to apply all styles
  all_cols <- 1:9
  
  addStyle(wb, sheet = 1, wrap_style, rows = 1:12, cols = all_cols, gridExpand = TRUE)
  addStyle(wb, sheet = 1, title_style, rows = 1, cols = merge_cols, gridExpand = TRUE)
  addStyle(wb, sheet = 1, df3_style, rows = 3, cols = 1:8, gridExpand = TRUE)
  addStyle(wb, sheet = 1, df_name_style, rows = 3, cols = 1:8, gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb, sheet = 1, mech_info_style, rows = 4, cols = 1:8, gridExpand = TRUE)
  addStyle(wb, sheet = 1, subtitle_style, rows = 6, cols = merge_cols, gridExpand = TRUE)
  addStyle(wb, sheet = 1, purpose_txt_style, rows = 7, cols = merge_cols, gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb, sheet = 1, note_style, rows = 8, cols = merge_cols, gridExpand = TRUE, 
           stack = TRUE)
  
  # Style for Mechanism info dictionary
  addStyle(wb, sheet = 1, subtitle_style, rows = 11, cols = merge_cols)
  addStyle(wb, sheet = 1, df_all_cell_style, rows = 12:20, cols = all_cols, 
           gridExpand = TRUE)
  addStyle(wb, sheet = 1, df_top_style, rows = 12, cols = all_cols, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, df_name_style, rows = 13:20, cols = 1, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, df3_style, rows = 13:20, cols = all_cols,
           gridExpand = TRUE, stack = TRUE)
  
  # Style for data dictionary
  addStyle(wb, sheet = 1, subtitle_style, rows = 22, cols = merge_cols)
  addStyle(wb, sheet = 1, df_all_cell_style, rows = 23:31, cols = all_cols, 
           gridExpand = TRUE)
  addStyle(wb, sheet = 1, df_top_style, rows = 23, cols = all_cols, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, df_name_style, rows = 24:31, cols = 1, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, df1_style, rows = 24:27, cols = all_cols,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, df2_style, rows = 28:31, cols = all_cols,
           gridExpand = TRUE, stack = TRUE)
  
  # Style for Template sheet
  addStyle(wb, sheet = 2, df_all_cell_style, rows = 1:200, cols = 1:dict_len_names, 
           gridExpand = TRUE)
  addStyle(wb, sheet = 2, template_style, rows = 1:200, cols = 1:dict_len_names,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 2, df_name_style, rows = 1, cols = 1:dict_len_names, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 2, df1_style, rows = 1:2, cols = 1:4,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 2, df2_style, rows = 1:2, cols = 5:8,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 2, blank_style, rows = 3:200, cols = 6:8,
           gridExpand = TRUE, stack = TRUE)
  
  freezePane(wb, sheet = 2, firstRow = TRUE)
  
  file_name <- glue("{dr}{mech_id[2]}_{mech_id[4]}_ER_template.xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
  
  return(df_with_cost)
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
test_df_with_cost <- gen_df_with_cost(test_df)

test_output <- wb_pipeline(test_mech)



# # Generate Files=========================================================================
# 
# # List of mechanisms
# lst_mech <- 
#   df_fsd %>% 
#   distinct(mech_code) %>% 
#   pull()
# 
# # removes backlash for creation of actual directory
# fisc_dir_name = substr(fisc_dir, 1, nchar(fisc_dir)-1)
# #create output folders folders locally
# dir_create(fisc_dir_name)
# 
# # Generate list from 1 to length of list
# lst_positions <- seq(1, length(lst_mech))
# # Global Progress Tracking variables
# global_progress <- 1
# total_files <- length(lst_mech)
# 
# # Create output budget files
# walk(lst_mech,
#       function(x, y) progress_tracker(global_progress = global_progress, 
#                                       mech_id = x,
#                                       dr = fisc_dir
#                                       ))
# 
# # Length of # of unique mechs matches the number of files created in ER directory
# total_files == length(list.files(fisc_dir))
# 
# 
# 
# # UPLOAD==============================================================================
# #### lines 212-217, 223-234 from Ben Kasdan's code "pull partner data.R"
# 
# #create folder for upload
# drive_mkdir(fisc_dir_name,
#             path = as_id(glbl_id))
# 
# # Get path for 
# drive_ids <- drive_ls(path = as_id(glbl_id))
# drive_fisc_dir <- drive_ids$id[drive_ids$name == fisc_dir_name]
# 
# #identify list of   
# local_files <- list.files(fisc_dir_name, full.names = TRUE)
# 
# #push to drive
# walk(local_files,
#      ~ drive_upload(.x,
#                     path = as_id(drive_fisc_dir), #path is to the ER21 test file folder
#                     name = basename(.x),
#                     type = "spreadsheet"))
# 
# #remove all local files
# unlink(fisc_dir_name, recursive = TRUE)
