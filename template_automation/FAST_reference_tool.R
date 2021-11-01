## version 2.3: turn summary sheet generator into a function

# LOCALS & SETUP ===============================================================
## IMPORTANT NOTE: set working directory to Source File for best results
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(openxlsx)
library(glamr)
library(gophr)
library(googlesheets4)
library(glue)
library(googledrive)
library(janitor)
library(tidyverse)
library(fs) #to create folders

######## GLOBAL VARIABLES ======================================================
# Global var for cols
curr_fiscal_year <- 2022
cop_year <- substr(as.character(curr_fiscal_year), 3, 4)

# Drive path is to Partner_data_output folder. Change path name to select correct upload path
glbl_id <- '10MUro75F5psphARCr0-0pnqpCMNswEIB'
# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
fisc_dir = "COP22_FAST_reference_tools/"

id_cols <-c('fundingagency', 'mech_name', 'primepartner', 'mech_code',
            'program_sub_program', 'beneficiary_sub_beneficiary')
data_cols <- c('expenditure_amt','workplan_budget_amt', 'cop_budget_total', 
               'fiscal_year')    
lst_er <- c("expenditure_amt", "workplan_budget_amt", "cop_budget_total")

# row names for program area breakdowns
pa_rows <- data.frame(c("C&T", "HTS", "PREV", "SE", 'ASP', 'PM', 'TOTAL'))
paStartRow <- 4
paStartCol <- 14
num_paRows <- count(pa_rows)[[1]]
dataRowStart <- 4
# max substring length for sheet name
max_substr_len <- 30 
# Row start for Summary dataframe on summary tab
sumRowStart <- 3


# column names
col_lst_names <- c("Funding Agency", "Mechanism Name", "Partner Name", "Mechanism ID",
                   "Program Area", "Beneficiary", 
                   glue("COP{(curr_fiscal_year - 3)} Expenditures"),
                   glue("COP{(curr_fiscal_year - 2)} Workplan Budget"),
                   glue("COP{(curr_fiscal_year - 1)} Budget"),
                   "Incremental Budget Change (%)",
                   "Incremental Budget Change ($)",
                   glue("COP{curr_fiscal_year} Intervention Budget Total"))
col_len_names <- length(col_lst_names)
matrix_col_names <- matrix(col_lst_names, 1, col_len_names)

# Set cell styles
blue_fill <- createStyle(fgFill = "#4472C4")
gold_fill <- createStyle(fgFill = "#BF8F00")
yellow_hilite <- createStyle(fgFill = "#FFFF00")
green_fill <- createStyle(fgFill = "#A9D08E")

header_txt <- createStyle(fontColour = "#FFFFFF", textDecoration = "bold")
grey_cells <- createStyle(fontColour = "#FFFFFF", fgFill = '#808080')
bold_txt <- createStyle(textDecoration = 'bold')
border_cells <- createStyle(border = "TopBottomLeftRight", borderStyle = 'thin')
thick_line <- createStyle(border = "left", borderStyle = "thick")
dollar_cell <- createStyle(numFmt = "CURRENCY") 
percent_cell <- createStyle(numFmt = "PERCENTAGE") 
wrap_txt <- createStyle(wrapText = TRUE)


# Functions ====================================================================
## Generate data.frame with cost category, for Work Plan Budget and Expenditure
## Input: data.frame with only ONE unique mechanism_id
concat_df <- function(df){
  # Concat columns
  df <- df %>%
    add_column("program_sub_program" = glue("{.$program}: {.$sub_program}"),
               .before = "program") %>%
    add_column("beneficiary_sub_beneficiary" = glue("{.$beneficiary}: {.$sub_beneficiary}"),
               .before = "beneficiary")
} 

# Create base dataframe for reference for 2 outputted dataframes
# Input: string name of OU
gen_df <- function(OU, df = df_fsd){
  df <- filter(df, df$operatingunit == OU) %>%
    concat_df() %>%
    group_by(fundingagency,mech_name, primepartner, mech_code,
             fiscal_year, program_sub_program, beneficiary_sub_beneficiary) %>% 
    summarise_at(vars(cop_budget_total, workplan_budget_amt, expenditure_amt), 
                 sum, na.rm = TRUE) %>%
    ungroup() %>%
    select(c(all_of(id_cols), all_of(data_cols)))
}

# Function to isolate correct budget column for the correct fiscal year
isolate_col <- function(col_name, fisc_yr, df){
  filter(df, df$fiscal_year == curr_fiscal_year - fisc_yr) %>%
    select(c(all_of(id_cols), all_of(col_name)))
}

# Creates merged dataframe with expenditure, workplan, and COP budget data
gen_merged <- function(df){
  # Initialize empty dataframe to start merge
  df_merged <- df[0,id_cols]
  for (i in 1:length(lst_er)){
    df_merged <- 
      # Note: We use 3 since 3 - index_position equals the X years ago's data to use
      isolate_col(lst_er[[i]], 3 - i, df) %>%
      full_join(df_merged, ., by = id_cols) 
  }
  # Arrange by program_sub_program and drop program
  df_merged <- df_merged %>% arrange(program_sub_program, beneficiary_sub_beneficiary)
}

# Creates dataframe with total $ for expenditure, workplan, and COP budget
gen_totals <- function(df){
  df_totals <- df %>%
    select(all_of(lst_er)) %>%
    summarise(across(everything(), .f = sum, na.rm = TRUE))
}

# Pipeline that takes in OU-level dataframe and spits out mechanism-level tables
mech_pipeline <- function(df, mech_name){
  # dyplr's filter handles strings poorly within functions, see documentation here:
  ### https://stackoverflow.com/questions/31118705/how-to-filter-a-column-by-multiple-flexible-criteria/45265617#45265617
  mech_name = enquo(mech_name) # Makes mech_name into a quosure
  df_mech <- df %>%
    filter(.$mech_name == UQ(mech_name)) %>%
    gen_merged()
  
  df_totals <- gen_totals(df_mech)
  return(list(df_mech = df_mech, df_totals = df_totals))
}

# Abridge a string so that it fits in Excel's sheet name
abridge <- function(strng, max_len){
  len_str <- nchar(strng)
  if (len_str > max_len){
    start <- substr(strng, 1, 23)
    end <- substr(strng, len_str-4, len_str)
    return(glue("{start}...{end}"))
  } else {
    return(strng)
  }
}

#### Create Sheet
gen_sheet <- function(wb, lst_df, mech_name, sheet_name, sheet_num){
  len_df <- count(lst_df$df_mech)[[1]]
  
  addWorksheet(wb, sheetName = sheet_name)
  
  #########  Write dataframes with FSD data  ######### 
  writeData(wb, sheet = sheet_num, x = lst_df$df_totals, startRow = 2, startCol = 7,
            colNames = FALSE, withFilter = FALSE)
  writeData(wb, sheet = sheet_num, x = lst_df$df_mech, startRow = 3)
  #########  Write in Program Area row names (right side small table)  ######### 
  writeData(wb, sheet = sheet_num, x = pa_rows, startCol = paStartCol, 
            startRow = paStartRow, colNames = FALSE,
            withFilter = FALSE)
  
  ######### Write in formulas for calculations  ######### 
  for (i in dataRowStart:(dataRowStart+len_df-1)){
    writeFormula(wb, sheet = sheet_num, x = glue("=(IFERROR(J{i}*I{i},0))"), 
                 startRow = i, startCol = 11)
    writeFormula(wb, sheet = sheet_num, x = glue("=(IFERROR(K{i}+I{i},0))"),
                 startRow = i, startCol = 12)
  }
  writeFormula(wb, sheet = sheet_num, x = "SUM(K4:K300)", startCol = 11, startRow = 2)
  writeFormula(wb, sheet = sheet_num, x = "SUM(L4:L300)", startCol = 12, startRow = 2)
  for (i in (1:(num_paRows-1))){
    writeFormula(wb, sheet = sheet_num, x = glue('=SUMIF($E$4:$E$5000, "{pa_rows[[i,1]]}*", $L$4:$L$400)'),
                 startCol = (paStartCol+1), startRow = paStartRow+i-1)
  }
  # hard-coded locations for summing program area budgets
  writeFormula(wb, sheet = sheet_num, 
               x = glue("=SUM(O{paStartRow}:O{(paStartRow+num_paRows-2)})"),
               startCol = (paStartCol+1), startRow = (paStartRow+num_paRows-1))
  
  ######### Insert headers  ######### 
  writeData(wb, sheet = sheet_num, x = "Totals", startCol = 7, startRow = 1)
  writeData(wb, sheet = sheet_num, x = "TOTAL BUDGET", startCol = 10, startRow = 1)
  writeData(wb, sheet = sheet_num, x = matrix_col_names, startRow = 3, colNames = FALSE, 
            withFilter = FALSE)
  
  ######### Add Style ######### 
  addStyle(wb, sheet = sheet_num, blue_fill, rows = 1, cols = 1:12, gridExpand = TRUE)
  addStyle(wb, sheet = sheet_num, gold_fill, rows = 2, cols = 1:12, gridExpand = TRUE)
  addStyle(wb, sheet = sheet_num, yellow_hilite, rows = dataRowStart:(dataRowStart+len_df-1),
           cols = 10)
  addStyle(wb, sheet = sheet_num, percent_cell, rows = dataRowStart:(dataRowStart+len_df-1),
           cols = 10, stack = TRUE)
  addStyle(wb, sheet = sheet_num, green_fill, rows = dataRowStart:(dataRowStart+len_df-1),
           cols = 12)
  
  addStyle(wb, sheet = sheet_num, header_txt, rows = 1:2, cols = 1:12, gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb, sheet = sheet_num, wrap_txt, rows = 3, cols = 1:12, gridExpand = TRUE)
  addStyle(wb, sheet = sheet_num, grey_cells, rows = dataRowStart:(3 + len_df), 
           cols = c(1:9, 11), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = dataRowStart:(3 + len_df), 
           cols = c(7:9, 11:12), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = 2,cols = 7:12, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = paStartRow:(paStartRow+num_paRows), 
           cols = 15, gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet = sheet_num, bold_txt, rows = 3, cols = 10:12, gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb, sheet = sheet_num, bold_txt, rows = (paStartRow+num_paRows-1), cols = 14:15, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, border_cells, rows = 1:(3+len_df), cols = 1:12, gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb, sheet = sheet_num, border_cells, rows = paStartRow:(paStartRow+num_paRows-1), 
           cols = 14:15, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, thick_line, rows = 1:(3+len_df), cols = 7, stack = TRUE)
  addStyle(wb, sheet = sheet_num, thick_line, rows = 1:(3+len_df), cols = 10, stack = TRUE)
  
  
  mergeCells(wb, sheet = sheet_num, cols = 7:9, rows = 1)
  mergeCells(wb, sheet = sheet_num, cols = 10:12, rows = 1)
  
  setColWidths(wb, sheet = sheet_num, cols = c(2, 4, 5, 6, 13), widths = c(15, 10, 15, 30, 2))
  setColWidths(wb, sheet = sheet_num, cols = c(7:12, 15), widths = 14)
  
  return(wb)
}

## Create workbook for a single OU
gen_wb <- function(OU_name, dr){
  df <- gen_df(OU_name)
  
  mech_code_df <- df %>% group_by(mech_name) %>% summarise(mech_code = first(mech_code))
  lst_mech <- mech_code_df$mech_name
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = glue("COP{cop_year} Budget Summary"))
  
  sheet_count <- 2
  
  sheet_names <- list()
  mech_lst_short <- list()
  
  #for (i in 1:length(lst_mech)){
  for (mech in lst_mech){
    lst_data <- mech_pipeline(df, mech)
    
    # Only build sheet if there is data
    if (dim(lst_data$df_mech)[1] != 0){
      # Not most elegant solution, but will use these lists for now
      mech_lst_short <- c(mech_lst_short, mech)
      
      mech_mod <- mech %>%
        str_replace_all(., c("\\[" = "", "\\]" = "", '\\/' = "", 
                                          "'" = " ")) %>%
        abridge(., max_len = max_substr_len)
        
      
      
      wb <- gen_sheet(wb, lst_data, mech_mod, mech_mod, sheet_count)
      sheet_count <- sheet_count + 1
      
      # Not most elegant solution, but will use these lists for now
      sheet_names <- append(sheet_names, mech_mod)#, substr(mech_mod, 1, max_substr_len))
    }
  }
  
  # Filter to only select IMs with actual data
  mech_code_df_short <- mech_code_df[mech_code_df$mech_name %in% mech_lst_short, ] %>%
    select(mech_code, mech_name)
  
  num_mech <- length(mech_lst_short)
  sumRowEnd <- sumRowStart + num_mech -1
  
  # Fill in summary sheet
  writeData(wb, sheet = 1, x = "Total IM Budget", startRow = 2)
  writeData(wb, sheet = 1, x = mech_code_df_short,
            startRow = 3,colNames = FALSE, withFilter = FALSE)
  
  # only way to reference sheet easily is by sheet_name, so need to create new external
  # variable that lists all the sheet names
  # NOTE: Code can be improved by having sheet names only generated once, then
  #       assign sheet names from this list. 
  
  im_summary <- sheet_names %>%
    lapply(function(x) glue("='{x}'!L2"))
  
  for (i in 1:num_mech){
    writeFormula(wb, sheet = 1, x = im_summary[[i]], 
                 startRow = (i+sumRowStart-1), startCol = 3)
  }
  
  #########  Write in Program Area row names (right side small table)  ######### 
  writeData(wb, sheet = 1, x = "Total Budget by PA", startCol = 5, 
            startRow = sumRowStart-1)
  writeData(wb, sheet = 1, x = pa_rows, startCol = 5, startRow = sumRowStart, 
            colNames = FALSE, withFilter = FALSE)
  for (i in (1:(num_paRows-1))){
    pa_formula <- "="
    for (nm in sheet_names){
      pa_formula <- glue("{pa_formula}'{nm}'!O{paStartRow+i-1}+")
    }
    pa_formula <- substr(pa_formula, 1, nchar(pa_formula)-1)
    
    writeFormula(wb, sheet = 1, x = pa_formula,
                 startCol = 6, startRow = sumRowStart+i-1)
  }
  # hard coded summing for now. Can adjust later to be more dynamic
  writeFormula(wb, sheet = 1, x = "=SUM(F3:F8)", startCol = 6, 
               startRow = sumRowStart+num_paRows-1)
  
  # sum up total IM budget for OU
  writeFormula(wb, sheet = 1, x = glue("=SUM(C{sumRowStart}:C{sumRowEnd})"),
               startCol = 3, startRow = sumRowEnd+1)
  writeData(wb, sheet = 1, x = "TOTAL", startCol = 2, startRow = sumRowEnd+1)
  
  ##### Style summary sheet
  addStyle(wb, sheet = 1, bold_txt, rows = 2, cols = 1:6, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, bold_txt, rows = 9, cols = 5:6, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, bold_txt, rows = sumRowEnd+1, cols = 2:3, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, border_cells, rows = sumRowStart:sumRowEnd, 
           cols = 1:3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, border_cells, rows = sumRowStart:(sumRowStart+num_paRows-2), 
           cols = 5:6, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, dollar_cell, rows = sumRowStart:(sumRowEnd+1), cols = 3,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = 1, dollar_cell, rows = sumRowStart:(sumRowStart+num_paRows), 
           cols = 6, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet = 1, cols = c(2, 3,6), widths = c(35, 14, 14))
  
  
  file_name <- glue("{dr}{OU_name}_FAST_reference.xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
}

# Progress tracker is a wrapper around the wb_pipeline function to track
# progress on the building of files
progress_tracker <- function(global_progress, ou_id, dr){
  if (global_progress == 1){
    cat("Starting building files\n")
  }
  
  if (global_progress %% 1 == 0){
    cat("Building", global_progress, "out of", total_files, "files\n", sep = " ")
  }
  
  ### Can change function used here. Better to have chosen function be an
  ### argument of the Progress Tracker function, but that is annoying to code right now
  gen_wb(ou_id, dr)
  
  global_progress <<- global_progress + 1
  
  if (global_progress == total_files){
    cat("Finished building all xlsx files.\n")
    global_progress <<- 1
  }
}
# Read in File==================================================================

# si_path is a function in glamr that outputs saved string path to local Data
glamr::si_path()
#<- 'Financial_Structured_Datasets_COP17-21_20210917.txt'

# Run glamr function "si_paths" to generate local paths based on organization's
# common folder structure for data stored locally.
# Then call latest file, read in data.frame and filter to select only USAID entries
df_fsd <- si_path() %>%
  return_latest("Fin") %>% 
  gophr::read_msd() %>% 
  filter(fundingagency == "USAID") %>%
  remove_mo()

# Align labels "Program Management" and "IM Program Management"
df_fsd$sub_program <- replace(df_fsd$sub_program, df_fsd$sub_program == "Program Management", "IM Program Management")

# Create Directory =============================================================
# removes backlash for creation of actual directory
fisc_dir_name = substr(fisc_dir, 1, nchar(fisc_dir)-1)
#create output folders folders locally
dir_create(fisc_dir_name)

# # Tests ========================================================================
# # test
# test_OU <- "Cote d'Ivoire"
# gen_wb(test_OU, fisc_dir)


# Walk =========================================================================
# List of OUs
lst_ou <- 
  df_fsd %>% 
  distinct(operatingunit) %>% 
  pull()

# SHORTEN THE LIST JUST FOR SAKE FO DEMO RUN TIME
lst_ou <- lst_ou[1:6]

# Global Progress Tracking variables
global_progress <- 1
total_files <- length(lst_ou)

# Create output budget files
walk(lst_ou,
     function(x, y) progress_tracker(global_progress = global_progress,
                                     ou_id = x,
                                     dr = fisc_dir
     ))

# Length of # of unique mechs matches the number of files created in ER directory
total_files == length(list.files(fisc_dir))


# # UPLOAD==============================================================================
# load_secrets()
# #### lines 212-217, 223-234 from Ben Kasdan's code "pull partner data.R"
# #create folder for upload
# drive_mkdir(fisc_dir_name,
#             path = as_id(glbl_id))
# 
# # Get path for created directory
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

