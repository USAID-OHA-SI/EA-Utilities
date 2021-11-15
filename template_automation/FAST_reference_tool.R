## version 3-2-3:
## New Features in 3-2-3 include:
##            * Change order of COP Budget and Workplan Budget
##               - to revert back, change Lines 65, 66, and 146
##            * Control openxlsx version used to 4.2.3
##              Latest version (4.2.4) does not preserve imported styling

# LOCALS & SETUP ===============================================================
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

library(openxlsx)
library(glamr)
library(gophr)
library(googlesheets4)
library(glue)
library(googledrive)
library(janitor)
library(tidyverse)
library(fs) #to create folders

source('C:/Users/davidsong/Desktop/USAID/GitHub/EA-Utilities/upload_dir_to_gdrive.R')

######## GLOBAL VARIABLES ======================================================
# Global var for cols
curr_fiscal_year <- 2022
cop_year <- substr(as.character(curr_fiscal_year), 3, 4)

# Drive path is to Partner_data_output folder. Change path name to select correct upload path
glbl_id <- '1OaOe8W0akNSIbtk68w1kXc20GNRzOzew'
# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
fisc_dir = "COP22_FAST_reference_tools/"

id_cols <-c('fundingagency', 'mech_name', 'primepartner', 'mech_code',
            'program_sub_program', 'beneficiary_sub_beneficiary')
data_cols <- c('expenditure_amt', 'workplan_budget_amt','cop_budget_total',
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
sumRowStart <- 5

fast_ref_template <- "fast_reference_template_v3.xlsx"
summary_sheet_num <- 2
ref_sheet_num <- summary_sheet_num + 1

# This is the default budget used for calculations
default_budget_option <- "Workplan Budget"

# column names
col_lst_names <- c("Funding Agency", "Mechanism Name", "Partner Name", "Mechanism ID",
                   "Program Area", "Beneficiary", 
                   glue("COP{(curr_fiscal_year - 3)} Expenditures"),
                   glue("COP{(curr_fiscal_year - 1)} Budget"),
                   glue("COP{(curr_fiscal_year - 2)} Workplan Budget"),
                   "Incremental Budget Change (%)",
                   "Incremental Budget Change ($)",
                   glue("COP{curr_fiscal_year} Intervention Budget Total"))
col_len_names <- length(col_lst_names)
matrix_col_names <- matrix(col_lst_names, 1, col_len_names)

# Set cell styles
yellow_hilite <- createStyle(fgFill = "#FFFF00")
green_fill <- createStyle(fgFill = "#A9D08E")

grey_cells <- createStyle(fontColour = "#FFFFFF", fgFill = '#808080')
bold_txt <- createStyle(textDecoration = 'bold')
border_cells <- createStyle(border = "TopBottomLeftRight", borderStyle = 'thin')
thick_line <- createStyle(border = "left", borderStyle = "thick")
dollar_cell <- createStyle(numFmt = "CURRENCY") 
percent_cell <- createStyle(numFmt = "PERCENTAGE") 
wrap_txt <- createStyle(wrapText = TRUE)
light_grey <- createStyle(fgFill = '#F2F2F2')

# Functions ====================================================================
## Generate data.frame with cost category, for Work Plan Budget and Expenditure

## Input: data.frame with only ONE unique OU
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

# Switches column order of workplan and COP budgets
new_col_order <- c("expenditure_amt", "cop_budget_total", "workplan_budget_amt")
switch_work_and_cop <- function(df){
  df %>% select(c(all_of(id_cols), all_of(new_col_order)))
}

# Pipeline that takes in OU-level dataframe and spits out mechanism-level tables
mech_pipeline <- function(df, mech_name){
  # dyplr's filter handles strings poorly within functions, see documentation here:
  ### https://stackoverflow.com/questions/31118705/how-to-filter-a-column-by-multiple-flexible-criteria/45265617#45265617
  mech_name = enquo(mech_name) # Makes mech_name into a quosure
  df_mech <- df %>%
    filter(.$mech_name == UQ(mech_name)) %>%
    gen_merged() %>%
    switch_work_and_cop()
  return(df_mech)
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
gen_sheet <- function(wb, df_mech, mech_mod, sheet_num){
  len_df <- count(df_mech)[[1]]
  
  df_mech_id <- df_mech[1, 1:4]
  df_extra_rows <- df_mech_id[rep(seq_len(nrow(df_mech_id)), each = 5), ]
  
  # always clone the reference sheet 
  cloneWorksheet(wb, mech_mod, clonedSheet = names(wb)[[ref_sheet_num]])
  
  #########  Write dataframes with FSD data  ######### 
  writeData(wb, sheet = sheet_num, x = df_mech, startRow = 4, colNames = F, withFilter = F)
  writeData(wb, sheet = sheet_num, x = df_extra_rows, startRow = 4+len_df, 
            colNames = F, withFilter = F)
  
  ######### Write in formulas for calculations  ######### 
   for (i in dataRowStart:(dataRowStart+len_df-1)){
    writeFormula(wb, sheet = sheet_num,
                 x = glue('=IF(R2="Workplan Budget",IFERROR(I{i}*J{i},0),IFERROR(H{i}*J{i},0))'),
                 startRow = i, startCol = 11)
    writeFormula(wb, sheet = sheet_num, x = glue("=(IFERROR(K{i}+I{i},0))"),
                 startRow = i, startCol = 12)
  }
  for (i in (1:(num_paRows-1))){
    writeFormula(wb, sheet = sheet_num, x = glue('=SUMIF($E$4:$E$5000, "{pa_rows[[i,1]]}*", $L$4:$L$400)'),
                 startCol = (paStartCol+1), startRow = paStartRow+i-1)
  }
  # hard-coded locations for summing program area budgets
  writeFormula(wb, sheet = sheet_num, 
               x = glue("=SUM(O{paStartRow}:O{(paStartRow+num_paRows-2)})"),
               startCol = (paStartCol+1), startRow = (paStartRow+num_paRows-1))
  
  ######### Insert headers  ######### 
  writeData(wb, sheet = sheet_num, x = matrix_col_names, startRow = 3, colNames = FALSE, 
            withFilter = FALSE)
  
  # Insert Drop-down menu. Note that openxlsx cannot handle hiding sheets, so
  # hide the drop-down options on the Guidance page
  df_options <- data.frame(list(list("Option:"), list(default_budget_option)))
  writeData(wb, sheet_num, x = df_options, startCol = 17, startRow = 2,
            colNames=F, withFilter=F)
  dataValidation(wb, sheet_num, col = 18, rows=2, type = "list",
                 value = "'Guidance'!$CA$1:$CA$2")
  
#  writeFormula(wb, sheet_num, x="=IF(Q2=='COP Budget', 100, 2", startCol=14, startRow=4)
  
  ######### Add Style ######### 
  addStyle(wb, sheet = sheet_num, yellow_hilite, rows = dataRowStart:(dataRowStart+len_df-1),
           cols = 10)
  addStyle(wb, sheet = sheet_num, percent_cell, rows = dataRowStart:(dataRowStart+len_df-1),
           cols = 10, stack = TRUE)
  addStyle(wb, sheet = sheet_num, green_fill, rows = dataRowStart:(dataRowStart+len_df+4),
           cols = 12)
  addStyle(wb, sheet = sheet_num, grey_cells, rows = dataRowStart:(3 + len_df), 
           cols = c(1:9, 11), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = dataRowStart:(3 + len_df), 
           cols = c(7:9, 11:12), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = paStartRow:(paStartRow+num_paRows), 
           cols = 15, gridExpand = TRUE, stack = TRUE)
  
  addStyle(wb, sheet = sheet_num, border_cells, rows = 1:(3+len_df), cols = 1:12, gridExpand = TRUE,
           stack = TRUE)
  addStyle(wb, sheet = sheet_num, thick_line, rows = 1:(3+len_df), cols = 7, stack = TRUE)
  addStyle(wb, sheet = sheet_num, thick_line, rows = 1:(3+len_df), cols = 10, stack = TRUE)
  
  addStyle(wb, sheet = sheet_num, border_cells, rows = (4+len_df):(8+len_df), cols = c(1:6,12), 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, grey_cells, rows = (4+len_df):(8+len_df), cols = c(1:4), 
           gridExpand = TRUE, stack = TRUE)
  return(wb)
}

## Create workbook for a single OU
gen_wb <- function(OU_name, dr){
  df <- gen_df(OU_name)
  
  mech_code_df <- df %>% group_by(mech_name) %>% summarise(mech_code = first(mech_code))
  lst_mech <- mech_code_df$mech_name
  
  wb <- loadWorkbook(fast_ref_template)
  names(wb)[[2]] <- glue("COP{cop_year} Budget Summary")
  
  sheet_count <- ref_sheet_num + 1
  
  sheet_names <- list()
  mech_lst_short <- list()
  
  #for (i in 1:length(lst_mech)){
  for (mech in lst_mech){
    df_mech <- mech_pipeline(df, mech)
    
    # Only build sheet if there is data. If 0 rows, do not build mech
    if (dim(df_mech)[1] != 0){
      # Not most elegant solution, but will use these lists for now
      mech_lst_short <- c(mech_lst_short, mech)
      
      mech_mod <- mech %>%
        str_replace_all(., c("\\[" = "", "\\]" = "", '\\/' = "", 
                                          "'" = " ")) %>%
        abridge(., max_len = max_substr_len)
      
      wb <- gen_sheet(wb, df_mech, mech_mod,sheet_count)
      sheet_count <- sheet_count + 1
      
      # Not most elegant solution, but will use these lists for now
      sheet_names <- append(sheet_names, mech_mod)
    }
  }
  
  # Filter to only select IMs with actual data
  mech_code_df_short <- mech_code_df[mech_code_df$mech_name %in% mech_lst_short, ] %>%
    select(mech_code, mech_name)
  
  num_mech <- length(mech_lst_short)
  sumRowEnd <- sumRowStart + num_mech -1
  
  # Fill in summary sheet
  writeData(wb, sheet = summary_sheet_num, x = mech_code_df_short,
            startRow = sumRowStart,colNames = FALSE, withFilter = FALSE)
  
  # only way to reference sheet easily is by sheet_name, so need to create new external
  # variable that lists all the sheet names
  # NOTE: Code can be improved by having sheet names only generated once, then
  #       assign sheet names from this list. 
  im_summary <- sheet_names %>%
    lapply(function(x) glue("='{x}'!L2"))
  
  for (i in 1:num_mech){
    writeFormula(wb, sheet = summary_sheet_num, x = im_summary[[i]], 
                 startRow = (i+sumRowStart-1), startCol = 3)
  }
  
  #########  Write in Program Area row names (right side small table)  ######### 
  for (i in (1:(num_paRows-1))){
    pa_formula <- "="
    for (nm in sheet_names){
      pa_formula <- glue("{pa_formula}'{nm}'!O{paStartRow+i-1}+")
    }
    pa_formula <- substr(pa_formula, 1, nchar(pa_formula)-1)
    
    writeFormula(wb, sheet = summary_sheet_num, x = pa_formula,
                 startCol = 6, startRow = sumRowStart+i-1)
  }
  # hard coded summing for now. Can adjust later to be more dynamic
  writeFormula(wb, sheet = summary_sheet_num, x = "=SUM(F5:F10)", startCol = 6, 
               startRow = sumRowStart+num_paRows-1)
  
  # sum up total IM budget for OU
  writeFormula(wb, sheet = summary_sheet_num, x = glue("=SUM(C{sumRowStart}:C{sumRowEnd})"),
               startCol = 3, startRow = sumRowEnd+1)
  writeData(wb, sheet = summary_sheet_num, x = "TOTAL", startCol = 2, startRow = sumRowEnd+1)
  
  
  ##### Style summary sheet
  addStyle(wb, sheet = summary_sheet_num, bold_txt, rows = sumRowEnd+1, cols = 2:3, 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = summary_sheet_num, light_grey, rows = sumRowEnd+1, cols=2:3,
           gridExpand=T, stack=T)
  addStyle(wb, sheet = summary_sheet_num, border_cells, rows = sumRowEnd+1, cols=2:3,
           gridExpand=T, stack=T)
  addStyle(wb, sheet = summary_sheet_num, border_cells, rows = sumRowStart:sumRowEnd, 
           cols = 1:3, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = summary_sheet_num, border_cells, rows = sumRowStart:(sumRowStart+num_paRows-2), 
           cols = 5:6, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = summary_sheet_num, dollar_cell, rows = sumRowStart:(sumRowEnd+1), cols = 3,
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = summary_sheet_num, dollar_cell, rows = sumRowStart:(sumRowStart+num_paRows), 
           cols = 6, gridExpand = TRUE, stack = TRUE)

  
  # Remove reference sheet
  removeWorksheet(wb, ref_sheet_num)
  
  # Add OU name and Fiscal year to Guidance page
  writeData(wb, 1, x = OU_name, startCol = 2, startRow = 3)
  writeData(wb, 1, x = curr_fiscal_year, startCol = 2, startRow = 4)
  
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


# UPLOAD==============================================================================
load_secrets()

upload_dir_to_gdrive(fisc_dir, glbl_id)



# #remove all local files
# unlink(fisc_dir_name, recursive = TRUE)

