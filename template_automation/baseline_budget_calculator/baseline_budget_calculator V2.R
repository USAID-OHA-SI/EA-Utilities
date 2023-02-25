## version 4-4:
## Author: David Song
## Date: 2022 March 17
## New Features in 4-4 include:
##            * Fixing comments for accuracy/clarity
##            * Adjusted fisc_dir input to no longer require backslash
##            * Fixed table header bug for program area to match correct COP year
##            * Added dynamic version of progress tracker

# LOCALS & SETUP ===============================================================
## IMPORTANT NOTE: set working directory to Source File for best results
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
### If-statement checks to see if openxlsx is installed and has the correct version
### Note: Must use openxlsx version 4.2.3--latest version does not import workbook 
###       formatting correctly (as of 4.2.4), see below URL
###       https://github.com/ycphs/openxlsx/issues/207
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

# Set path to where the where GitHub folder can be found
git_dir <- "C:/Users/jmontespenaloza/Documents"
# Use google drive uploader function
source(glue('{git_dir}/GitHub/EA-Utilities/upload_dir_to_gdrive.R'))

######## GLOBAL VARIABLES ======================================================
##### USER ADJUSTABLE GLOBAL VARIABLES
# Input Current COP Year for tool (COP22)
curr_cop_year <- 2023

## Path Names
# Drive path is to Partner_data_output folder. Change path name to select correct upload path
glbl_id <- '17tyyeuPorZIiW-v40og95EGKRXw8rQCY' 

# Directory name (used both on local system and in Google Drive, see "Generate File" section of code)
### Note: Best to hard code full path, e.g. "C:/Users/davidsong/Documents/desired_folder_name"
fisc_dir = "COP22_baseline_budget_tools"

# Path to the FAST tool reference template file. Normally in the working directory, but adjust path as needed
fast_ref_template <- "fast_reference_template_v4-2.xlsx"


#================================================================================
###### USER SHOULD NOT ADJUST THESE GLOBAL VARIABLES UNLESS THEY KNOW CODE
# Last two characters of the year, e.g. "21", "22", "23"
cop_year <- substr(as.character(curr_cop_year), 3, 4)

### Dataframe global variables
# ID columns make up a unique combination for a row in the Tool's data frame
id_cols <-c('funding_agency', 'mech_name', 'prime_partner_name', 'mech_code',
            'program_sub_program', 'beneficiary_sub_beneficiary')
# Column names for the actual financial data that we want to pull into the tool (budgets, expenditures)
data_cols <- c('expenditure_amt', 'workplan_budget_amt','cop_budget_total',
               'fiscal_year')    

### Vars for Formatting Excel Sheets
# Row names for program area breakdowns. We will for-loop through this dataframe to automate formulas later
pa_rows <- data.frame(c("C&T", "HTS", "PREV", "SE", 'ASP', 'PM', 'TOTAL'))
# The column in Excel where the Program Area Sub-totals will start on each mechanism's sheet in Excel
paStartCol <- 14
# Number of rows that the Program Area Subtotals cover, for setting formatting later
num_paRows <- count(pa_rows)[[1]]
# The row number where we insert our mechanism's data into the Excel sheet
dataRowStart <- 6
# max substring length for sheet name, for abriding mech names to fit in Excel tabs
max_substr_len <- 30 
# Row start for Summary dataframe on summary tab
sumRowStart <- 5

# Location of the summary sheet and the reference template sheet in the reference Excel file
summary_sheet_num <- 2
ref_sheet_num <- summary_sheet_num + 1

# This is the default budget used for calculations
default_budget_option <- "Workplan Budget"

# column names for the mech dataframe
col_lst_names <- c("Funding Agency", "Mechanism Name", "Partner Name", "Mechanism ID",
                   "Program Area", "Beneficiary", 
              ## We subtract 2000 to get the base year number
                   glue("COP{(curr_cop_year - 2 -2000)} Expenditures"),
                   glue("COP{(curr_cop_year - 1 -2000)} FAST Budget"),
                   glue("COP{(curr_cop_year - 1 -2000)} Workplan Budget"),
                   "Incremental Budget Change (%)",
                   "Incremental Budget Change ($)",
                   glue("COP{curr_cop_year -2000} Intervention Budget Total"))
# Save columns as a "row" rather than a "column" for easier insertion into Excel sheet
col_len_names <- length(col_lst_names)
matrix_col_names <- matrix(col_lst_names, 1, col_len_names)

### Create cell styles for later use
###        See: https://ycphs.github.io/openxlsx/reference/createStyle.html
# Colors
yellow_hilite <- createStyle(fgFill = "#FFFF00")
green_fill <- createStyle(fgFill = "#A9D08E")
grey_cells <- createStyle(fontColour = "#FFFFFF", fgFill = '#808080')
light_grey <- createStyle(fgFill = '#F2F2F2')
# Text
bold_txt <- createStyle(textDecoration = 'bold')
wrap_txt <- createStyle(wrapText = TRUE)
# Borders
border_cells <- createStyle(border = "TopBottomLeftRight", borderStyle = 'thin')
thick_line <- createStyle(border = "left", borderStyle = "thick")
# Numbers
percent_cell <- createStyle(numFmt = "PERCENTAGE") 
    # Note: These following two use Excel formula format for formatting decimal points
dollar_cell <- createStyle(numFmt = '_($* #,##0_);_($* (#,##0);_($* "-"??_);_(@_)') 
mech_num_cell <- createStyle(numFmt = '_*0')


########  FUNCTIONS ====================================================================
###### DATA MANIPULATING FUNCTIONS 

# Concat_df: Helper function to gen_df
### Role: add concatenated columns for program+sub_probram and beneficiary+sub_beneficiary
### Input: data.frame with only ONE unique OU
### Output: data.frame
concat_df <- function(df){
  # Concat columns
  df <- df %>%
    add_column("program_sub_program" = glue("{.$program}: {.$sub_program}-{.$interaction_type}"),
               .before = "program") %>%
    add_column("beneficiary_sub_beneficiary" = glue("{.$beneficiary}: {.$sub_beneficiary}"),
               .before = "beneficiary")
} 

# Gen_df: Create the base OU-level dataframe for reference for all later outputted  
#         dataframes on summary and mech sheets. Filters by OU.
### Inputs: OU: string name of OU
###         df: data.frame, default set to the FSD dataframe
### Output: data.frame
gen_df <- function(OU, df = df_fsd){
  df <- filter(df, df$operatingunit == OU) %>%
    concat_df() %>%
    group_by(funding_agency,mech_name, prime_partner_name, mech_code,
             fiscal_year, program_sub_program, beneficiary_sub_beneficiary) %>% 
    summarise_at(vars(cop_budget_total, workplan_budget_amt, expenditure_amt), 
                 sum, na.rm = TRUE) %>%
    ungroup() %>%
    # Selec tonly columns of interest: the ID columns and the financial data columns
    select(c(all_of(id_cols), all_of(data_cols)))
}

# Isolate_col: Helper function to gen_merged
### Role: Function to isolate correct budget column for the correct fiscal year
### Input: col_name: Column name to isolate (expenditure, FAST budget, or workplan budget)
###        fisc_yr: Year of data needed for column of interest
###        df: dataframe to use. In this case, a mechanism-level dataframe
### Output: data.frame
isolate_col <- function(col_name, fisc_yr, df){
  filter(df, df$fiscal_year == fisc_yr) %>%
    # Only selects columns for ID and the single financial data column of interest
    select(c(all_of(id_cols), all_of(col_name)))
}

# gen_merged: Helper function to mech_pipeline
## Role: Creates merged dataframe with expenditure, workplan, and COP budget data
##       for the correct fiscal years (expenditure data is always one year behind)
### Input: df: Dataframe. In this case, a mechanism-level dataframe (only 1 mech)
### Output: data.frame
gen_merged <- function(df){
  # Start by building previous year's ER data into dataset
  df_merged <- isolate_col(col_name="expenditure_amt", 
                           fisc_yr = (curr_cop_year - 1), 
                           df=df)
  df_merged <- isolate_col(col_name="cop_budget_total", 
                           fisc_yr = curr_cop_year, 
                           df=df) %>%
    full_join(df_merged, ., by= id_cols)
  df_merged <- isolate_col(col_name="workplan_budget_amt", 
                           fisc_yr = curr_cop_year, 
                           df=df) %>%
    full_join(df_merged, ., by= id_cols)
  
  # Arrange by program_sub_program and drop program
  df_merged <- df_merged %>% arrange(program_sub_program, beneficiary_sub_beneficiary)
}

# mech_pipeline: Pipeline that takes in OU-level dataframe and spits out mechanism-level tables
### Input: df: OU-level data.frame (only one unique OU in the dataframe)
###        mech_name: String. Name of mechanism we want to select from our data
### Output: data.frame, with only 1 unique mechanism
mech_pipeline <- function(df, mech_name){
  # dyplr's filter handles strings poorly within functions, see documentation here:
  ### https://stackoverflow.com/questions/31118705/how-to-filter-a-column-by-multiple-flexible-criteria/45265617#45265617
  # enquo converts mech_name from string into a quosure
  mech_name = enquo(mech_name) 
  df_mech <- df %>%
    # Use UQ function to properly read in the quosure, as filter normally only takes strings
    filter(.$mech_name == UQ(mech_name)) %>%
    # Generate dataframe with correct years for correct financial columns
    gen_merged()
  return(df_mech)
}

# abridge: Abridge a string so that it fits in Excel's sheet name
### Input: strng: A string
###        max_len: integer. Max length of desired string
### Output: 
abridge <- function(strng, max_len){
  len_str <- nchar(strng)
  if (len_str > max_len){
    # selects the first 21 characters
    start <- substr(strng, 1, 19)
    # selects the last 7 characters
    end <- substr(strng, len_str-8, len_str)
    # combines the two above chunks with an ellipse
    return(glue("{start}...{end}"))
  } else {
    # If not larger than max string length, just returns the string as is
    return(strng)
  }
}

###### EXCEL WORKBOOK-CREATING FUNCTIONS

# gen_sheet: Helper function to gen_workbook
## Role: Create one Excel Workbook Sheet
### Inputs: wb: Workbook object. From openxlsx. See: https://ycphs.github.io/openxlsx/articles/Introduction.html
###         df_mech: data.frame; in this case, a single mech's dataframe
###         mech_mod: string. The abridged mech name used for the sheet's tab name in Excel
###         sheet_num: integer. The number corresponding to the current sheet/tab in Excel 
### Output: Workbook object
gen_sheet <- function(wb, df_mech, mech_mod, sheet_num){
  len_df <- count(df_mech)[[1]]
  
  # change from string to numeric to turn off Excel warning on string numbers
  df_mech$mech_code <- as.numeric(df_mech$mech_code)
  
  # Generate 5 extra blank rows with agency, mech name, partner name, and mech code
  df_mech_id <- df_mech[1, 1:4]
  df_extra_rows <- df_mech_id[rep(seq_len(nrow(df_mech_id)), each = 5), ]
  
  # Always clone the reference sheet. https://ycphs.github.io/openxlsx/reference/cloneWorksheet.html
  ### Note: names(wb) is a list of sheet names. We use the index number for the reference sheet to get the string
  print(mech_mod)
  cloneWorksheet(wb, mech_mod, clonedSheet = names(wb)[[ref_sheet_num]])
  
  #########  Write dataframes with FSD data  ######### 
  ### Note: colNames and withFilter must BOTH be false, or openxlsx will create a header row
  writeData(wb, sheet = sheet_num, x = df_mech, startRow = dataRowStart, colNames = F, withFilter = F)
  writeData(wb, sheet = sheet_num, x = df_extra_rows, startRow = dataRowStart+len_df, 
            colNames = F, withFilter = F)
  
  ######### Write in formulas for calculations  ######### 
  ### The logic/explanation: For each row in the mech dataframe, write a formula 
  ###                        using other columns in the same row number
   for (i in dataRowStart:(dataRowStart+len_df-1)){
    writeFormula(wb, sheet = sheet_num,
                 # Use Excel IF statement to switch between Workplan and FAST budget columns
                 x = glue('=IF(B2="Workplan Budget",IFERROR(I{i}*J{i},0),IFERROR(H{i}*J{i},0))'),
                 startRow = i, startCol = 11)
    writeFormula(wb, sheet = sheet_num, x = glue('=IF(B2="Workplan Budget",IFERROR(K{i}+I{i},0),IFERROR(K{i}+H{i},0))'),
                 startRow = i, startCol = 12)
   }
  # This for loop generates formulas for summing by program area for the entire mechanism
  for (i in (1:(num_paRows-1))){
    writeFormula(wb, sheet = sheet_num, x = glue('=SUMIF($E$4:$E$5000, "{pa_rows[[i,1]]}*", $L$4:$L$400)'),
                 startCol = (paStartCol+1), startRow = dataRowStart+i-1)
  }
  # This formual sums the total of all program area budgets
  writeFormula(wb, sheet = sheet_num, 
               x = glue("=SUM(O{dataRowStart}:O{(dataRowStart+num_paRows-2)})"),
               startCol = (paStartCol+1), startRow = (dataRowStart+num_paRows-1))
  
  # Insert headers, i.e. column names
  writeData(wb, sheet = sheet_num, x = matrix_col_names, startRow = dataRowStart-1, colNames = FALSE, 
            withFilter = FALSE)
  writeData(wb, sheet= sheet_num, x = glue("COP{curr_cop_year-2000} Budget"), 
            startRow=5,startCol=15, colNames=F, withFilter=F)
  
  # Role: Insert Drop-down menu. Note that openxlsx cannot handle hiding sheets, so
  #       I hid the drop-down options on the Guidance page
  ## Set the default option to "Workplan budget"
  df_options <- data.frame(list(list("Option:"), list(default_budget_option)))
  writeData(wb, sheet_num, x = df_options, startCol = 1, startRow = 2,
            colNames=F, withFilter=F)
  # Create drop down menus via dataValidation function https://stackoverflow.com/questions/29898269/possible-to-write-excel-formulas-or-data-validation-using-r
  dataValidation(wb, sheet_num, col = 2, rows=2, type = "list",
                 value = "'Guidance'!$CA$1:$CA$2")
  dataValidation(wb, sheet_num, col = 5, 
                 rows=(dataRowStart + len_df):(dataRowStart+len_df+4), 
                 type = "list",
                 value = "'Guidance'!$BY$2:$BY$56")
  dataValidation(wb, sheet_num, col = 6, 
                 rows=(dataRowStart + len_df):(dataRowStart+len_df+4), 
                 type = "list",
                 value = "'Guidance'!$BZ$2:$BZ$28")
  
  
  ######### Add Style ######### 
  ### Note: stack = TRUE ensures that styles stack, rather than overwrite, each other.
  ### Also: set gridExpand = TRUE if style covers a square, rather than a single row/column
  addStyle(wb, sheet = sheet_num, yellow_hilite, rows = dataRowStart:(dataRowStart+len_df-1),
           cols = 10)
  addStyle(wb, sheet = sheet_num, percent_cell, rows = dataRowStart:(dataRowStart+len_df-1),
           cols = 10, stack = TRUE)
  addStyle(wb, sheet = sheet_num, green_fill, rows = dataRowStart:(dataRowStart+len_df+4),
           cols = 12)
  addStyle(wb, sheet = sheet_num, grey_cells, rows = dataRowStart:(dataRowStart + len_df-1), 
           cols = c(1:9, 11), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = dataRowStart:(dataRowStart + len_df-1), 
           cols = c(7:9, 11:12), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = (dataRowStart+len_df):(dataRowStart+len_df+4), 
           cols = 12, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = dataRowStart:(dataRowStart+num_paRows), 
           cols = 15, gridExpand = TRUE, stack = TRUE)
  # Get rid of decimals for row 4, cumulatives
  addStyle(wb, sheet = sheet_num, dollar_cell, rows = 4, cols = 7:12, stack=T)
  
  addStyle(wb, sheet = sheet_num, border_cells, rows = (dataRowStart-1):(dataRowStart + len_df-1), 
           cols = 1:12, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, thick_line, rows = (dataRowStart-3):(dataRowStart + len_df-1), 
           cols = 7, stack = TRUE)
  addStyle(wb, sheet = sheet_num, thick_line, rows = (dataRowStart-3):(dataRowStart + len_df-1), 
           cols = 10, stack = TRUE)
  addStyle(wb, sheet = sheet_num, border_cells, rows = (dataRowStart + len_df):(dataRowStart+len_df+4), cols = c(1:6,12), 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, grey_cells, rows = (dataRowStart + len_df):(dataRowStart+len_df+4), cols = c(1:4), 
           gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet = sheet_num, bold_txt, rows = 12, cols = 15, gridExpand=T, stack=T)
  addStyle(wb, sheet= sheet_num, mech_num_cell, rows = dataRowStart:(dataRowStart+len_df+4), 
           cols = 4, gridExpand=T, stack=T)
  return(wb)
}

# gen_wb: Create workbook for a single OU. "Walk" this function to get workbooks for all OUs
### Inputs: OU_name: string. Name of OU
###         dr: string. Pathway to where you want to save the workbook. 
### Output: Doesn't matter. Key action is this function saves a workbook onto your computer
gen_wb <- function(OU_name, dr=""){
  # Add backslash to directory path dr, if default "empty" quotes are not used
  if (dr != ""){
    dr <- glue("{dr}/")
  }
  
  # Generate OU-level dataframe with helper function
  df <- gen_df(OU_name)
  
  # Save data.frame of mech names with their associated mech codes for the summary sheet
  mech_code_df <- df %>% group_by(mech_name) %>% summarise(mech_code = first(mech_code))
  # Save list of mech names for iterating through all mechs to make all sheets
  lst_mech <- mech_code_df$mech_name
  
  # Load template, based on fast_ref_template path, set earlier in the code
  wb <- loadWorkbook(fast_ref_template)
  # Rename the second sheet to have the correct COP Year in the name
  names(wb)[[2]] <- glue("COP{cop_year} Budget Summary")
  
  # Set initial sheet count to after the reference sheet.
  ### Note: This counter will increase as we add more sheets per each mechanism
  sheet_count <- ref_sheet_num + 1
  
  # Initialize empty lists for the sheet names and mech names. 
  ### Reason: Not all mechs will get a sheet (only if they have data), and sheets will
  ###         use abridged mech names. Therefore, we need to store them to call them
  ###         in the correct order when we write formulas for the summary sheet
  sheet_names <- list()
  mech_lst_short <- list()
  
  # Loop through list of mechanisms to build each mech's sheets
  for (mech in lst_mech){
    # Generate dataframe for the mechanism, with helper function
    df_mech <- mech_pipeline(df, mech)
    
    # Only build sheet if there is data. If 0 rows, do not build mech
    if (dim(df_mech)[1] != 0){
      # Not most elegant solution, but will use these lists for now to save names 
      # of mechanisms that have data.
      mech_lst_short <- c(mech_lst_short, mech)
      
      # Remove special characters that Excel's sheet/tab names do not allow, and abridge
      # to fit in the Excel tab name
      mech_mod <- mech %>%
        gsub("Placeholder - Mechanism ", "Placeholder-", .) %>%
        str_replace_all(., c("\\[" = "", "\\]" = "", '\\/' = "", 
                                          "'" = " ", ":"="")) %>%
        abridge(., max_len = max_substr_len)
      
      # Generates sheet for the mechanism, with helper function
      wb <- gen_sheet(wb, df_mech, mech_mod,sheet_count)
      # Move sheet counter by 1, to tell code to move on to the next sheet
      sheet_count <- sheet_count + 1
      
      # Not most elegant solution, but saves the mech's Excel Sheet name
      sheet_names <- append(sheet_names, mech_mod)
    }
  }
  
  # Filter to only select IMs with actual data. IMs with no data are not in the list
  ### Note: See Method 1 to refresh on base R subsetting: https://www.statology.org/r-select-rows-by-condition/
  mech_code_df_short <- mech_code_df[mech_code_df$mech_name %in% mech_lst_short, ] %>%
    select(mech_code, mech_name)
  # Change from string to numeric to turn off Excel warning on string numbers
  mech_code_df_short$mech_code <- as.numeric(mech_code_df_short$mech_code)
  
  # Role: Calculate where the financial dataframe ends in the Excel workbook, for formatting purposes
  num_mech <- length(mech_lst_short)
  sumRowEnd <- sumRowStart + num_mech -1
  
  # Role: Fill in summary sheet with COP year 
  writeData(wb, sheet=summary_sheet_num, x = glue("COP{curr_cop_year -2000} Budget Summary"), 
            startRow=1, colNames=F, withFilter=F)
  # Role: On the summary sheet, ddd the dataframe with all mech names and mech codes 
  #       in the order of their sheets
  writeData(wb, sheet = summary_sheet_num, x = mech_code_df_short,
            startRow = sumRowStart,colNames = FALSE, withFilter = FALSE)
  
  # Role: Creates list of strings, that are Excel formuals calling data from sheets
  # Note: Only way to reference sheet easily is by sheet_name, so need to create 
  #       new external variable that lists all the sheet names
  # NOTE: Code can be improved by having sheet names only generated once, then
  #       assign sheet names from this list. 
  im_summary <- sheet_names %>%
    lapply(function(x) glue("='{x}'!L4"))
  # For each mechanism, write the formula above in the summary sheet
  for (i in 1:num_mech){
    writeFormula(wb, sheet = summary_sheet_num, x = im_summary[[i]], 
                 startRow = (i+sumRowStart-1), startCol = 3)
  }
  
  #########  Write in Program Area row names (right side small table)  ######### 
  # For each program area, we run another for-loop that loops across all the sheets in the workbook
  for (i in (1:(num_paRows-1))){
    # Initialize string just starting with an equal sign. This is the beginning of the formula string.
    pa_formula <- "="
    # Loop: For each sheet in our list of sheet names, we glue the formula string, sheet name, 
    #       and correct data row. Each loop, the string gets longer. 
    for (nm in sheet_names){
      pa_formula <- glue("{pa_formula}'{nm}'!O{dataRowStart+i-1}+")
    }
    
    # Remove the last "+" sign from the string, so that Excel does not error out the formula
    pa_formula <- substr(pa_formula, 1, nchar(pa_formula)-1)
    
    # Write formula into the spreadsheet for each Program Area row
    writeFormula(wb, sheet = summary_sheet_num, x = pa_formula,
                 startCol = 6, startRow = sumRowStart+i-1)
  }
  
  # Add the formula for summing all the program area subtotal rows to get the total
  ## Note: Hard coded summing for now. Can adjust later to be more dynamic
  writeFormula(wb, sheet = summary_sheet_num, x = "=SUM(F5:F10)", startCol = 6, 
               startRow = sumRowStart+num_paRows-1)
  
  # Sum up total IM budget for OU, based on IM totals in column C of the summary sheet
  writeFormula(wb, sheet = summary_sheet_num, x = glue("=SUM(C{sumRowStart}:C{sumRowEnd})"),
               startCol = 3, startRow = sumRowEnd+1)
  # Add text: "Total" to the correct location, which dynamically changes depending on # of IMs
  writeData(wb, sheet = summary_sheet_num, x = "TOTAL", startCol = 2, startRow = sumRowEnd+1)
  
  
  ##### Style summary sheet. See https://ycphs.github.io/openxlsx/reference/createStyle.html and https://ycphs.github.io/openxlsx/reference/addStyle.html
  # Reminder: gridExpand and stack should both be TRUE
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
  addStyle(wb, sheet = summary_sheet_num, bold_txt, rows=11, cols=6, gridExpand=T, stack=T)
  
  # Turn off gridlines for the summary sheet
  showGridLines(wb, summary_sheet_num, showGridLines = FALSE)
  
  # Remove reference
  removeWorksheet(wb, ref_sheet_num)
  
  # Add OU name and Fiscal year to Guidance page
  writeData(wb, 1, x = OU_name, startCol = 2, startRow = 4)
  writeData(wb, 1, x = curr_cop_year, startCol = 2, startRow = 5)
  showGridLines(wb, 1, showGridLines = FALSE)
  
  # Save file based on "dr" (file path to directory) and OU name
  file_name <- glue("{dr}{OU_name}_FAST_reference.xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
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


# Read in File==================================================================

# Note: Run glamr function "si_paths" to generate local paths based on organization's
#       common folder structure for data stored locally.
#       Then call latest file, read in data.frame and filter to select only USAID entries
df_fsd_full <- glamr::si_path() %>%
  return_latest("Fin") %>% 
  gophr::read_msd()

# Subset FSD for only USAID data and correctly mutate items
df_fsd <- df_fsd_full%>% 
  gophr::clean_agency()%>%
  dplyr::mutate(funding_agency=dplyr::case_when(funding_agency== "WCF"~"USAID", 
                                               TRUE ~funding_agency))%>%
  dplyr::mutate(interaction_type=case_when(interaction_type=="Non Service Delivery"~"NSD",
                                           interaction_type=="Service Delivery"~"SD",
                                           TRUE ~interaction_type)) %>%
  filter(funding_agency == "USAID") %>%
  remove_mo()

# Align labels "Program Management" and "IM Program Management"
df_fsd$sub_program <- replace(df_fsd$sub_program, df_fsd$sub_program == "Program Management", "IM Program Management")


# Create Directory =============================================================
#create output folders folders locally
dir_create(fisc_dir)

# # Tests ========================================================================
 test_OU <- "Nigeria"
 gen_wb(test_OU, fisc_dir)


# Walk =========================================================================
# List of distinct OUs
lst_ou <-
  df_fsd %>%
  distinct(operatingunit) %>%
  pull() 
   
   lst_ou <- lst_ou [!lst_ou == "Nigeria"]


# SHORTEN THE LIST JUST FOR SAKE FO DEMO RUN TIME
lst_ou <- lst_ou[1:6]

# Global Progress Tracking variables
global_progress <- 1
total_files <- length(lst_ou)

# Create output budget files
walk(lst_ou,
     function(x) progress_tracker(track_num=1, 
                                     global_progress = global_progress,
                                     total_files=total_files,
                                     func=gen_wb,
                                     # OU_name: string type. This passes onto gen_wb as an argument
                                     OU_name = x,
                                     # dr: string type. Path argument that passes onto gen_wb
                                     dr = fisc_dir
     ))


# Length of # of unique mechs matches the number of files created in ER directory
total_files == length(list.files(fisc_dir))


# UPLOAD==============================================================================
load_secrets()

upload_dir_to_gdrive(fisc_dir, glbl_id)



# #remove all local files
# unlink(fisc_dir_name, recursive = TRUE)

