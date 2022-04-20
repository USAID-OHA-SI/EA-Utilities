# Version 3.2
# Purpose: Compiles ARPA templates and pivots for easier reading
# Updates for version 3.2
#         * Long pivot and add TOTAL category
# Author: David Song
# Date: 2022 Jan 31

# LOCALS & SETUP ===============================================================
# Below code sets working directory to source file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries ====================================================================
library(installr)
### If statement checks to see if openxlsx is installed and has the correct version
packageurl <- "https://cran.r-project.org/src/contrib/Archive/openxlsx/openxlsx_4.2.3.tar.gz"
if(!require(openxlsx)){install.packages('openxlsx')#packageurl, repos=NULL, type="source")
} else if (packageVersion('openxlsx') != "4.2.3"){
  detach("package:openxlsx", unload=TRUE)
  install.packages(packageurl, repos=NULL, type="source")}

library(glamr)
library(gophr)
library(googledrive)
library(glue)
library(tidyverse)

# Global Variables =============================================================
# Path to where filled templates are
filled_dir <- "filled_templates"

# This is the beginning of the output file name. See below to modify file type
### Example: file name becomes: compiled_arpa_2022-03-16.csv
compiled_file_name_header <- "compiled_arpa"

# Functions ====================================================================
# Generate mechanism-level dataframe, based on mechanism filename
### Input: String. Path to an Excel file (a filled ARPA template)
### output: data.frame. A mechanism-level data.frame with data from template
gen_df_mech <- function(file_path){
  # Read in data rows
  df_id <- read.xlsx(file_path, 1, rows = 3:4)
  df_data <- read.xlsx(file_path, 2, startRow=1)
  
  # Create columns with mechanism ID info on 1st page of the filled template
  df_mech <- cbind(operatingunit = df_id$Operating.Unit, 
                 country = df_id$Country,
                 primepartner = df_id$Partner.Name,
                 mech_code = df_id$Mech.ID,
                 mech_name = df_id$Mechanism.Name,
                 fiscal_year = df_id$Fiscal.Year,
                 df_data) 
  
  # Split glued program:sub-program and beneficiary:sub-beneficiary columns
  df_mech <- df_mech %>%
    separate(col="Program.Area", 
             into=c("program", "sub_program"), sep=":.") %>%
    separate(col="Beneficiary", 
             into=c("beneficiary", "sub_beneficiary"), sep=":.")
  
  return(df_mech)
}

# Func: Binds all xlsx's in designated directory together, by ARPA columns
## Note: This function works for ARPA, Quarterly, and DREAMS templates
### Inputs: dir_path: String. Path to the main directory that all OU sub-folders live in
### Output: 
bind_arpa <- function(dir_path){
  # List of all files, including sub-folders and files inside subfolders, in our main directory
  ### Note: full.names=T means that we have the full, complete path to these files
  paths_lst <- list.files(dir_path, full.names=T)
  # List of all sub-folders in our main directory
  dirs_lst <- list.dirs(dir_path, recursive=F, full.names=T)
  # Create list of file paths that exclude the paths of the sub-folders themselves (hence using setdiff)
  true_lst <- setdiff(paths_lst, dirs_lst)
  
  # Initialize dataframe df_arpa with the first mechanism in our directory
  ### Note: because we initialize our bind with the 1st mechanism found in our directory, 
  ###       we do not need to hard code what columns to join on. 
  ### Warning: HOWEVER: If the 1st mechanism has the wrong columns, this rbind will fail.
  df_arpa <- gen_df_mech(true_lst[1])
  print(true_lst[1])
  df_arpa <- gen_df_mech(true_lst[1])
  # Bind generated dataframes per xlsx in the directory to one main ARAP dataframe
  for (file_path in true_lst[-1]){
    print(file_path)
    df_arpa <- rbind(df_arpa, gen_df_mech(file_path))
  }
  
  return(df_arpa)
}

# Main =========================================================================

## Build out the main dataframe 
small_df <- bind_arpa(filled_dir)

#=================OPTIONAL: Do not use when compiling non-ARPA==================
## Prepare data.frame for pivoting, to format it as Christy requested
# convert periods to spaces in column names, before converting them to categories
names(small_df) <- gsub("\\.", " ", names(small_df))

small_df <- small_df %>% rename(`Total FY21 Expenditure` = Expenditure)

# Pivot Longer to create ARPA category column
long_df <- small_df %>% 
  pivot_longer(cols= `Total FY21 Expenditure`:`Mitigation: Other`, 
               names_to="ARPA_category", 
               values_to="ARPA_expenditure",
               values_drop_na = T) %>%
  # case_when requires a numeric NA, not base "logical" NA https://github.com/tidyverse/dplyr/issues/3202
  mutate(`Activity Description` = case_when(ARPA_category == "Total FY21 Expenditure" ~ NA_character_,
                                          TRUE ~ `Activity Description`)) %>%
  relocate(`Activity Description`, .after = last_col())
#===============================================================================
#### Export data================================================================
# Pull date for naming file
date <- as.character(Sys.Date())
# Note: file is saved as csv by default
filename <- glue("{compiled_file_name_header}_{date}.csv")
write.csv(long_df, filename, na="", row.names=FALSE)
