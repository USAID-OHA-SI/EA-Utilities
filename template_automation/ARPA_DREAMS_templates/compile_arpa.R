# Version 3.2
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

# Functions ====================================================================
# Generate OU dataframe, based on OU filename
gen_df_ou <- function(file_path){
  # Read in data rows
  df_id <- read.xlsx(file_path, 1, rows = 3:4)
  df_data <- read.xlsx(file_path, 2, startRow=1)
  
  # Create columns with mechanism ID info
  df_ou <- cbind(operatingunit = df_id$Operating.Unit, 
                 country = df_id$Country,
                 primepartner = df_id$Partner.Name,
                 mech_code = df_id$Mech.ID,
                 mech_name = df_id$Mechanism.Name,
                 fiscal_year = df_id$Fiscal.Year,
                 df_data) 
  
  # Split glued program:sub-program and beneficiary:sub-beneficiary columns
  df_ou <- df_ou %>%
    separate(col="Program.Area", 
             into=c("program", "sub_program"), sep=":.") %>%
    separate(col="Beneficiary", 
             into=c("beneficiary", "sub_beneficiary"), sep=":.")
  
  return(df_ou)
}

# Func: Binds all xlsx's in designated directory together, by ARPA columns
bind_arpa <- function(dir_path, col_names = cols){
  paths_lst <- list.files(dir_path, full.names=T)
  dirs_lst <- list.dirs(dir_path, recursive=F, full.names=T)
  true_lst <- setdiff(paths_lst, dirs_lst)
  
  print(true_lst[1])
  df_arpa <- gen_df_ou(true_lst[1])
  # bind generated dataframes per xlsx in the directory to one main ARAP dataframe
  for (file_path in true_lst[-1]){
    print(file_path)
    df_arpa <- rbind(df_arpa, gen_df_ou(file_path))
  }
  
  return(df_arpa)
}

# Main =========================================================================

small_df <- bind_arpa(filled_dir)

# df_data <- df_arpa_test[13:32]

# # https://www.geeksforgeeks.org/remove-rows-with-empty-cells-in-r/
# num_cols <- ncol(df_data)
# na_lst <- is.na(df_data)
# na_rows_idx <- rowSums(na_lst)
# small_df <- df_arpa_test %>%
#   mutate(na_rows = na_rows_idx)%>%
#   filter(na_rows != num_cols) %>%
#   select(-na_rows)

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

# Export data
date <- as.character(Sys.Date())
filename <- glue("compiled_arpa_{date}.csv")
write.csv(long_df, filename, na="", row.names=FALSE)
