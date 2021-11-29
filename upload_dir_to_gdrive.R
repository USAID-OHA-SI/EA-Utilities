##### Upload to Google Drive ################################################
# Version 4.1
# Instructions: use the function "upload_dir_to_gdrive()" to upload
#               Do NOT use the helper function "drive_upload_uniq"
# Inputs:
#     * local_dir: name of local directory you want to upload (type: string)
#     * drive_path: name of google drive folder (destination) (type: string)
#
# NOTE: This function will ignore sub-folders in your target local directory
#       and will only upload files. I will work on improving this so it can 
#       recursively upload sub-folders too. 
#
# Author: David Song
# Date: 2021 Nov 9
# 
# Version 4.1: Adjust path inputs to better work in for loops
#
#              Use built-in Google Drive function for overwrite 
#              But since overwite cannot handle single quotes, strip quote from
#              file names

library(glue)
library(tidyverse)
library(googledrive)

# Helper Func: Check if file exists before uploading
# Reason: Google API allows for duplicate file name uploads
drive_upload_uniq <- function(path_name,
                              drive_dir){
  file_name <- str_replace(basename(path_name), "'", " ")
  print(file_name)
  drive_upload(path_name, path = as_id(drive_dir),
               name = file_name, overwrite=T)
}

# Func: Upload directory to Google Drive
# Inputs:
#         * local_dir: name of local directory you want to upload (type: string)
#         * drive_path: name of google drive folder (destination) (type: string)
upload_dir_to_gdrive <- function(local_dir, drive_path){
  # Check if local directory of that name actually exists
  if (!(dir.exists(local_dir))){
    stop(glue("Local directory {local_dir} does not exist"))
  }
  
  # check if output directory already exists in drive, and if not,
  # then makes it
  local_dir_basename <- basename(local_dir)
  if(!(local_dir_basename %in% drive_ls(path = as_id(drive_path))$name)){
    drive_mkdir(local_dir_basename,
                # path is to your desired Google drive directory
                path = as_id(drive_path))
  }
  
  len_id <- 0  

  while (len_id == 0) {
    # Get path for created directory
    drive_ids <- drive_ls(path = as_id(drive_path))
    drive_dir <- drive_ids$id[drive_ids$name == local_dir_basename]
    print(drive_dir)
    len_id <- length(drive_dir)
    if (len_id == 0){
      # Assuming CPU usage is low enough, sleep will slow down code so that 
      # Google Drive has enough time to make the directory
      print("Waiting for Google Drive...")
      Sys.sleep(4)
    }
  }
  

  # Exclude directories and only include files
  lst_files <- setdiff(list.files(path = local_dir), 
                       list.dirs(path=local_dir, recursive=F,full.names=F))
  lst_paths <- paste0(glue("{local_dir}/"), lst_files, sep="")
  
  existing_files <-drive_ls(path = drive_dir)
  
  # Upload pdfs to Drive
  walk(lst_paths,
       ~ drive_upload_uniq(.x, drive_dir = drive_dir))
       
       # (.x, path = drive_dir, name = basename(.x), overwrite=T))
}

df_ou$mech_code[df_ou$operatingunit== "zoom"]

drive_ids2 <-drive_ls(path = as_id(gdrive_path))
drive_dr <- drive_ids2$id[drive_ids2$name == 'fake']
length(drive_dr)

