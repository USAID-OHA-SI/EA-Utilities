##### Upload to Google Drive ################################################
# Version 3.0
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
# Version 3.0: Fixes "drive_rm" behavior to only remove file with correct 
#              google drive id, rather than deleting a file with the same name
#              but anywhere in the user's Google Drive

library(glue)
library(tidyverse)
library(googledrive)

# Helper Func: Check if file exists before uploading
# Reason: Google API allows for duplicate file name uploads
drive_upload_uniq <- function(path_name,
                              existing_files,
                              drive_dir){
  file_name <- basename(path_name)
  
  if((file_name %in% existing_files$name)){
    print(glue("checking {file_name}......."))
    # Finds Google Drive file ID based on name
    file_id <- existing_files[existing_files$name == file_name,]
    drive_rm(as_id(file_id))
    warning(glue("{file_name} already exists in the Google Drive and was overwritten."))
  } 
  
  drive_upload(path_name, path = as_id(drive_dir),
               name = file_name)
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
  
  # Get path for created directory
  drive_ids <- drive_ls(path = as_id(drive_path))
  drive_dir <- drive_ids$id[drive_ids$name == local_dir_basename]
  
  # Exclude directories and only include files
  lst_files <- setdiff(list.files(path = local_dir), 
                       list.dirs(path=local_dir, recursive=F,full.names=F))
  lst_paths <- paste0(glue('{local_dir}/'), lst_files, sep='')
  
  existing_files <-drive_ls(path = as_id(drive_dir))
  
  # Upload pdfs to Drive
  walk(lst_paths,
       ~ drive_upload_uniq(.x,
                           existing_files = existing_files,
                           drive_dir = drive_dir))
}
