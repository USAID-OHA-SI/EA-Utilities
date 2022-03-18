##### Upload to Google Drive ################################################
# Version 4.2
# Instructions: use the function "upload_dir_to_gdrive()" to upload
#               Do NOT use the helper function "drive_upload_uniq" standalone
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
# Modification Date: 2022 March 14
#
# Version 4.2: Clean comments
# Version 4.1: Adjust path inputs to better work in for loops
#              Use built-in Google Drive function for overwrite 
#              But since overwite cannot handle single quotes, strip quote from
#              file names

library(glue)
library(tidyverse)
library(googledrive)

# Helper Function for upload_dir_to_gdrive
### Purpose: Check if file exists before uploading, then runs the Google upload API
### Reason: Google API allows for duplicate file name uploads
drive_upload_uniq <- function(path_name,
                              drive_dir){
  # file_name comes from the "basename" of the path, i.e. the actual file name
  ### Note: This strips single quotation marks because Google's drive_upload cannot 
  ###       correctly overwrite files with single quotes in the name
  file_name <- str_replace(basename(path_name), "'", " ")
  print(file_name)
  
  # saves file onto google drive in the Google folder specified by path
  ### See: https://www.rdocumentation.org/packages/googledrive/versions/2.0.0/topics/drive_upload
  ### Note: We use the Google API's as_id in order to convert string to an ID that
  ###       the API understands as the folder's location
  drive_upload(path_name, path = as_id(drive_dir),
               name = file_name, overwrite=T)
}

######## MAIN FUNCTION TO CALL IN YOUR CODE ####################################
# Purpose: Upload directory to Google Drive
# Inputs:
#         * local_dir: name of local directory you want to upload (type: string)
#         * drive_path: name of google drive folder (destination) (type: string)
upload_dir_to_gdrive <- function(local_dir, drive_path){
  # Check if local directory of that name actually exists
  if (!(dir.exists(local_dir))){
    # If doesn't exist, stop code
    stop(glue("Local directory {local_dir} does not exist"))
  }
  
  # Store just the directory name as string, not full path 
  ### Note: regardless of if full path is provided, basename() will return directory name
  local_dir_basename <- basename(local_dir)
  
  # Purpose: Check if output directory already exists in Google Drive, and if not,
  #          then makes the output directory
  if(!(local_dir_basename %in% drive_ls(path = as_id(drive_path))$name)){
    drive_mkdir(local_dir_basename,
                # path is to your desired Google drive directory
                path = as_id(drive_path))
  }
  
  # Initialize number of files in directory to 0, to start while-loop
  len_id <- 0  

  # This while-loop checks if the directory exists or got made in line 54
  ### Note: This loop exists because sometimes the code runs faster than 
  ###       Google Drive, so the code thinks the directory does not exist when 
  ###       it was just made.
  while (len_id == 0) {
    # Get path for created directory
    drive_ids <- drive_ls(path = as_id(drive_path))
    drive_dir <- drive_ids$id[drive_ids$name == local_dir_basename]
    print(drive_dir)
    
    # Checks number of files in Google Drive directory
    len_id <- length(drive_dir)
    # If number of files in directory are zero, wait/sleep
    if (len_id == 0){
      # Assuming CPU usage is low enough, sleep will slow down code so that 
      # Google Drive has enough time to make the directory
      print("Waiting for Google Drive...")
      Sys.sleep(4)
    }
  }
  

  # Note: setdiff excludes directories and only include real files
  ### This is because list.files lists both files AND directories
  lst_files <- setdiff(list.files(path = local_dir), 
                       list.dirs(path=local_dir, recursive=F,full.names=F))
  
  # Purpose: Generate full file paths for all files in your local directory that  
  #          you want exported to Google Drive
  lst_paths <- paste0(glue("{local_dir}/"), lst_files, sep="")
  
  existing_files <-drive_ls(path = drive_dir)
  
  # Upload pdfs to Drive by walking through all local file paths
  walk(lst_paths,
       # Calls helper function described above
       ~ drive_upload_uniq(.x, drive_dir = drive_dir))
}

