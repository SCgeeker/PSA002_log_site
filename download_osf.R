#######################################################################
# R code accompanying Sau-Chin Chen et al. (2019)
# OSF projet: https://osf.io/e428p/
# ---------------------------------------------------------
# Download rawdata from lab OSF
# Written by Sau-Chin Chen
# E-mail: pmsp96@gmail.com
# Last update: September 29, 2019
#############################################################

# Download the lab data
#library(tidyverse)
library(dplyr)
library(osfr)

##set working directory to current project folder
setwd(here::here())


## Check the status of the data osf
## Reserve the id for processing
## Method to download multiple data in one OSF:
## https://github.com/CenterForOpenScience/osfr/issues/98
old_path = dirname(getwd()) ## Store the root directory
N_files <- NULL

##loop over osf_ids
for(osf_id in lab_info$osfid){
  ## Check the publicity of lab OSF
  if(subset(lab_info, osfid == osf_id)$Publicity == "Yes"){
    
  ##look at their project
  cr_project <- osf_retrieve_node(osf_id)
  
  ## Get file list from OSF
  osf_data <- osf_ls_files(cr_project,n_max = Inf)
  
  ## Get the number of files
  N_files <- c(N_files,dim(osf_data)[1])
  
  ##make sure this lab OSF is public & has collected data
  if(subset(lab_info, osfid == osf_id)$N > 0 & 
     tail(N_files,1) > 10){
    
      ## Check if the folder exists to put the data
      if(!dir.exists(paths=paste0(old_path,"/1_raw_data/",as.character(subset(lab_info, osfid == osf_id)$PSA_ID)))){
        dir.create(paste0(old_path,"/1_raw_data/",as.character(subset(lab_info, osfid == osf_id)$PSA_ID)))
      }
    
      ## Switch the working directory
      setwd(paste0(old_path,"/1_raw_data/",as.character(subset(lab_info, osfid == osf_id)$PSA_ID)))
    
      ## Grab the list of files in the directory
      Lab_files <- list.files()
    
      ## Find the differences
      Download_files <- setdiff(osf_data$name, Lab_files)
    
      ## If there are new files, download them  
      if(length(Download_files) > 0) {

        osf_data %>%
          filter(name %in% Download_files) %>% 
          {split(., 1:nrow(.))} %>%
          lapply(osf_download, path= .$name, overwrite = TRUE)
        }
     }
  }
  ## Switch back to the root directory
  setwd(old_path)
}

## Update the latest data size of each participating team.
data_info <- bind_cols(subset(lab_info, Publicity == "Yes"), N_files = N_files)
write.csv(data_info, file=paste0(old_path,"/1_raw_data/data_info.csv"), row.names = FALSE)
