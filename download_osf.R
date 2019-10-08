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

## Get the lab info from csv file
lab_info <- getwd() %>%
  paste0("/",dir(recursive = TRUE, pattern = "Lab_info.csv"))  %>%
  read.csv()

## Check the status of the data osf
## Reserve the id for processing
## Method to download multiple data in one OSF:
## https://github.com/CenterForOpenScience/osfr/issues/98
old_path = getwd() ## Store the root directory
N_files <- NULL

##loop over osf_ids
for(osf_id in lab_info$osfid){

  ##make sure it's public
  if(subset(lab_info, osfid == osf_id)$Publicity == "Yes"){
    
    ##look at their project
    cr_project <- osf_retrieve_node(osf_id)
    
    ## Get file list from OSF
    osf_data <- osf_ls_files(cr_project,n_max = Inf)
    
    ## Get the number of files
    N_files <- c(N_files,dim(osf_data)[1])
    
    ## Check if the folder exists to put the data
    if(!dir.exists(paths=paste0(old_path,"/EXPDATA/raw_data/",as.character(subset(lab_info, osfid == osf_id)$PSA_ID)))){
      dir.create(paste0("EXPDATA/raw_data/",as.character(subset(lab_info, osfid == osf_id)$PSA_ID)))
    }
    
    ## Switch the working directory
    setwd(paste0(old_path,"/EXPDATA/raw_data/",as.character(subset(lab_info, osfid == osf_id)$PSA_ID)))
    
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
      
      ## Switch back to the root directory
      setwd(old_path)
      
    }
    
  }


data_info <- bind_cols(subset(lab_info, Publicity == "Yes"), N_files = N_files)
data_info %>% write.csv("EXPDATA/raw_data/data_info.csv")
