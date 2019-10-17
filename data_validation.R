#######################################################################
# R code accompanying Sau-Chin Chen et al. (2019)
# OSF projet: https://osf.io/e428p/
# ---------------------------------------------------------
# Download rawdata from lab OSF
# Written by Sau-Chin Chen
# E-mail: pmsp96@gmail.com
# Last update: September 30, 2019
#############################################################

library(tidyverse)
library(data.table)
library(magrittr)

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Define comments from labratories.
# Participants' data with these comments are reserved
comment_words <- c("reverse order")

## Get the data info from csv file 
data_info <- dirname(getwd()) %>%
  dir(full.names = TRUE, recursive = TRUE, include.dirs = TRUE, pattern = "data_info.csv")  %>%
  read.csv()

##set working directory to current project folder
setwd(here::here())

# Focus on the lab with data files
data_dir <- subset(data_info, N_files > 10) %>% pull(PSA_ID) %>% as.character()
old_path = dirname(getwd()) ## Store the root directory

# Check the exist of the data directory
rawdata_log <- NULL
rawdata_SP_V <- NULL
rawdata_SP_M <- NULL
rawdata_PP <- NULL

for(LAB in data_dir){
  
  # Confirm the number of files as the same as the lab log
  if(dir.exists(paths=paste0(old_path,"/raw_data/", LAB))){
    paste(LAB, (subset(data_info,PSA_ID == LAB) %>% 
                  pull(N_files) ) ==  (list.files(paste0(old_path,"/raw_data/", LAB)) %>% 
                                         length())) %>% print()
  } 
  
  ## Import lab log
  rawdata_log <- bind_rows(rawdata_log, 
                           dir(path = paste0(old_path,"/raw_data/", LAB), pattern = paste0(LAB,".csv"), recursive = TRUE, full.names = TRUE)  %>%
    read.csv %>% filter((DATE!= ""))  )
  
  ## Import SP verification data
  rawdata_SP_V <- bind_rows(rawdata_SP_V,dir(path = paste0(old_path,"/raw_data/", LAB), pattern = "_SP_", recursive = TRUE, full.names = TRUE)  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>%
    filter(Task == "V") %>% mutate_if(is.integer, as.character))
  
  ## Import SP memory data
  rawdata_SP_M <- bind_rows(rawdata_SP_M, dir(path = paste0(old_path,"/raw_data/", LAB), pattern = "_SP_", recursive = TRUE, full.names = TRUE)  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>%
    filter(Task == "M") %>% mutate_if(is.integer, as.character))
  
  ## Import PP verification data
  rawdata_PP <- bind_rows(rawdata_PP, dir(path = paste0(old_path,"/raw_data/", LAB), pattern = "_PP_", recursive = TRUE, full.names = TRUE)  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>% mutate_if(is.integer, as.character))
}


## Managing: Filter the valid logs
valid_logs <- rawdata_log %>% filter((DATE!= "")) %>% 
  filter(Comments %in% comment_words | Comments == "" | is.na(Comments)) %>%
  select("SEED", "SUBJID")


## Check the order and date of SP
## Mutate and retrieve the log cells for validation
((rawdata_log %>% subset(DATE != "") %>% 
  mutate(log_date = as.Date(DATE,"%d/%m/%Y"), log_order = if_else(Sequence == "002_SP -> 002_PP -> 003","Yes","No")) %>% 
  select(SEED, SUBJID, log_date, log_order)) == 
## Mutate and retrieve the rawdata cells for validation
(rawdata_SP_V %>% group_by(LAB_SEED, date = as.Date(datetime,"%m/%d/%y"), subject_nr,task_order) %>%
  summarise(n()) %>% arrange(subject_nr) %>%  
  select(LAB_SEED, subject_nr, date, task_order) %>% as.data.frame())) %>%
colSums() ## If the lab follow the log sheet, the numbers will equal to the number of available of data files.

## append PSA_ID to rawdata
## Export all the rawdata to the single file
## Raw data of SP verification trials
data_info %>% 
  select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_SP_V, by=c("SEED" = "LAB_SEED")) %>%
  filter((SEED %in% valid_logs$SEED) & (subject_nr %in% valid_logs$SUBJID)) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_SP_V.csv"),row.names = FALSE)
## Raw data of SP memory trials
data_info %>% select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_SP_M, by=c("SEED" = "LAB_SEED")) %>% 
  filter((SEED %in% valid_logs$SEED) & (subject_nr %in% valid_logs$SUBJID)) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_SP_M.csv"),row.names = FALSE)
## Raw data of PP verification trials
data_info %>% select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_PP, by=c("SEED" = "LAB_SEED")) %>% 
  filter((SEED %in% valid_logs$SEED) & (subject_nr %in% valid_logs$SUBJID)) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_PP.csv"),row.names = FALSE)

