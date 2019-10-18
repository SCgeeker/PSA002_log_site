#######################################################################
# R code accompanying Sau-Chin Chen et al. (2019)
# OSF projet: https://osf.io/e428p/
# ---------------------------------------------------------
# Download rawdata from lab OSF
# Written by Sau-Chin Chen
# E-mail: pmsp96@gmail.com
# Last update: October 18, 2019
#############################################################

library(tidyverse)
library(data.table)
library(magrittr)

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Gather the note from labratories.
# Participants' data with these words are excluded
excluded_words <- c("had to leave","SP crashed","didnt finish qualtrics","már volt")

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

# Import and separate 
for(LAB in data_dir){
  
  # Confirm the number of files as the same as the lab log
  if(dir.exists(paths=paste0(old_path,"/1_raw_data/", LAB))){
    paste(LAB, (subset(data_info,PSA_ID == LAB) %>% 
                  pull(N_files) ) ==  (list.files(paste0(old_path,"/1_raw_data/", LAB)) %>% 
                                         length())) %>% print()
  } 
  
  ## Import the latest lab log
  ## Check the first line of a csv file
  new_rawdata_log_path <- dir(path = paste0(old_path,"/1_raw_data/", LAB), pattern = paste0(LAB,".csv"), recursive = TRUE, full.names = TRUE)
  if(read.table(new_rawdata_log_path, nrows = 1)$V1 %>% str_detect(";") == TRUE){
    new_rawdata_log <- read.csv2(new_rawdata_log_path)  %>% filter((DATE!= ""))
  } else {
    new_rawdata_log <- read.csv(new_rawdata_log_path)  %>% filter((DATE!= ""))
  }

  ## Extract lab note from lab log
  lab_note <- NULL
  if(dim(new_rawdata_log)[2] > 6){
    lab_note <- new_rawdata_log[,c(1,2,7)]
    names(lab_note) <- c("SEED","SUBJID","Note")
    new_rawdata_log = new_rawdata_log[,1:6]
  }
    
  ## Combine the latest lab log
  rawdata_log <- bind_rows(rawdata_log, 
                           new_rawdata_log)
  
  ## Append the latest lab note
  if(!is.null(lab_note)){
    rawdata_log <- left_join(rawdata_log, lab_note, by=c("SEED","SUBJID"), copy=TRUE, all = TRUE)
    if(sum(names(rawdata_log) %in% c("Note.x","Note.y")) > 1 ) {
      rawdata_log$Note.x <- rawdata_log$Note.x %>% str_replace_all(pattern = "<NA>",replacement = "")
      rawdata_log$Note.y <- rawdata_log$Note.y %>% str_replace_all(pattern = "<NA>",replacement = "")
      rawdata_log <- rawdata_log %>% unite("Note",Note.x:Note.y, remove = TRUE, na.rm=TRUE, sep = "")
    }
  }
  
  ## Import SP verification data
  rawdata_SP_V <- bind_rows(rawdata_SP_V,dir(path = paste0(old_path,"/1_raw_data/", LAB), pattern = "_SP_", recursive = TRUE, full.names = TRUE)  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>%
    filter(Task == "V") %>% mutate_if(is.integer, as.character))
  
  ## Import SP memory data
  rawdata_SP_M <- bind_rows(rawdata_SP_M, dir(path = paste0(old_path,"/1_raw_data/", LAB), pattern = "_SP_", recursive = TRUE, full.names = TRUE)  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>%
    filter(Task == "M") %>% mutate_if(is.integer, as.character))
  
  ## Import PP verification data
  rawdata_PP <- bind_rows(rawdata_PP, dir(path = paste0(old_path,"/1_raw_data/", LAB), pattern = "_PP_", recursive = TRUE, full.names = TRUE)  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>% mutate_if(is.integer, as.character))
}


## Managing: Filter the invalid participants
invalid_logs <- rawdata_log %>% filter((DATE!= "")) %>% 
  filter(Note %in% excluded_words) %>%
  select("SEED", "SUBJID")


#### Below code validates the consistency between lab log and SP rawdata
#### DATE format require updateings.
## Check the order and date of SP
## Mutate and retrieve the log cells for validation
((rawdata_log %>% subset(DATE != "") %>%
    ### Not all DATE could be transfered
  mutate(log_date = as.Date(DATE,"%d/%m/%Y"), log_order = if_else(Sequence == "002_SP -> 002_PP -> 003","Yes","No")) %>% 
  select(SEED, SUBJID, log_date, log_order)) == 
## Mutate and retrieve the rawdata cells for validation
(rawdata_SP_V %>% group_by(LAB_SEED, date = as.Date(datetime,"%m/%d/%y"), subject_nr,task_order) %>%
  summarise(n()) %>% arrange(subject_nr) %>%  
  select(LAB_SEED, subject_nr, date, task_order) %>% as.data.frame())) %>%
colSums() ## If the lab follow the log sheet, the numbers will equal to the number of available of data files.

## append PSA_ID to rawdata
## Export all the valid rawdata to the single file
## Raw data of SP verification trials
data_info %>% 
  select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_SP_V, by=c("SEED" = "LAB_SEED")) %>%
  filter(!(SEED %in% invalid_logs$SEED) & !(subject_nr %in% invalid_logs$SUBJID)) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_SP_V.csv"),row.names = FALSE)
## Raw data of SP memory trials
data_info %>% select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_SP_M, by=c("SEED" = "LAB_SEED")) %>% 
  filter(!(SEED %in% invalid_logs$SEED) & !(subject_nr %in% invalid_logs$SUBJID)) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_SP_M.csv"),row.names = FALSE)
## Raw data of PP verification trials
data_info %>% select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_PP, by=c("SEED" = "LAB_SEED")) %>% 
  filter(!(SEED %in% invalid_logs$SEED) & !(subject_nr %in% invalid_logs$SUBJID)) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_PP.csv"),row.names = FALSE)

