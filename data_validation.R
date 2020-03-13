#######################################################################
# R code accompanying Sau-Chin Chen et al. (2019)
# OSF projet: https://osf.io/e428p/
# ---------------------------------------------------------
# Download rawdata from lab OSF
# Written by Sau-Chin Chen
# E-mail: pmsp96@gmail.com
# Last update: January 18, 2020
#############################################################

library(tidyverse)
library(data.table)
library(magrittr)

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

# Gather the note from labratories.
# Participants' data with these words are excluded
excluded_words <- c("had to leave","SP crashed","PP crashed","didnt finish qualtrics","már volt","SP & PP Would not work properly
","Completed Task A in Chinese")

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
    new_rawdata_log <- read.csv2(new_rawdata_log_path, encoding = "UTF-8")  %>% filter((DATE!= ""))
  } else {
    new_rawdata_log <- read.csv(new_rawdata_log_path, encoding = "UTF-8")  %>% filter((DATE!= ""))
  }
  
  #new_rawdata_log <- tibble(new_rawdata_log)

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
      rawdata_log$Note.x <- rawdata_log$Note.x %>% str_replace_all(pattern = "NA",replacement = "")
      rawdata_log$Note.y <- rawdata_log$Note.y %>% str_replace_all(pattern = "NA",replacement = "")
      rawdata_log <- rawdata_log %>% unite("Note",Note.x:Note.y, remove = TRUE, na.rm=TRUE, sep = "")
    }
  }
  
  ## Print message
  print("lab log loaded.")
  
  ## Validate SP file size
  ## Code the SP files beyond 100k
  SP_ind <- ((SP_path <- dir(path = paste0(old_path,"/1_raw_data/", LAB), pattern = "_SP_|_sp_|-SP_|-SP-", recursive = TRUE, full.names = TRUE)) %>%
      file.info() %>%
      select(size) > 90000) %>%
      which()
  
  ## Extract valid Lab seed
  valid_SEED <- rawdata_log$SEED %>% unique()

  ## Import SP verification data
  rawdata_SP_V <- bind_rows(rawdata_SP_V, SP_path[SP_ind]  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>%
    filter(Task == "V") %>% mutate_if(is.integer, as.character) %>%
      ## Retreive the columns for summary
      select(datetime,LAB_SEED, logfile, subject_nr,task_order,List,Match,Orientation,PList,Probe,Target,response_time,correct,opensesame_codename,opensesame_version)) %>%
      arrange(LAB_SEED, as.numeric(logfile))
  ## Filter invalid lab seed
  rawdata_SP_V <- rawdata_SP_V %>% filter(LAB_SEED %in% valid_SEED)
  
  ## Print message
  print("SP verification data loaded.")
  
    
  ## Import SP memory data
  rawdata_SP_M <- bind_rows(rawdata_SP_M, SP_path[SP_ind]  %>%
    lapply(read.csv)  %>%
      rbindlist(fill = TRUE) %>%
    filter(Task == "M") %>% mutate_if(is.integer, as.character) %>%
    ## Retreive the columns for summary 
    select(datetime,LAB_SEED, logfile,subject_nr,List,PList,Probe,response_time,correct,opensesame_codename,opensesame_version,alt_task)  )
  ## Filter invalid lab seed
  rawdata_SP_M <- rawdata_SP_M %>% filter(LAB_SEED %in% valid_SEED)
  
  ## Print message
  print("SP memory data loaded.")
  
  ## Validate PP file size
  ## Code the PP files beyond 70k
  PP_ind <- ((PP_path <- dir(path = paste0(old_path,"/1_raw_data/", LAB), pattern = "_PP_|_pp_|-PP_|_pP_", recursive = TRUE, full.names = TRUE)) %>%
               file.info() %>%
               select(size) > 70000) %>%
               which()
  
  ## Import PP verification data
  rawdata_PP <- bind_rows(rawdata_PP, PP_path[PP_ind]  %>%
    lapply(read.csv) %>% rbindlist(fill = TRUE) %>% mutate_if(is.integer, as.character) %>%
      ## Retreive the columns for summary
      select(datetime,LAB_SEED, logfile,subject_nr,PPList,Orientation1,Orientation2,Identical,Picture1,Picture2,response_time,correct,opensesame_codename,opensesame_version))
  ## Filter invalid lab seed
  rawdata_PP <- rawdata_PP %>% filter(LAB_SEED %in% valid_SEED)
  
  
  ## Print message
  print("PP verification data loaded.")
  
}

## Save rawdata lab log 
rawdata_log %>%
  fwrite(file=paste0(old_path,"/1_raw_data/rawdata_log.csv"), row.names = FALSE)
#  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_log.csv"),row.names = FALSE)

## Clean date formats
require(lubridate)

## Change date format of MYS_004 lab log
rawdata_log[rawdata_log$SEED==5186, "DATE"] = rawdata_log %>% filter(SEED==5186) %>% 
  mutate(DATE = DATE %>% parse_date_time(order="dmy") %>% as.character()) %>%
  pull(DATE)

## Change date format of THA_001 lab log
rawdata_log[rawdata_log$SEED==7236, "DATE"] = rawdata_log %>% filter(SEED==7236) %>% 
  mutate(DATE = DATE %>% parse_date_time(order="dmy") %>% as.character()) %>%
  pull(DATE)

## Change date format of GBR_043 lab log
rawdata_log[rawdata_log$SEED==5620, "DATE"] = rawdata_log %>% filter(SEED==5620) %>% 
  mutate(DATE = DATE %>% parse_date_time(order="dmy") %>% as.character()) %>%
  pull(DATE)


#### Below code validates the consistency between lab log and SP rawdata
#### DATE format require updateings.
## Rearrange the lab log and unify the date format for validation
log_df <- (rawdata_log %>% subset(!is.na(DATE)) %>%
    ### Not all DATE could be transfered
  mutate(lab_date = DATE %>% 
           parse_date_time(orders = c('dmy','mdy','ymd','dmy, HM','ymd, HM') ) %>%
           format(format="%Y-%m-%d"), 
         task_order = if_else(Sequence == "002_SP -> 002_PP -> 003","Yes","No")) %>% 
  select(SEED, SUBJID, lab_date, task_order, Note) %>%
  arrange(SEED, SUBJID))
## Check the order and date of SP data files
## Mutate and retrieve the log cells for validation
## Mutate and retrieve the rawdata cells for validation
SP_df <- (rawdata_SP_V %>% 
   mutate( lab_date = datetime %>% 
             parse_date_time(orders = c('mdy HMS','b d HMS Y','a b d HMS Y')) %>%
             format(format="%Y-%m-%d")) %>%
   group_by(LAB_SEED, subject_nr, lab_date, task_order) %>%
   summarise(N_trials = n()) %>% 
   arrange(LAB_SEED, as.numeric(subject_nr) ) %>%  
   rename(SEED = LAB_SEED, SUBJID = subject_nr, lab_date = lab_date, task_order = task_order, N_trials = N_trials) %>% 
   as.data.frame() ) 

## Modify the date format in log file
#log_df[,3] <- as.Date(log_df[,3])
## Modify the calss of LAB SEED and Subject ID in SP data files
SP_df[,1] <- as.numeric(SP_df[,1])
SP_df[,2] <- as.numeric(SP_df[,2])

## Some labs incorrectly coined the task order of SP
## Isolate the comment "Wrong choice, Correct sequence" in lab log
## Switch the label of task_order in the lab log
log_df$task_order[which(log_df$Note == "Wrong choice, Correct sequence")] = "Yes"
log_df$task_order[which(log_df$Note == "Wrong order option")] = "none of above"

## Merge lab log and SP meta data
## Export validataion information
(validation_df <- inner_join(SP_df, log_df) %>%
    filter(!(Note %in% excluded_words)) ) %>% 
  write.csv(file=paste0(old_path,"/1_raw_data/data_validataion.csv"),row.names = FALSE)

## append PSA_ID to rawdata
## Export all the valid rawdata to the single file
## Raw data of SP verification trials
data_info %>% 
  select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_SP_V, by=c("SEED" = "LAB_SEED")) %>%
  #filter( (SEED %in% validation_df$SEED) & (subject_nr %in% validation_df$SUBJID) ) %>%
  filter(paste(SEED, subject_nr) %in% paste(validation_df$SEED,validation_df$SUBJID) ) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_SP_V.csv"),row.names = FALSE)
## Raw data of SP memory trials
data_info %>% select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_SP_M, by=c("SEED" = "LAB_SEED")) %>% 
  #filter( (SEED %in% validation_df$SEED) & (subject_nr %in% validation_df$SUBJID) ) %>%
  filter(paste(SEED, subject_nr) %in% paste(validation_df$SEED,validation_df$SUBJID) ) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_SP_M.csv"),row.names = FALSE)
## Raw data of PP verification trials
data_info %>% select(PSA_ID, SEED) %>%
  mutate_if(is.integer, as.character) %>%
  inner_join(rawdata_PP, by=c("SEED" = "LAB_SEED")) %>% 
  #filter( (SEED %in% validation_df$SEED) & (subject_nr %in% validation_df$SUBJID) ) %>%
  filter(paste(SEED, subject_nr) %in% paste(validation_df$SEED,validation_df$SUBJID) ) %>%
  write.csv(file=paste0(old_path,"/1_raw_data/rawdata_PP.csv"),row.names = FALSE)

