# Shape the online data and merge with lab raw data
# Run this code after "data_validation.R"

# load required packages
if(!require(tidyverse)) {install.packages("tidyverse");library(tidyverse)} else (library(tidyverse))
if(!require(magrittr)) {install.packages("magrittr");library(magrittr)} else (library(magrittr))
if(!require(lubridate)) {install.packages("lubridate");library(lubridate)} else (library(lubridate))
if(!require(stringr)) {install.packages("stringr");library(stringr)} else (library(stringr))


# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English")

setwd("../")

# locate lab meta file
meta_path <- getwd() %>%
  paste0("/1_raw_data") %>%
  list.files(pattern="jatos_meta_[0-9]{14}.csv",all.files = TRUE,full.names = TRUE, recursive = TRUE,include.dirs = TRUE)

# locate lab rawdata files
data_path <- getwd() %>%
  paste0("/1_raw_data") %>%
  list.files(pattern="jatos_results_[0-9]{14}.csv",all.files = TRUE,full.names = TRUE, recursive = TRUE,include.dirs = TRUE)  ## Erase the tab in the column names


# merge meta and data files path by path
#step = 0
all_rawdata <- NULL
for(meta_origin in meta_path) {
 # step = step + 1

  jatos_metas <- read_csv(meta_origin) # import JATOS meta files

  colnames(jatos_metas) = gsub(names(jatos_metas), pattern = " ",replacement = "")  # Remove blanks in column names.

  for(data_origin in data_path){
#    print(meta_origin)
#    print(data_origin)
   osweb_rawdata <- read_csv(data_origin)
   # Merge meta data and rawdata
   tmp_rawdata <- subset(jatos_metas, State == "FINISHED") %>%
##     filter(minute(parse_time(Duration)) > 5)  %>%
     left_join(osweb_rawdata, by=c(`Result ID` = "jatosStudyResultId")) %>%
     mutate(datetime = (datetime %>% substr(5,24) %>%  parse_date_time("%m/ %d/ %y/ HMS", tz = "GMT") - datetime %>% substr(29,33) %>% as.numeric()/100) %>% substr(1,10))  %>%
     ## select the available variables for the analysis
     ## 20210316 note: Some participants' post-study responses were incompatible with the whole data sheet. Use "count_Survey_response" to locate the post-study responses.
     ## 20210708 note: add "correct_Target_response","correct_Probe_sti_response" that store the correctness of verification responses and memory checks
     select(`Result ID`, Batch, identifier, lang_prof, Question, response_Survey_response, count_Survey_response, title, datetime, sessionid, Task, jatosVersion, List, Match, Orientation, subject_parity, Probe, Target, correct_Probe_sti_response, opensesame_codename, opensesame_version, starts_with("check_"), PPList, Orientation1, Orientation2, Identical, Picture1, Picture2, trial_seq, response_time, response_time_Target_response, correct, correct_Target_response)  ## 'response_time_Target_response' stored the original SPV response `response_time` stored the last response in a trial which were SPV response or memory check.

   tmp_rawdata$response_Survey_response <- as.character(tmp_rawdata$response_Survey_response)

   if(is.null(all_rawdata)){
     all_rawdata = tmp_rawdata
   } else {
     all_rawdata = bind_rows(all_rawdata, tmp_rawdata)
   }
 }

}


#names(all_rawdata)


# participants' identity code, language proficency, post survey
online_meta <- all_rawdata %>%
  filter(count_Survey_response %in% c(0,1,2)) %>%
  mutate(ID = paste0(Batch,"_",`Result ID`)) %>%
  select(Batch,ID,identifier, lang_prof,response_Survey_response) %>%
  mutate(PostQ = rep(c("gender","digit3","digit4"),length(ID)/3)) %>%
  distinct() %>%
  spread(key=PostQ, value=response_Survey_response, convert = TRUE) %>%
  mutate(birth_year = paste0(digit3,digit4)) %>%
  select(Batch, ID,identifier, lang_prof, gender, birth_year)

online_meta %>% write_csv(file = "1_raw_data/jatos_meta.csv")


# Isolate SP data
## Processing verification data
jatos_SP_V <- all_rawdata %>% filter(Task=="SP") %>%
  select(Batch, title, datetime, sessionid, `Result ID`, jatosVersion, List, Match, Orientation, subject_parity, Probe, Target, response_time_Target_response, correct_Target_response, opensesame_codename, opensesame_version) %>%
  rename(PSA_ID = Batch) %>%
  rename(subject_nr = `Result ID`) %>%
  rename(SEED = title ) %>%
  rename(logfile = sessionid) %>%
  rename(task_order = jatosVersion) %>%
  rename(PList = subject_parity) %>%
  rename(response_time = response_time_Target_response) %>% ## original SPV response time
  rename(correct = correct_Target_response)  ## original SPV correct

jatos_SP_V$SEED = as.numeric(as.factor(jatos_SP_V$SEED))
jatos_SP_V$logfile = as.character(jatos_SP_V$logfile)
jatos_SP_V$List = as.character(jatos_SP_V$List)


# mappings of rawdata in site and online
# SEED == title (no seed number for online data)
# logfile == sessionid (no individual log file from JATOS)
# task_order == jatosVersion
# PList == subject_parity (remove one PList because of OSWEB bug)

## Bind with lab data
(rawdata_SP_V <- read_csv(file = "1_raw_data/rawdata_SP_V.csv") %>% bind_rows(jatos_SP_V)) %>%
  write_csv(file = "1_raw_data/all_rawdata_SP_V.csv")

## Processing memory check data
jatos_SP_M <- all_rawdata %>% filter(Task=="SP") %>%
  select(Batch, `Result ID`, starts_with("check_")) %>%
  gather(key = "memory_check", value = "trial_seq",-Batch, -`Result ID`) %>%
  distinct() %>%
  arrange(`Result ID`, trial_seq) %>%
  inner_join(all_rawdata, by=c("Batch","Result ID","trial_seq") ) %>%
  select(Batch, title, datetime, sessionid, `Result ID`, jatosVersion, List, subject_parity, Probe, response_time, correct_Probe_sti_response, opensesame_codename,  starts_with("check_")) %>%
  rename(PSA_ID = Batch) %>%
  rename(subject_nr = `Result ID`) %>%
  rename(SEED = title ) %>%
  rename(logfile = sessionid) %>%
#  rename(task_order = jatosVersion) %>%
  rename(PList = subject_parity) %>%
  rename(correct = correct_Probe_sti_response ) %>%
  unite("alt_task",starts_with("check_"),sep = ",")


jatos_SP_M$SEED = as.character(as.factor(jatos_SP_M$SEED))
jatos_SP_M$logfile = as.character(jatos_SP_M$logfile)
jatos_SP_M$List = as.character(jatos_SP_M$List)

## Bind with lab data
rawdata_SP_M <- read_csv(file = "1_raw_data/rawdata_SP_M.csv")
## change the data type of some columns
rawdata_SP_M$SEED <- as.character(rawdata_SP_M$SEED)
rawdata_SP_M$logfile <- as.character(rawdata_SP_M$logfile)
rawdata_SP_M$List <- as.character(rawdata_SP_M$List)
## Combine and export all memory check data
(rawdata_SP_M %>% bind_rows(jatos_SP_M)) %>%
  write_csv(file = "1_raw_data/all_rawdata_SP_M.csv")


# Isolate PP data
jatos_PP <- all_rawdata %>% filter(Task=="PP") %>%
  select(Batch, title, datetime, sessionid, `Result ID`,PPList, Orientation1, Orientation2, Identical, Picture1, Picture2, response_time, correct, opensesame_codename, opensesame_version) %>%
  rename(PSA_ID = Batch) %>%
  rename(subject_nr = `Result ID`) %>%
  rename(SEED = title ) %>%
  rename(logfile = sessionid)

jatos_PP$SEED = as.numeric(as.factor(jatos_PP$SEED))
jatos_PP$logfile = as.character(jatos_PP$logfile)
jatos_PP$PPList = as.character(jatos_PP$PPList)


## Bind with lab data
(rawdata_PP <- read_csv(file = "1_raw_data/rawdata_PP.csv") %>% bind_rows(jatos_PP)) %>%
  write_csv(file = "1_raw_data/all_rawdata_PP.csv")

