# Shape the online data and merge with lab raw data
# Run this code after "data_validation.R"

# load required packages
if(!require(tidyverse)) {install.packages("tidyverse");library(tidyverse)} else (library(tidyverse))

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English")

# locate lab meta file
meta_path <- getwd() %>%
  paste0("/0_log/") %>%
  list.files(pattern="jatos_meta_[0-9]{14}.csv",all.files = TRUE,full.names = TRUE, recursive = TRUE,include.dirs = TRUE)

# locate lab rawdata files
data_path <- getwd() %>%
  paste0("/1_raw_data") %>%
  list.files(pattern="jatos_results_[0-9]{14}.csv",all.files = TRUE,full.names = TRUE, recursive = TRUE,include.dirs = TRUE)

# import lab meta file
jatos_metas <- meta_path %>%
  map_df(~read_csv(.))

# import lab rawdata files
osweb_rawdata <- data_path %>%
  map_df(~read_csv(.))

# Merge meta data and rawdata
all_rawdata <- jatos_metas %>% right_join(osweb_rawdata, by=c(`Result ID` = "jatosStudyResultId"))

# participants' identity code, language proficency, post survey
online_meta <- all_rawdata %>% filter(!is.na(response_Survey_response)) %>%
  select(`Result ID`,Batch,identifier, lang_prof,Question,response_Survey_response) %>%
  spread(Question,response_Survey_response) %>%
  rename(gender="Your gender?<br/>Press the number or key representing your answer",
         digit3= "The third digit of your birth year?<br/>e.g., if you were born in 2001, press \"0\".",
         digit4= "The  fourth digit of your birth year?<br/>e.g., if you were born in 2001, press \"1\".") %>%
  mutate(birth_year = paste0(digit3,digit4)) %>%
  select(Batch, `Result ID`,identifier, lang_prof, gender, birth_year)

online_meta %>% write_csv(file = "jatos_meta.csv")



# Isolate SP data
## Processing verification data
jatos_SP_V <- all_rawdata %>% filter(Task=="SP") %>%
  select(Batch, title, datetime, sessionid, `Result ID`, jatosVersion, List, Match, Orientation, subject_parity, Probe, Target, response_time, correct, opensesame_codename, opensesame_version) %>%
  rename(PSA_ID = Batch) %>%
  rename(subject_nr = `Result ID`) %>%
  rename(SEED = title ) %>%
  rename(logfile = sessionid) %>%
  rename(task_order = jatosVersion) %>%
  rename(PList = subject_parity)

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
  write_csv(file = "1_raw_data/rawdata_SP_V.csv")

## Processing memory check data
jatos_SP_M <- all_rawdata %>% filter(Task=="SP") %>%
  select(Batch, `Result ID`, starts_with("check_")) %>%
  gather(key = "memory_check", value = "trial_seq",-Batch, -`Result ID`) %>%
  distinct() %>%
  arrange(`Result ID`, trial_seq) %>%
  inner_join(all_rawdata, by=c("Batch","Result ID","trial_seq") ) %>%
  select(Batch, title, datetime, sessionid, `Result ID`, jatosVersion, List, Match, Orientation, subject_parity, Probe, Target, response_time, correct_Probe_sti_response, opensesame_codename, opensesame_version, starts_with("check_")) %>%
  rename(PSA_ID = Batch) %>%
  rename(subject_nr = `Result ID`) %>%
  rename(SEED = title ) %>%
  rename(logfile = sessionid) %>%
  rename(task_order = jatosVersion) %>%
  rename(PList = subject_parity) %>%
  rename(correct = correct_Probe_sti_response ) %>%
  unite("alt_task",starts_with("check_"),sep = ",")


jatos_SP_M$SEED = as.numeric(as.factor(jatos_SP_M$SEED))
jatos_SP_M$logfile = as.character(jatos_SP_M$logfile)
jatos_SP_M$List = as.character(jatos_SP_M$List)

## Bind with lab data
(rawdata_SP_M <- read_csv(file = "1_raw_data/rawdata_SP_M.csv") %>% bind_rows(jatos_SP_M)) %>%
  write_csv(file = "1_raw_data/rawdata_SP_M.csv")


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
  write_csv(file = "1_raw_data/rawdata_PP.csv")
