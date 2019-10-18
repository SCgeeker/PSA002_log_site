#######################################################################
# R code accompanying Sau-Chin Chen et al. (2019)
# OSF projet: https://osf.io/e428p/
# ---------------------------------------------------------
# Download rawdata from lab OSF
# Written by Sau-Chin Chen
# E-mail: pmsp96@gmail.com
# Last update: October 17, 2019
#############################################################

library(tidyverse)
library(data.table)
library(magrittr)

# Import multiple-bytes string in English system
Sys.setlocale("LC_ALL","English") 

##set working directory to current project folder
setwd(here::here())

## Sequential analysis, subset by the end sequence
require(BayesFactor)

BF_sequential <- function(DF, acc, scale=0.707){
  
  data <- DF %>%
    group_by(subject_nr) %>%
    subset(correct == 1)
  tmp_seq <- data %>% pull(subject_nr) %>% table() %>% unlist()
  tmp_ind <- data.frame(id = rep(names(tmp_seq) %>% as.numeric(), tmp_seq), end_seq = rep(names(tmp_seq) %>% order(), tmp_seq))
  data <- data %>% left_join(tmp_ind,by=c("subject_nr"="id")) %>%
    group_by(end_seq, subject_nr, Match) %>%
    summarise(V_RT = median(response_time, na.rm = TRUE), V_Accuracy = (n()/16) )
  
  bfProg <- matrix(0, ncol = 3, nrow = length(unique(data$end_seq)) )
  colnames(bfProg) <- c("ID","N", "BF")
  
  for(i in data$end_seq){
    
    ## Initial Point
    if(i < 3){
      bfProg[i, "ID"] <- as.numeric(names(tmp_seq)[i])
      bfProg[i, "N"] <- 1
      bfProg[i, "BF"] <- 1
    }
    
    if(i > 2){
      ## BF analysis for large and small objects
      ## Save the current sample size
      bfProg[i, "ID"] <- as.numeric(names(tmp_seq)[i])
      bfProg[i, "N"] <- i
      
      bfProg[i, "BF"] <- (ttestBF(
        x = data %>%
          filter(end_seq %in% (1:i), Match == "N") %>%
          pull(V_RT),
        y = data %>%
          filter(end_seq %in% (1:i), Match == "Y") %>%
          pull(V_RT),
        nullInterval = c(0,Inf),
        paired = TRUE,
        rscale = scale) %>%
          ## Return BF10 and BF01
          extractBF(onlybf = TRUE))[1] #%>%
      #round(2)
    }
    
    
  }
  
  data.frame(bfProg)
}

# Processing lab data
rawdata_SP_V <- dirname(getwd()) %>% 
                dir(pattern = "rawdata_SP_V", 
                    recursive = TRUE, full.names = TRUE) %>% read.csv

## Setup of randomization seed
set.seed(100)

## Run sequential analysis and export the result to lab folder
for(LAB in unique(rawdata_SP_V$PSA_ID)){
  route <- dirname(getwd()) %>% 
          dir(pattern = paste0(LAB,"$"), 
            recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
  
  BF_sequential(subset(rawdata_SP_V, PSA_ID == LAB)) %>%
    write.csv(file = paste0(route,"/Seq_output.csv"),row.names = FALSE)
}
