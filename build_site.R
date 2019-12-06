#######################################################################
# R code accompanying Sau-Chin Chen et al. (2019)
# OSF projet: https://osf.io/e428p/
# ---------------------------------------------------------
# Download rawdata from lab OSF
# Written by Erin Buchanan
# Updated by Sau-Chin Chen
# E-mail: pmsp96@gmail.com
# Last update: October 17, 2019
#############################################################

library(tidyverse)
# Build the Site ----------------------------------------------------------
setwd(here::here() )

## Clean site when we have to add new lab data
rmarkdown::clean_site()

## Get the lab info from csv file 
## We stored lab info in the directory "log"
lab_info <- dirname(getwd()) %>%
    dir(full.names = TRUE, recursive = TRUE, include.dirs = TRUE, pattern = "Lab_info.csv")  %>%
    read.csv()

# Download the data
source("download_osf.R")

# Validate the Data
setwd(here::here() )
source("data_validation.R")

# Run the sequential analysis
source("data_seq_analysis.R")

# Knit the new files

#update the index file
rmarkdown::render("index.Rmd", output_format = "html_document", output_dir = "docs")

#update the sims
#turned this off since I don't have the sim data
#rmarkdown::render("Simdata.Rmd", output_format = "html_document", output_dir = "docs")

#update the about page
rmarkdown::render("about.Rmd", output_format = "html_document", output_dir = "docs")

#update the faq page
rmarkdown::render("faq.Rmd", output_format = "html_document", output_dir = "docs")


#update Languages
rmarkdown::render("English.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("TC.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("SC.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Norwegian.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Hungarian.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Polish.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Deutsch.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Thai.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Portuguesa.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Italian.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Serbian.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Spanish.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Tukish.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Slovak.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Hindi.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Greek.Rmd", output_format = "html_document", output_dir = "docs")
rmarkdown::render("Hebrew.Rmd", output_format = "html_document", output_dir = "docs")
