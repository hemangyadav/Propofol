if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, here, arsenal, purrr, janitor, tidyr, "MatchIt", dplyr)    # Load pre-requisite packages

Sys.setlocale("LC_ALL","English")
rm(list = ls())     # CLEAR GLOBAL ENVIRONMENT (RESET R)

setwd(here())        # Auto-set work directory to the parent folder
source("propofol_utils.R")                # Load utility functions

r_data <- fread("./data/23-007416 Main Data File.csv", header=TRUE, colClasses="character")
f_data <- r_data %filter_by% read.csv("./data/def_columns.csv")

r_data_pancreatitis <- fread("./data/pancreatitis.csv", header=TRUE, colClasses="character")
f_data_pancreatitis <- r_data_pancreatitis %filter_by% read.csv("./data/def_columns_pancreatitis.csv")

# Filter dataset to keep needed columns only
source("propofol_filtering_code.R")

m_data <- prop_match(data = f_data, group_by = "hypertgl",
                     ratio = 2, match_by = match_var) %filter_by% export_vars
 
 #Acute pancreatitis regression sample
                    
library(tidyverse)