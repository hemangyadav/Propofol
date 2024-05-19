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
                    
library(tidyverse)library(MASS)library(sfsmisc)library(data.table) library(ggplot2) library(dplyr) library(tidyverse) library(arsenal)library(readxl)library(writexl)data <- read_csv('TglMeasuredPropofol_wPanc.csv')table_panc <- tableby(Panc_Categories ~ HospitalLOS +	ICULOS + InvasiveVentUse + InvasiveVentDays + NonInvasiveVentUse + NonInvasiveVentDays + HFNCInICU + HFNCVentDays + HospitalDischargeLocation + CRRT, data=data, test = TRUE, numeric.stats=c("Nmiss", "mean", "sd"), total = FALSE)summary(table_panc, text = TRUE)write2word(table_panc, "table_panc.doc", title="PancBreakdown")        #Logistic      data$FirstTriglyceridesInICU_adj <- data$FirstTriglyceridesInICU / 100data$WorstTriglyceridesInICU_adj <- data$WorstTriglyceridesInICU / 100data$PropofolCumulative_adj <- data$PropofolCumulative / 1000data$PropofolbyDay_weight <- data$PropofolCumulative/data$Propofol.Cumulative.Time.In.ICU.Minutes/data$Weight.Kg*1440PropofolAdjustedLogisitic1 <- glm(acute_pancreatitis_icu ~                                    PropofolbyDay_weight, data = data, family = "binomial")exp(confint.default(PropofolAdjustedLogisitic1))exp(coef((PropofolAdjustedLogisitic1)))summary(PropofolAdjustedLogisitic1)#Binary Sample Logistic      PropofolAdjustedLogisitic1 <- glm(acute_pancreatitis_icu ~ Age +                                  Sex +                                   BMI +                                  HTN +                                  DM +                                  SOFA24 +                                  CovidPositive + TriglyceridesHyper +                                  FirstGlucoseInICU +                                  FirstLactateInICU +                                  FirstWBCInICU +                                  PropofolCumulative, data = data, family = "binomial")library(car)vif_results <- vif(PropofolAdjustedLogisitic1)print(vif_results)# Extract coefficientscoefs <- exp(coef(PropofolAdjustedLogisitic1))# Extract confidence intervalsCIs <- exp(confint.default(PropofolAdjustedLogisitic1))# Extract p-valuesp_values <- summary(PropofolAdjustedLogisitic1)$coefficients[, "Pr(>|z|)"]# Combine coefficients, confidence intervals, and p-values into a data frameresults_df <- data.frame(  Coefficient = names(coefs),  Coef_Value = coefs,  Lower_CI = CIs[, 1],  Upper_CI = CIs[, 2],  p_value = p_values)