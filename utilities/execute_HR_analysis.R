args <- commandArgs(trailingOnly = TRUE)

s_time <- Sys.time()
cat("Started at",format(s_time, "%Y_%m_%d_%H%M%S"),"\n")

library(tidyverse)
library(lubridate)

source("R/functions_for_disease_network.R")

file_prefix <- args[[1]]
disease_file <- args[[2]]


dat_file <- paste0("Results/",file_prefix,"_input_data.RData")
parameter_file <- paste0("Results/",file_prefix,"_parameters.RData")

load(dat_file);load(parameter_file);load(disease_file)

time_varying_dat <- prepare_cox_data(disease_records = dat_for_HR,target_disease = t_disease,include_disease=x$pid,
                                     start_date = start_date,end_date = end_date,
                                     min_prevalence_to_include_disease = min_prevalence,
                                     min_two_disease_gap = min_two_disease_gap,
                                     variable_included_for_cox_data=variable_for_cox)
cat("time_varing_dat prepared!\n")

HR_res <- cox_with_time_varing_var(prepared_cox_dat = time_varying_dat, adjust_variable = variable_for_cox)

analysis_part <- disease_file %>% str_replace_all(".*_(part[0-9]*)\\..*","\\1")

save(HR_res,file = paste0("Results/",file_prefix,"_HR_analysis_",analysis_part,".RData"))

e_time <- Sys.time()
cat("Ended at",format(e_time, "%Y_%m_%d_%H%M%S"),"\n")

cat("Costed",format((e_time-s_time) %>% as.difftime(units="mins")),"\n")

cat("HR analysis",analysis_part,"finished!\n")

