args <- commandArgs(trailingOnly = TRUE)

file_prefix <- args[[1]]
disease_pairs_file <- args[[2]]

s_time <- Sys.time()
cat("Started at",format(s_time, "%Y_%m_%d_%H%M%S"),"\n")

library(tidyverse)
library(lubridate)
source("R/functions_for_disease_network.R")

dat_file <- paste0("Results/",file_prefix,"_input_data.RData")
parameter_file <- paste0("Results/",file_prefix,"_parameters.RData")


load(dat_file);load(parameter_file);load(disease_pairs_file)


set.seed(12345678) #set seed so that we can always get the same case-control dataset
case_control_dataset <- di_traj_cc_dataset(disease_pair_dat = x %>% select(disease_A,disease_B),
                                           disease_records = dat_for_HR,start_date = start_date,
                                           end_date = end_date,n_controls = N_control,
                                           min_prevalence_to_include_disease = min_prevalence,
                                           min_two_disease_gap = min_two_disease_gap,
                                           variable_included_for_cox_data=variable_for_ccwc)
c_log_res <- di_traj_clogistic(case_control_dataset)

save(case_control_dataset,c_log_res,file = disease_pairs_file %>% str_replace_all("\\.RData","_dataset.RData"))
save(c_log_res,file = disease_pairs_file %>% str_replace_all("\\.RData","_clog_res.RData"))


analysis_part <- disease_pairs_file %>% str_replace_all(".*(part[0-9]*).*","\\1")

# cat(file_for_ccwc %>% str_replace_all(".*(part[0-9]*).*","\\1"),"finished!")

e_time <- Sys.time()
cat("Ended at",format(e_time, "%Y_%m_%d_%H%M%S"),"\n")

cat("Costed",format((e_time-s_time) %>% as.difftime(units="mins")),"\n")

cat("CCWC analysis",analysis_part,"finished!")
