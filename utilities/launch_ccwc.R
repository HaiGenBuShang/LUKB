#this file is for launch case-control matching and conditional analysis
args <- commandArgs(trailingOnly = TRUE)

file_prefix <- args[[1]]
number_of_core <- as.integer(args[[2]])


library(tidyverse)


dat_file <- paste0("Results/",file_prefix,"_input_data.RData")
parameter_file <- paste0("Results/",file_prefix,"_parameters.RData")
disease_pair_file <- paste0("Results/",file_prefix,"_disease_pair_test_pairs.RData")

load(dat_file);load(parameter_file);load(disease_pair_file)



# d_pair_passed_direction %>% select(disease_A,disease_B) %>% mutate(total_pairs=n()) %>% 
#   mutate(r_n=row_number()) %>% 
#   mutate(n_pair_for_each_core=total_pairs/number_of_core) %>% 
#   mutate(group=ceiling(r_n/n_pair_for_each_core)) %>% split(.$group) %>% 
#   lapply(function(x){
#     save(x,#dat_for_HR,start_date,end_date,N_control,min_prevalence,min_two_disease_gap,variable_for_ccwc,
#          file = paste0(file_prefix,"_part",(x$group %>% unique),"_ccwc_splited.RData"))
#   })



d_pair_passed_direction %>% select(disease_A,disease_B) %>% mutate(r_n=row_number()) %>% 
  mutate(group=cut(r_n,breaks=number_of_core) %>% as.integer()) %>% 
  split(.$group) %>% 
  lapply(function(x){
    save(x,#dat_for_HR,start_date,end_date,N_control,min_prevalence,min_two_disease_gap,variable_for_ccwc,
         file = paste0("Results/",file_prefix,"_ccwc_part",(x$group %>% unique),".RData"))
  })

system(paste("bash utilities/run_ccwc.sh",file_prefix))

