##for disease disease pair binomial and directional test

args <- commandArgs(trailingOnly = TRUE)

file_prefix <- args[[1]]

s_time <- Sys.time()
cat("Started at",format(s_time, "%Y_%m_%d_%H%M%S"),"\n")

library(tidyverse)
library(lubridate)
library(igraph)
source("R/functions_for_disease_network.R")

dat_file <- paste0("Results/",file_prefix,"_input_data.RData")
parameter_file <- paste0("Results/",file_prefix,"_parameters.RData")
HR_res_file <- paste0("Results/",file_prefix,"_HR_analysis_combined.RData")

load(dat_file);load(parameter_file);load(HR_res_file)


##Here we filter the disease that HR larger than 1,
##which means the target disease have risk effects for other diseases
HR_disease <- HR_res %>% arrange(desc(HR)) %>% filter(p_adj<0.05,HR_p_adj<0.05,HR>1)


#Here we use binomial distribution and binomial test to determine 
#if target disease is associated with the occurrence for other diseases in temporal order 
binomial_res <- di_traj_binomial(disease_records = dat_for_HR,target_disease = t_disease,cox_passed_disease = HR_disease$pid,
                                 start_date = start_date,end_date = end_date,
                                 min_prevalence_to_include_disease = min_prevalence,
                                 min_two_disease_gap = min_two_disease_gap)


#Here we only included diseases that the probability of binomial distribution and p-value of binomial test are significant,
#and the probability of getting disease B after disease A is larger than probability of only getting disease B
# disease_pair_to_exam <- binomial_res %>% filter(pr_A_to_B_adj<0.05,p_val_A_to_B_adj<0.05) %>% filter(uplift>1)

#Here we only included diseases that the probability of binomial distribution and p-value of binomial test are significant
disease_pair_to_exam <- binomial_res %>% filter(p_val_A_to_B_adj<0.05) %>% filter(uplift>1)


#Here we do direction,
#we only include the diseases pairs that the p-value of D_A -> D_B is significant but D_B -> D_A is not. 
d_pair_direction_test_res <- di_traj_direction_test(disease_records = dat_for_HR,
                                                    disease_pair_dat = disease_pair_to_exam,
                                                    start_date = start_date,
                                                    end_date = end_date)

d_pair_passed_direction <- d_pair_direction_test_res %>% filter(d_test_p_A_B_adj<0.05)

g <- graph_from_data_frame(d_pair_passed_direction %>% select(disease_A,disease_B))

save(d_pair_passed_direction,file = paste0("Results/",file_prefix,"_disease_pair_test_pairs.RData"))
save(g,file = paste0("Results/",file_prefix,"_disease_pair_test_graph.RData"))

e_time <- Sys.time()
cat("Ended at",format(e_time, "%Y_%m_%d_%H%M%S"),"\n")

cat("Costed",format((e_time-s_time) %>% as.difftime(units="mins")),"\n")

cat("Disease pair analysis finished!")






