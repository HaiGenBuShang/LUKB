##file to split one task into sub-tasks

args <- commandArgs(trailingOnly = TRUE)
library(tidyverse)

file_prefix <- args[[1]]
number_of_core <- as.integer(args[[2]])

dat_file <- paste0("Results/",file_prefix,"_input_data.RData")
parameter_file <- paste0("Results/",file_prefix,"_parameters.RData")



load(dat_file);load(parameter_file)

dat_for_HR %>% distinct(pid) %>% arrange(pid) %>% mutate(row_n=row_number()) %>% 
  mutate(core_assigned=cut(row_n,breaks=number_of_core) %>% as.integer()) %>% 
  group_split(core_assigned) %>% lapply(function(x){
    save(x,file = paste0("Results/",file_prefix,"_HR_disease_part",x$core_assigned %>% unique(),".RData"))
  })

system(paste("bash utilities/run_HR_analysis.sh",file_prefix))

