args <- commandArgs(trailingOnly = TRUE)

library(tidyverse)
library(data.table)
library(lubridate)

input_file <- args[[1]]
output_file <- args[[2]]

dat_for_HR <- fread(file = input_file,header = TRUE) %>% as_tibble() %>% mutate(across(3,as_date))

save(dat_for_HR,file = output_file)
