##functions


#HR bewteen any two disease


# #parameters
# target_disease <- "D12"
# # data <- dat
# data <- raw %>% as_tibble() %>% mutate(value=as_date(value)) #%>% filter(value>as_date("2020-01-01"))
# 
# #filter_out_date <- as_date("2020-01-01")
# 
# start_date <- as_date("2020-01-01") #start date considered as the start of fellowup
# end_date <- data$value %>% as_date() %>% max() #end date considered as the end of fellowup
# 
# min_prevalence_to_include_disease <- 0.01
# 
# 
# 
# 
# start_date <- as_date(start_date)
# end_date <- as_date(end_date)
# 
# #data preparing
# data_1 <- data %>% mutate(value=as_date(value)) %>% 
#   mutate(d_date=if_else(pid==target_disease,value,NA),d_state=if_else(pid==target_disease,1,0)) %>% 
#   group_by(eid) %>% fill(d_date,.direction = "updown") %>% mutate(d_state=max(d_state)) %>% 
#   mutate(start_date=as_date(start_date),end_date=as_date(end_date)) %>% ungroup()
# 
# max_end_date <- max(c(end_date, data$value %>% as_date() %>% max()))
# 
# #data for those have any diseases after the start_date
# #this data is used for produce the included people
# included_pop <- data_1 %>% filter(value>=start_date) %>% distinct(eid)
# 
# #date info of people have target disease and its corresponding date
# target_disease_pop_info <- data_1 %>% filter(d_state==1) %>% select(eid,d_state,d_date,start_date,end_date) %>% 
#   distinct() %>% 
#   mutate(d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
# 
# 
# #whole data for disease trajectory 
# data_2 <- included_pop %>% left_join(data_1,by="eid") %>% 
#   mutate(d_to_disease=if_else(is.na(d_date),value-start_date,value-d_date)) %>% 
#   mutate(end_date=max_end_date,d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
# 
# data_3 <- data_2 %>% group_split(pid)
# 
# 
# cox_res <- data_3 %>% sapply(function(x){
#   
#   #include those have diseases before specific disease occurs
#   tmp_1 <- included_pop %>% 
#     #to exclude people that have disease before specific disease occurs
#     anti_join(x %>% filter(d_to_disease<0|(!d_date_between_start_and_end)),by="eid") %>% 
#     #to exclude people that have specific diseases before the start date or after the end date
#     anti_join(target_disease_pop_info %>% filter(!d_date_between_start_and_end),by="eid") %>% 
#     left_join(x,by="eid") %>% filter(value>start_date|is.na(d_date))
#   
#   if(tmp_1 %>% filter(!is.na(pid)) %>% nrow() / tmp_1 %>% nrow > 0.01){
#     cox_dat <- tmp_1 %>% mutate(d_1_status=if_else(is.na(pid),0,1)) %>% mutate(pid=unique(pid) %>% na.omit()) %>% 
#       select(eid,pid,d_1_status,d_1_date=value) %>% 
#       left_join(target_disease_pop_info %>% select(eid,d_state,d_date),by="eid") %>% 
#       mutate(d_state=if_else(is.na(d_state),0,d_state)) %>% 
#       mutate(start_date=start_date,end_date=max_end_date) %>% 
#       mutate(d_to_d_1_days=if_else(is.na(d_1_date)&is.na(d_date),end_date-start_date,NA)) %>% 
#       mutate(d_to_d_1_days=if_else(is.na(d_1_date)&(!is.na(d_date)),end_date-d_date,d_to_d_1_days)) %>% 
#       mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&is.na(d_date),d_1_date-start_date,d_to_d_1_days)) %>% 
#       mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&(!is.na(d_date)),d_1_date-d_date,d_to_d_1_days))
#     
#     res <- survival::coxph(Surv(d_to_d_1_days,d_1_status)~d_state, #more variable should be added such as age, sex
#                            data=cox_dat)
#     
#     res_sum <- summary(res)
#     pval <- res_sum$waldtest["pvalue"] %>% setNames(NULL)
#     c(pid=cox_dat$pid %>% unique,pvalue=pval,HR=res_sum$coefficients[1,2],
#       CI_low=res_sum$conf.int[1,3],CI_upp=res_sum$conf.int[1,4],n=cox_dat %>% nrow())
#   }else{
#     NULL
#   }
#   
# })




##############################################################################
#HR function that did not consider the former disease as time varying variable
#Which was deprecated in our condition
##############################################################################
# library(tidyverse)
# di_traj_HR_analysis <- function(disease_records,target_disease,
#                                 start_date,end_date=max(as_date(disease_records %>% pull(3))),
#                                 min_prevalence_to_include_disease=0.01,
#                                 min_two_disease_gap=0,
#                                 adjust_variable=NULL){
#   
#   message("Note: the first three columns should contain individual IDs, disease codes, and date of getting diseases!")
#   
#   data <- disease_records %>% rename(eid=1,pid=2,value=3)%>% mutate(value=as_date(value)) %>% as_tibble()
#   
#   start_date <- as_date(start_date)
#   end_date <- as_date(end_date)
#   
#   #data preparing
#   data_1 <- data %>% mutate(value=as_date(value)) %>% 
#     mutate(d_date=if_else(pid==target_disease,value,NA),d_state=if_else(pid==target_disease,1,0)) %>% 
#     group_by(eid) %>% fill(d_date,.direction = "updown") %>% mutate(d_state=max(d_state)) %>% 
#     mutate(start_date=as_date(start_date),end_date=as_date(end_date)) %>% ungroup()
#   
#   # max_end_date <- max(c(end_date, data$value %>% as_date() %>% max()))
#   max_end_date <- end_date
#   
#   #data for those have any diseases after the start_date
#   #this data is used for produce the included people
#   included_pop <- data_1 %>% filter(value>=start_date) %>% distinct(eid)
#   
#   #date info of people have target disease and its corresponding date
#   target_disease_pop_info <- data_1 %>% filter(d_state==1) %>% select(eid,d_state,d_date,start_date,end_date) %>% 
#     distinct() %>% 
#     mutate(d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
#   
#   
#   #whole data for disease trajectory 
#   data_2 <- included_pop %>% left_join(data_1,by="eid") %>% 
#     mutate(d_to_disease=if_else(is.na(d_date),value-start_date,value-d_date)) %>% 
#     mutate(end_date=max_end_date,d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
#   
#   data_3 <- data_2 %>% 
#     #filter out the target disease because this disease is not should be calculated
#     #if do not filter out, an error will occur when do fisher.test
#     filter(pid!=target_disease) %>% 
#     group_split(pid)
#   
#   
#   cox_res <- data_3 %>% sapply(function(x){
#     
#     #include those have diseases before specific disease occurs
#     tmp_1 <- included_pop %>% 
#       #to exclude people that have disease before specific disease occurss
#       anti_join(x %>% filter(d_to_disease<0|(!d_date_between_start_and_end)),by="eid") %>% 
#       #to exclude people that have specific diseases before the start date or after the end date
#       anti_join(target_disease_pop_info %>% filter(!d_date_between_start_and_end),by="eid") %>% 
#       # left_join(x,by="eid") %>% filter(value>start_date|is.na(d_date)) %>% 
#     
#     
#       left_join(x %>% select(-any_of(adjust_variable)),by="eid") %>% filter(value>start_date|is.na(d_date)) %>%
#       # mutate(pid_for_match=unique(pid) %>% na.omit()) %>%
#       left_join(data %>% select(eid,any_of(adjust_variable)) %>% distinct(),by="eid")
#       
#     
#     if(tmp_1 %>% filter(!is.na(pid)) %>% nrow() / tmp_1 %>% nrow > 0.01){
#       cox_dat <- tmp_1 %>% mutate(d_1_status=if_else(is.na(pid),0,1)) %>% mutate(pid=unique(pid) %>% na.omit()) %>% 
#         select(eid,pid,d_1_status,d_1_date=value,any_of(adjust_variable)) %>% 
#         left_join(target_disease_pop_info %>% select(eid,d_state,d_date),by="eid") %>% 
#         mutate(d_state=if_else(is.na(d_state),0,d_state)) %>% 
#         mutate(start_date=start_date,end_date=max_end_date) %>% 
#         mutate(d_to_d_1_days=if_else(is.na(d_1_date)&is.na(d_date),end_date-start_date,NA)) %>% 
#         mutate(d_to_d_1_days=if_else(is.na(d_1_date)&(!is.na(d_date)),end_date-d_date,d_to_d_1_days)) %>% 
#         mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&is.na(d_date),d_1_date-start_date,d_to_d_1_days)) %>% 
#         mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&(!is.na(d_date)),d_1_date-d_date,d_to_d_1_days)) %>% 
#         filter(d_to_d_1_days>=min_two_disease_gap) %>% 
#         #add one column that how many days from the start date to the status date
#         mutate(start_to_d_1_days=if_else(is.na(d_1_date),end_date-start_date,d_1_date-start_date))
#         
#       
#       print(cox_dat$pid %>% unique() %>% na.omit)
#       fisher_res <- cox_dat %>% xtabs(~d_1_status+d_state,.) %>% fisher.test()
#       
#       # res <- survival::coxph(Surv(d_to_d_1_days,d_1_status)~d_state, #more variable should be added such as age, sex
#       #                        data=cox_dat)
#       # res <- survival::coxph(as.formula(paste(c("Surv(d_to_d_1_days,d_1_status)~d_state",adjust_variable),
#       #                                         collapse ="+")),data=cox_dat)
#       
#       
#       
#       #For the coxph part, as d_state is changing according to the time,
#       #Thus, a better solution is considering the d_state is a time-varying variable,
#       #and use time-varying methods to do cox regression
#       #see reference: PMC6015946
#       
#       
#       res <- survival::coxph(as.formula(paste(c("Surv(start_to_d_1_days,d_1_status)~d_state",adjust_variable),
#                                               collapse ="+")),data=cox_dat)
#       
#       
#       
#       res_sum <- summary(res)
#       pval <- res_sum$waldtest["pvalue"] %>% setNames(NULL)
#       c(pid=cox_dat$pid %>% unique,pvalue=pval,HR=res_sum$coefficients[1,2],
#         CI_low=res_sum$conf.int[1,3],CI_upp=res_sum$conf.int[1,4],n=cox_dat %>% nrow(),
#         f_OR=fisher_res$estimate %>% setNames(NULL),f_pvalue=fisher_res$p.value)
#     }else{
#       NULL
#     }
#   })
#   
#   cox_res[!cox_res %>% sapply(is.null)] %>% bind_rows() %>% 
#     mutate(HR=as.numeric(HR),pvalue=as.numeric(pvalue),
#            f_OR=as.numeric(f_OR),f_pvalue=as.numeric(f_pvalue)) %>% 
#     mutate(p_adj=p.adjust(pvalue,method="bon"),f_p_adj=p.adjust(f_pvalue,method="bon"))
# }


# dat_for_analysis <- dat %>% filter(did=="U071") %>% select(1:3) %>% rename(pid=did) %>% bind_rows(raw)
# a <- di_traj_HR_analysis(disease_records = dat_for_analysis,target_disease = "U071",start_date = "2020-01-01",
#                          end_date = max(dat_for_analysis$value),min_prevalence_to_include_disease = 0.01)


#The di_traj_HR_analysis function were splited into two functions
# to reuse the data it produced
# ##############################################################################
# #HR function that did not consider the former disease as time varying variable
# #Which was deprecated in our condition
# ##############################################################################
# library(tidyverse)
# di_traj_HR_analysis <- function(disease_records,target_disease,
#                                 start_date,end_date=max(as_date(disease_records %>% pull(3))),
#                                 min_prevalence_to_include_disease=0.01,
#                                 min_two_disease_gap=0,
#                                 adjust_variable=NULL){
#   
#   message("Note: the first three columns should contain individual IDs, disease codes, and date of getting diseases!")
#   
#   data <- disease_records %>% rename(eid=1,pid=2,value=3)%>% mutate(value=as_date(value)) %>% as_tibble()
#   
#   if(!any(target_disease%in%(disease_records$pid %>% unique()))){
#     stop("The endpoint disease did not appear in your input data!")
#   }
#   
#   start_date <- as_date(start_date)
#   end_date <- as_date(end_date)
#   
#   #data preparing
#   data_1 <- data %>% mutate(value=as_date(value)) %>% 
#     mutate(d_date=if_else(pid==target_disease,value,NA),d_state=if_else(pid==target_disease,1,0)) %>% 
#     group_by(eid) %>% fill(d_date,.direction = "updown") %>% mutate(d_state=max(d_state)) %>% 
#     mutate(start_date=as_date(start_date),end_date=as_date(end_date)) %>% ungroup()
#   
#   # max_end_date <- max(c(end_date, data$value %>% as_date() %>% max()))
#   max_end_date <- end_date
#   
#   #data for those have any diseases after the start_date
#   #this data is used for produce the included people
#   included_pop <- data_1 %>% filter(value>=start_date) %>% distinct(eid)
#   
#   #date info of people have target disease and its corresponding date
#   target_disease_pop_info <- data_1 %>% filter(d_state==1) %>% select(eid,d_state,d_date,start_date,end_date) %>% 
#     distinct() %>% 
#     mutate(d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
#   
#   
#   #whole data for disease trajectory 
#   data_2 <- included_pop %>% left_join(data_1,by="eid") %>% 
#     mutate(d_to_disease=if_else(is.na(d_date),value-start_date,value-d_date)) %>% 
#     mutate(end_date=max_end_date,d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
#   
#   data_3 <- data_2 %>% 
#     #filter out the target disease because this disease is not should be calculated
#     #if do not filter out, an error will occur when do fisher.test
#     filter(pid!=target_disease) %>% 
#     group_split(pid)
#   
#   
#   cox_res <- data_3 %>% sapply(function(x){
#     
#     #include those have diseases before specific disease occurs
#     tmp_1 <- included_pop %>% 
#       #to exclude people that have disease before specific disease occurss
#       anti_join(x %>% filter(d_to_disease<0|(!d_date_between_start_and_end)),by="eid") %>% 
#       #to exclude people that have specific diseases before the start date or after the end date
#       anti_join(target_disease_pop_info %>% filter(!d_date_between_start_and_end),by="eid") %>% 
#       # left_join(x,by="eid") %>% filter(value>start_date|is.na(d_date)) %>% 
#       
#       
#       left_join(x %>% select(-any_of(adjust_variable)),by="eid") %>% filter(value>start_date|is.na(d_date)) %>%
#       # mutate(pid_for_match=unique(pid) %>% na.omit()) %>%
#       left_join(data %>% select(eid,any_of(adjust_variable)) %>% distinct(),by="eid")
#     
#     
#     if(tmp_1 %>% filter(!is.na(pid)) %>% nrow() / tmp_1 %>% nrow > min_prevalence_to_include_disease){
#       cox_dat_1 <- tmp_1 %>% mutate(d_1_status=if_else(is.na(pid),0,1)) %>% mutate(pid=unique(pid) %>% na.omit()) %>% 
#         select(eid,pid,d_1_status,d_1_date=value,any_of(adjust_variable)) %>% 
#         left_join(target_disease_pop_info %>% select(eid,d_state,d_date),by="eid") %>% 
#         mutate(d_state=if_else(is.na(d_state),0,d_state)) %>% 
#         mutate(start_date=start_date,end_date=max_end_date) %>% 
#         mutate(d_to_d_1_days=if_else(is.na(d_1_date)&is.na(d_date),end_date-start_date,NA)) %>% 
#         mutate(d_to_d_1_days=if_else(is.na(d_1_date)&(!is.na(d_date)),end_date-d_date,d_to_d_1_days)) %>% 
#         mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&is.na(d_date),d_1_date-start_date,d_to_d_1_days)) %>% 
#         mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&(!is.na(d_date)),d_1_date-d_date,d_to_d_1_days)) %>% 
#         filter(d_to_d_1_days>=min_two_disease_gap) %>% 
#         #add one column that how many days from the start date to the status date
#         mutate(start_to_d_1_days=if_else(is.na(d_1_date),end_date-start_date,d_1_date-start_date))
#       
#       
#       #For the coxph part, as d_state is changing according to the time,
#       #Thus, a better solution is considering the d_state is a time-varying variable,
#       #and use time-varying methods to do cox regression
#       #see reference: PMC6015946
#       
#       cox_dat_2 <- cox_dat_1 %>% select(eid,pid,d_1_status,start_to_d_1_days,any_of(adjust_variable)) %>% 
#         #add 0.1 to for those getting disease right in the day of the start date
#         #or tmgerge wouldnot work
#         mutate(start_to_d_1_days=if_else(start_to_d_1_days==0,start_to_d_1_days+0.1,start_to_d_1_days)) %>% 
#         
#         tmerge(data1=.,data2 = .,id=eid,event=event(start_to_d_1_days,d_1_status))
#       
#       
#       target_dat <- cox_dat_1 %>% select(eid,d_state,d_date,start_date) %>% 
#         mutate(start_to_d_days=d_date-start_date)
#       
#       
#       # cox_dat <- tmerge(data1 = cox_dat_2,data2 = target_dat,id=eid,target_d_condition=tdc(start_to_d_days,d_state))
#       cox_dat <- tmerge(data1 = cox_dat_2,data2 = target_dat,id=eid,target_d_condition=tdc(start_to_d_days))
#       
#       print(cox_dat$pid %>% unique() %>% na.omit)
#       fisher_res <- cox_dat_1 %>% xtabs(~d_1_status+d_state,.) %>% fisher.test()
#       
#       
#       res <- survival::coxph(as.formula(paste(c("Surv(tstart,tstop,event)~target_d_condition",adjust_variable),
#                                               collapse ="+")),data=cox_dat,cluster = eid)
#       
#       res_sum <- summary(res)
#       #do not use Wald Test but the Likelihood Ratio Test
#       #since it is more reliable for most conditions
#       #but for clustered data, Wald Test is more reliable
#       pval <- res_sum$waldtest["pvalue"] %>% setNames(NULL)
#       # pval <- res_sum$logtest["pvalue"] %>% setNames(NULL)
#       
#       c(pid=cox_dat$pid %>% unique,pvalue=pval,HR=res_sum$coefficients[1,2],
#         CI_low=res_sum$conf.int[1,3],CI_upp=res_sum$conf.int[1,4],n=cox_dat %>% nrow(),
#         f_OR=fisher_res$estimate %>% setNames(NULL),f_pvalue=fisher_res$p.value)
#     }else{
#       NULL
#     }
#   })
#   
#   cox_res[!cox_res %>% sapply(is.null)] %>% bind_rows() %>% 
#     mutate(HR=as.numeric(HR),pvalue=as.numeric(pvalue),
#            f_OR=as.numeric(f_OR),f_pvalue=as.numeric(f_pvalue)) %>% 
#     mutate(p_adj=p.adjust(pvalue,method="bon"),f_p_adj=p.adjust(f_pvalue,method="bon"))
# }

#*********************************
#*The first splited function
#*it is used to produce data
#*containing time varying variable
#*********************************
#The target disease were considered as a time varying variable
prepare_cox_data <- function(disease_records,target_disease,include_disease=NULL,
                             start_date,end_date=max(as_date(disease_records %>% pull(3))),
                             min_prevalence_to_include_disease=0.01,
                             min_two_disease_gap=0,
                             variable_included_for_cox_data=NULL,for_cwcc=FALSE){
  message("Note: the first three columns should contain individual IDs, disease codes, and date of getting diseases!")
  
  data <- disease_records %>% rename(eid=1,pid=2,value=3)%>% mutate(value=as_date(value)) %>% as_tibble()
  
  if(!any(target_disease%in%(disease_records$pid %>% unique()))){
    stop("The endpoint disease did not appear in your input data!")
  }
  
  start_date <- as_date(start_date)
  end_date <- as_date(end_date)
  
  print(paste0("target disease is: ",target_disease))
  
  #data preparing
  data_1 <- data %>% mutate(value=as_date(value)) %>% 
    mutate(d_date=if_else(pid==target_disease,value,NA),d_state=if_else(pid==target_disease,1,0)) %>% 
    group_by(eid) %>% fill(d_date,.direction = "updown") %>% mutate(d_state=max(d_state)) %>% 
    mutate(start_date=as_date(start_date),end_date=as_date(end_date)) %>% ungroup()
  
  # max_end_date <- max(c(end_date, data$value %>% as_date() %>% max()))
  max_end_date <- end_date
  
  #data for those have any diseases after the start_date
  #this data is used for produce the included people
  included_pop <- data_1 %>% filter(value>=start_date,value<=end_date) %>% distinct(eid)
  
  #date info of people have target disease and its corresponding date
  target_disease_pop_info <- data_1 %>% filter(d_state==1) %>% select(eid,d_state,d_date,start_date,end_date) %>% 
    distinct() %>% 
    mutate(d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
  
  
  #whole data for disease trajectory 
  data_2 <- included_pop %>% left_join(data_1,by="eid") %>% 
    mutate(d_to_disease=if_else(is.na(d_date),value-start_date,value-d_date)) %>% 
    mutate(end_date=max_end_date,d_date_between_start_and_end=d_date>=start_date&d_date<=end_date)
  
  data_3 <- data_2 %>% 
    #filter out the target disease because this disease is not should be calculated
    #if do not filter out, an error will occur when do fisher.test
    filter(pid!=target_disease) %>% 
    filter(if(is.null(include_disease)){TRUE}else{pid%in%include_disease}) %>% 
    group_split(pid)
  
  cox_res <- data_3 %>% lapply(function(x){
    
    #include those have diseases before specific disease occurs
    # tmp_1 <- included_pop %>% 
    #   #to exclude people that have disease before specific disease occurss
    #   anti_join(x %>% filter(d_to_disease<0|(!d_date_between_start_and_end)),by="eid") %>% 
    #   #to exclude people that have specific diseases before the start date or after the end date
    #   anti_join(target_disease_pop_info %>% filter(!d_date_between_start_and_end),by="eid") %>% 
    #   # left_join(x,by="eid") %>% filter(value>start_date|is.na(d_date)) %>% 
    #   
    #   
    #   left_join(x %>% select(-any_of(variable_included_for_cox_data)),by="eid") %>% 
    #   filter(value>start_date|is.na(d_date)) %>%
    #   # mutate(pid_for_match=unique(pid) %>% na.omit()) %>%
    #   left_join(data %>% select(eid,any_of(variable_included_for_cox_data)) %>% distinct(),by="eid")
    
    tmp_1 <- included_pop %>% 
      
      #to exclude people that have specific diseases before the start date or after the end date
      anti_join(target_disease_pop_info %>% filter(!d_date_between_start_and_end),by="eid") %>% 
      # left_join(x,by="eid") %>% filter(value>start_date|is.na(d_date)) %>% 
      
      left_join(x %>% select(-any_of(variable_included_for_cox_data)),by="eid") %>% 
      
      #to exclude people that got the diseases before start date 
      #did not consider the order of disease and specific disease occurring
      filter(value>=start_date|is.na(pid)) %>%
      # mutate(pid_for_match=unique(pid) %>% na.omit()) %>%
      left_join(data %>% select(eid,any_of(variable_included_for_cox_data)) %>% distinct(),by="eid")
    
    
    if((tmp_1 %>% filter(!is.na(pid)) %>% nrow() / tmp_1 %>% nrow > min_prevalence_to_include_disease)|(for_cwcc)){
      cox_dat_1 <- tmp_1 %>% mutate(d_1_status=if_else(is.na(pid),0,1)) %>% mutate(pid=unique(pid) %>% na.omit()) %>% 
        select(eid,pid,d_1_status,d_1_date=value,any_of(variable_included_for_cox_data)) %>% 
        left_join(target_disease_pop_info %>% select(eid,d_state,d_date),by="eid") %>% 
        mutate(d_state=if_else(is.na(d_state),0,d_state)) %>% 
        mutate(start_date=start_date,end_date=max_end_date) %>% 
        mutate(d_to_d_1_days=if_else(is.na(d_1_date)&is.na(d_date),end_date-start_date,NA)) %>% 
        mutate(d_to_d_1_days=if_else(is.na(d_1_date)&(!is.na(d_date)),end_date-d_date,d_to_d_1_days)) %>% 
        mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&is.na(d_date),d_1_date-start_date,d_to_d_1_days)) %>% 
        mutate(d_to_d_1_days=if_else((!is.na(d_1_date))&(!is.na(d_date)),d_1_date-d_date,d_to_d_1_days)) %>% 
        
        # filter(d_to_d_1_days>=min_two_disease_gap) %>% 
        
        #add one column that how many days from the start date to the status date
        mutate(start_to_d_1_days=if_else(is.na(d_1_date),end_date-start_date,d_1_date-start_date))
      
      
      #For the coxph part, as d_state is changing according to the time,
      #Thus, a better solution is considering the d_state is a time-varying variable,
      #and use time-varying methods to do cox regression
      #see reference: PMC6015946
      
      cox_dat_2 <- cox_dat_1 %>% select(eid,pid,d_1_status,start_to_d_1_days,any_of(variable_included_for_cox_data)) %>% 
        #add 0.1 to for those getting disease right in the day of the start date
        #or tmgerge wouldnot work
        mutate(start_to_d_1_days=if_else(start_to_d_1_days==0,start_to_d_1_days+0.1,start_to_d_1_days)) %>% 
        
        tmerge(data1=.,data2 = .,id=eid,event=event(start_to_d_1_days,d_1_status))
      
      
      target_dat <- cox_dat_1 %>% select(eid,d_state,d_date,start_date) %>% 
        mutate(start_to_d_days=d_date-start_date)
      
      print(cox_dat_2$pid %>% unique() %>% na.omit)
      # cox_dat <- tmerge(data1 = cox_dat_2,data2 = target_dat,id=eid,target_d_condition=tdc(start_to_d_days,d_state))
      cox_dat <- tmerge(data1 = cox_dat_2,data2 = target_dat,id=eid,target_d_condition=tdc(start_to_d_days))
      
    }else{
      NULL
    }
  })
  
}

#*********************************
#*The second splited function
#*it is used to do fisher test and
#*cox regression
#*********************************

cox_with_time_varing_var <- function(prepared_cox_dat,adjust_variable=c("age_at_start","sex")){
  cox_res <- prepared_cox_dat %>% sapply(function(cox_dat){
    if(is.null(cox_dat)){
      NULL
    }else{
      print(cox_dat$pid %>% unique() %>% na.omit)
      
      fisher_res <- cox_dat %>% group_by(eid) %>% 
        summarise(d_1_status=max(d_1_status),target_d_condition=max(target_d_condition)) %>% 
        xtabs(~d_1_status+target_d_condition,.) %>% fisher.test()
      
      res <- survival::coxph(as.formula(paste(c("Surv(tstart,tstop,event)~target_d_condition",adjust_variable),
                                              collapse ="+")),data=cox_dat,cluster = eid)
      
      res_sum <- summary(res)
      #do not use Wald Test but the Likelihood Ratio Test
      #since it is more reliable for most conditions
      #but for clustered data, Wald Test is more reliable
      pval <- res_sum$waldtest["pvalue"] %>% setNames(NULL)
      # pval <- res_sum$logtest["pvalue"] %>% setNames(NULL)
      
      c(pid=cox_dat$pid %>% unique,pvalue=pval,HR=res_sum$coefficients[1,2],
        CI_low=res_sum$conf.int[1,3],CI_upp=res_sum$conf.int[1,4],n=cox_dat %>% nrow(),
        HR_p=res_sum$coefficients[1,6],
        f_OR=fisher_res$estimate %>% setNames(NULL),f_pvalue=fisher_res$p.value)
    }
  })
  
  cox_res[!cox_res %>% sapply(is.null)] %>% bind_rows() %>% 
    mutate(HR=as.numeric(HR),HR_p=as.numeric(HR_p),pvalue=as.numeric(pvalue),
           f_OR=as.numeric(f_OR),f_pvalue=as.numeric(f_pvalue)) %>% 
    mutate(HR_p_adj=p.adjust(HR_p,method="bon"),p_adj=p.adjust(pvalue,method="bon"),
           f_p_adj=p.adjust(f_pvalue,method="bon"))
}















# disease_records <- dat_for_HR
# target_disease <- "U071"
# cox_passed_disease <- HR_disease %>% pull(pid)
# start_date <- as_date("2020-01-01")




#disease trajectories
di_traj_binomial <- function(disease_records,target_disease,cox_passed_disease,
                             start_date,end_date=max(as_date(disease_records %>% pull(3))),
                             min_prevalence_to_include_disease=0.01,
                             min_two_disease_gap=0){
  
  message("Note: the first three columns should contain individual IDs, disease codes, and date of getting diseases!")
  
  if(!any(target_disease%in%(disease_records$pid %>% unique()))){
    stop("The endpoint disease did not appear in your input data!")
  }
  
  data <- disease_records %>% rename(eid=1,pid=2,value=3)%>% mutate(value=as_date(value)) %>% as_tibble()
  
  #total individuals number considered in disease trajectory analysis
  total_n <- data %>% filter(value>=start_date,value<=end_date) %>% pull(eid) %>% n_distinct()
  
  #only target and cox passed disease were selected.
  #this step will decrease the total number of individuals in the next steps
  data_to_start <- data %>% filter(pid%in%c(target_disease,cox_passed_disease)) %>% select(1:3)
  
  # #data frame combine two diseases
  # data_1 <- data_to_start %>% left_join(data_to_start,by="eid",relationship = "many-to-many") %>% 
  #   rename(disease_A=2,disease_B=4,date_A=3,date_B=5) %>% filter(disease_A!=disease_B) %>% 
  #   mutate(gap_A_to_B=date_B-date_A) %>% arrange(eid) %>% 
  #   #filter out disease B occurred before disease A
  #   filter(gap_A_to_B>0) %>% 
  #   #filter out disease pairs that occurred too close
  #   filter(gap_A_to_B>=min_two_disease_gap) %>% 
  #   #filter out diseases occurred before start and after end date
  #   filter(date_A>=start_date) %>% filter(date_A<=end_date,date_B<=end_date)
  
  
  #data frame combine two diseases
  data_1 <- data_to_start %>% left_join(data_to_start,by="eid",relationship = "many-to-many") %>% 
    rename(disease_A=2,disease_B=4,date_A=3,date_B=5) %>% filter(disease_A!=disease_B) %>% 
    mutate(gap_A_to_B=date_B-date_A) %>% arrange(eid) %>% 
    #filter out diseases occurred before start and after end date
    filter(date_A>=start_date) %>% filter(date_A<=end_date,date_B<=end_date)
  
  #count for each disease
  d_count <- data_1 %>% distinct(eid,disease_A) %>% count(disease_A)
  
  #count individuals with disease B occurring before disease A and disease B occurring after disease A too close
  n_B_before_A <- data_1 %>% group_by(disease_A,disease_B) %>% summarise(n=sum(gap_A_to_B<max(0,min_two_disease_gap)))
  
  
  
  #this data_1 is for trajectory calculation
  data_1 <- data_1 %>% 
    #filter out disease B occurred before disease A
    filter(gap_A_to_B>0) %>% 
    #filter out disease pairs that occurred too close
    filter(gap_A_to_B>=min_two_disease_gap)
  
  # #count for each disease
  # d_count <- data_1 %>% distinct(eid,disease_A) %>% count(disease_A)
  
  #produce the data for binomial test
  data_2 <- d_count %>% rename(nA=n) %>% 
    left_join(data_1 %>% group_by(disease_A,disease_B) %>% summarise(nAB=n()),by="disease_A") %>% 
    left_join(d_count %>% rename(nB=n),by=c("disease_B"="disease_A")) %>% 
    # mutate(n=data_1 %>% distinct(eid) %>% nrow()) %>% mutate(p_A=nA/n,p_B=nB/n) %>% mutate(p_B_A=nAB/(nA)) %>% 
    
    #adjust the total individual count methods
    #We consider all the individuals but for specific disease pair,
    #we exclude the individuals that B occurred before A
    mutate(n=total_n) %>% left_join(n_B_before_A,by=c("disease_A","disease_B")) %>% 
    mutate(n=n.x-n.y) %>% select(-n.x,-n.y) %>% 
    mutate(p_A=nA/n,p_B=nB/n) %>% mutate(p_B_A=nAB/(nA)) %>% 
    # mutate(uplift=p_B_A/p_A) %>% 
    mutate(uplift=p_B_A/p_B) %>% 
    #filter out disease with low prevalence
    filter(p_A>min_prevalence_to_include_disease,p_B>min_prevalence_to_include_disease)
  
  #do binomial test
  data_3 <- data_2 %>% 
    
    #probability of getting B after getting A
    mutate(pr_A_to_B=dbinom(nAB,nA,p_B)) %>% 
    
    #p-value that the probability of binomial test
    #which tests for null hypothesis that the success probability is less than p_B (prevalence B)
    mutate(p_val_A_to_B=pmap_dbl(list(nAB,nA,p_B),~binom.test(..1,..2,..3,alternative = "greater")$p.value)) %>% 
    mutate(pr_A_to_B_adj=p.adjust(pr_A_to_B,method="bonferroni"),
           p_val_A_to_B_adj=p.adjust(p_val_A_to_B,method="bonferroni")) 
  
  data_3
}






#disease trajectory direction test
di_traj_direction_test <- function(disease_records,disease_pair_dat,start_date,
                                   end_date=max(as_date(disease_records %>% pull(3)))){
  
  message(paste0("Note: the first three columns of disease records should contain individual IDs,",
                 " disease codes, and date of getting diseases!"))
  
  data <- disease_records %>% rename(eid=1,pid=2,value=3)%>% mutate(value=as_date(value)) %>% as_tibble()
  
  diseases <- disease_pair_dat %>% select(disease_A,disease_B) %>% 
    pivot_longer(cols = 1:2,names_to = "disease_type",values_to = "diseases") %>% pull(diseases) %>% unique()
  
  #only disease occurred in the start and end interval was considered
  #and because only the individuals getting both A and B will be included,
  #some individuals that have diseases out side the start and end interval will not be considered
  #thus we can considered individuals with diseases occurred in the the start and end interval
  data_to_start <- data %>% filter(pid%in%diseases) %>% select(1:3) %>% 
    filter(value>=start_date,value<=end_date)
  
  
  
  #data frame combine two diseases
  data_1 <- data_to_start %>% left_join(data_to_start,by="eid",relationship = "many-to-many") %>% 
    rename(disease_A=2,disease_B=4,date_A=3,date_B=5) %>% filter(disease_A!=disease_B) %>% 
    mutate(gap_A_to_B=date_B-date_A) %>% arrange(eid)
  
  #count individuals with disease B occurring before disease A and disease B occurring after disease A too close
  n_B_before_A <- data_1 %>% group_by(disease_A,disease_B) %>% summarise(n_B_A=sum(gap_A_to_B<0))
  n_A_before_B <- data_1 %>% group_by(disease_A,disease_B) %>% summarise(n_A_B=sum(gap_A_to_B>0))
  n_A_and_B <- data_1 %>% group_by(disease_A,disease_B) %>% summarise(n_A_B_simul=sum(gap_A_to_B==0))
  
  
  data_for_directionilty <- reduce(list(n_A_before_B,n_B_before_A,n_A_and_B),left_join,by=c("disease_A","disease_B")) %>% 
    ungroup %>% 
    #the gap A to B equal to 0 days, we cannot exactly know which disease occurred first
    #thus we use only the individuals exactly know the occurring order
    mutate(n=n_B_A+n_A_B) %>% 
    
    #only included the disease pair that we need
    #so that we will not do additional p value adjusting
    semi_join(disease_pair_dat,by=c("disease_A","disease_B"))
  
  directionilty_test <- data_for_directionilty %>% 
    mutate(p_A_B=map2_dbl(n_A_B,n,~(binom.test(.x,.y,p=0.5,alternative="greater")$p.value))) %>% 
    mutate(p_B_A=map2_dbl(n_B_A,n,~(binom.test(.x,.y,p=0.5,alternative="greater")$p.value))) %>% 
    mutate(p_A_B_adj=p.adjust(p_A_B,method="bon"),p_B_A_adj=p.adjust(p_B_A,method="bon")) %>% 
    rename_with(.fn = ~paste0("d_test_",.x),.cols = -1:-2)
  
  disease_pair_dat %>% 
    left_join(directionilty_test, by=c("disease_A","disease_B"))
  
}







#produce case control dataset
library(Epi)
library(survival)
di_traj_cc_dataset <- function(disease_pair_dat,disease_records,start_date="2020-01-01",
                               end_date = max(as_date(disease_records[[3]])),n_controls=2,
                               min_prevalence_to_include_disease = 0.01,min_two_disease_gap = 1,
                               variable_included_for_cox_data=c("age_at_start","sex","age_group")){
  message("Note: the first two columns should contain the first disease and the second disease in order!")
  
  data <- disease_pair_dat %>% select(1:2) %>% rename(disease_A=1,disease_B=2) %>% 
    group_by(disease_A) %>% nest()
  
  # #prepare data to generating case control dataset
  data_1 <- data %>% mutate(prepared_dat=map2(disease_A,data,~(
    prepare_cox_data(disease_records = disease_records,target_disease = .x,include_disease=.y[[1]],
                     start_date = start_date,end_date = end_date,
                     min_prevalence_to_include_disease = min_prevalence_to_include_disease,
                     min_two_disease_gap = min_two_disease_gap,
                     variable_included_for_cox_data=variable_included_for_cox_data,for_cwcc = TRUE)
  ))) %>% unnest(cols = c(data,prepared_dat))
  
  match_call <- as.call(c(list(as.name("list")), variable_included_for_cox_data %>% lapply(as.name)))
  
  # data_2 <- data_1 %>% mutate(cc_dataset=map(prepared_dat,~(
  #   .x %>%
  #     ccwc_m(entry = tstart,exit = tstop,fail = event,origin = 0,controls = n_controls,
  #            match = match_call,
  #            include = list(eid,pid,target_d_condition),data = .,silent=TRUE)
  # )))
  
  data_2 <- data_1 %>% mutate(cc_dataset=map2(prepared_dat,disease_A,~{
    print(paste0("disease_A is: ",.y,"; disease_B is ",unique(.x$pid)))
    .x %>%
      ccwc_m(entry = tstart,exit = tstop,fail = event,origin = 0,controls = n_controls,
             match = match_call,
             include = list(eid,pid,target_d_condition),data = .,silent=TRUE)
  }))
  
}


#********************
#*mutile core version
#********************

# #produce case control dataset
# library(Epi)
# library(parallel)
# di_traj_cc_dataset <- function(disease_pair_dat,disease_records,start_date="2020-01-01",
#                                end_date = max(as_date(disease_records[[3]])),n_controls=2,
#                                min_prevalence_to_include_disease = 0.01,min_two_disease_gap = 1,
#                                variable_included_for_cox_data=c("age_at_start","sex","age_group"),
#                                n_core=1){
#   message("Note: the first two columns should contain the first disease and the second disease in order!")
#   
#   data <- disease_pair_dat %>% select(1:2) %>% rename(disease_A=1,disease_B=2) %>% 
#     group_by(disease_A) %>% nest()
#   
#   # # #prepare data to generating case control dataset
#   # data_1 <- data %>% mutate(prepared_dat=map2(disease_A,data,~(
#   #   prepare_cox_data(disease_records = disease_records,target_disease = .x,include_disease=.y[[1]],
#   #                    start_date = start_date,end_date = end_date,
#   #                    min_prevalence_to_include_disease = min_prevalence_to_include_disease,
#   #                    min_two_disease_gap = min_two_disease_gap,
#   #                    variable_included_for_cox_data=variable_included_for_cox_data)
#   # ))) %>% unnest(cols = c(data,prepared_dat))
#   
#   #assign cores for data
#   data <- data %>% ungroup %>% mutate(n_row=map_dbl(data,~nrow(.x))) %>% mutate(total_n=sum(n_row)) %>% 
#     mutate(n_row_for_each_core=total_n/n_core) %>% mutate(task_accumulate=cumsum(n_row)) %>% 
#     mutate(group=ceiling(task_accumulate/n_row_for_each_core)) %>% select(disease_A,data,group)
#   
#   
#   data_1 <- data %>% group_split(group) %>% 
#     parallel::mclapply(function(x){
#       x %>% mutate(prepared_dat=map2(disease_A,data,~(
#         prepare_cox_data(disease_records = disease_records,target_disease = .x,include_disease=.y[[1]],
#                          start_date = start_date,end_date = end_date,
#                          min_prevalence_to_include_disease = min_prevalence_to_include_disease,
#                          min_two_disease_gap = min_two_disease_gap,
#                          variable_included_for_cox_data=variable_included_for_cox_data)
#       ))) %>% unnest(cols = c(data,prepared_dat))
#     },mc.cores = n_core,mc.set.seed = 12345678)
#   
#   
#   match_call <- as.call(c(list(as.name("list")), variable_included_for_cox_data %>% lapply(as.name)))
#   
#   # data_2 <- data_1 %>% mutate(cc_dataset=map(prepared_dat,~(
#   #   .x %>% 
#   #     ccwc_m(entry = tstart,exit = tstop,fail = event,origin = 0,controls = n_controls,
#   #            match = match_call, 
#   #            include = list(eid,pid,target_d_condition),data = .,silent=TRUE)
#   # )))
#   
#   data_1 <- data_1 %>% 
#     parallel::mclapply(function(x){
#       x %>% mutate(cc_dataset=map(prepared_dat,~(
#         .x %>% 
#           ccwc_m(entry = tstart,exit = tstop,fail = event,origin = 0,controls = n_controls,
#                  match = match_call, 
#                  include = list(eid,pid,target_d_condition),data = .,silent=TRUE)
#       )))
#     },mc.cores = n_core,mc.set.seed = 12345678)
#   
#   data_1 %>% bind_rows()
# }






di_traj_clogistic <- function(data_cc_dataset){
  data <- data_cc_dataset
  data %>% mutate(c_log_res=map(cc_dataset,~(
    clogistic(Fail~target_d_condition,data = .x,strata = Set)
  ))) %>% 
    mutate(clog_HR=map_dbl(c_log_res,~(.x$coefficients[1] %>% exp)),
           clog_p=map_dbl(c_log_res,~(1-pchisq(-2*(.x$loglik[1]-.x$loglik[2]),
                                               df=sum(!is.na(.x$coefficients)))))) %>% 
    select(-prepared_dat,-cc_dataset,-c_log_res)
}






# net_cc_dat <- test %>% filter(pbrn<0.05) %>% as_tibble() %>% 
#   mutate(cc_dat=map2(pidB,pidA,~(
#     confirm_dat %>% group_by(eid) %>% 
#       summarise(pid_A_time=max(if_else(pidA==.y,timeA,as_date(0))),
#                 pid_B_time=max(if_else(pidB==.x,timeB,as_date(0)))) %>% 
#       mutate(pid_A_status=if_else(pid_A_time>pid_B_time,1,0),d
#              pid_B_status=if_else(pid_B_time!=as_date(0),1,0)) %>% 
#       left_join(raw_2 %>% filter(pid==.y) %>% select(eid,sports_disease_diff),by="eid") %>% 
#       left_join(max_fol_up,by="eid") %>% 
#       mutate(fellow_up=if_else(is.na(sports_disease_diff),max_fellow_up,sports_disease_diff)) %>% 
#       left_join(d_s_y %>% select(eid,sex,sports_age,birth_decades),by="eid") %>% 
#       ccwc(entry=0,exit = fellow_up,fail=pid_A_status,origin = 0,
#            controls = 5,include = list(pid_B_status,sex,sports_age,birth_decades,eid),
#            data = .,silent = TRUE)
#   )
#   ))
# 
# net_cc_res <- net_cc_dat %>% 
#   mutate(c_logistic=map(cc_dat,~clogistic(Fail~pid_B_status+sex+sports_age+birth_decades,strata = Set,data = .x))) %>% 
#   mutate(clog_HR=map_dbl(c_logistic,~(.x$coefficients[1] %>% exp)),
#          clog_p=map_dbl(c_logistic,~(1-pchisq(-2*(.x$loglik[1]-.x$loglik[2]),
#                                               df=sum(!is.na(.x$coefficients))))))










ccwc_m <- function (entry = 0, exit, fail, origin = 0, controls = 1, match = list(), 
          include = list(), data = NULL, silent = FALSE) 
{
  entry <- eval(substitute(entry), data)
  exit <- eval(substitute(exit), data)
  fail <- eval(substitute(fail), data)
  origin <- eval(substitute(origin), data)
  n <- length(fail)
  if (length(exit) != n) 
    stop("All vectors must have same length")
  if (length(entry) != 1 && length(entry) != n) 
    stop("All vectors must have same length")
  if (length(origin) == 1) {
    origin <- rep(origin, n)
  }
  else {
    if (length(origin) != n) 
      stop("All vectors must have same length")
  }
  t.entry <- as.numeric(entry - origin)
  t.exit <- as.numeric(exit - origin)
  # marg <- substitute(match)
  marg <- match
  if (mode(marg) == "name") {
    match <- list(eval(marg, data))
    names(match) <- as.character(marg)
  }
  else if (mode(marg) == "call" && marg[[1]] == "list") {
    mnames <- names(marg)
    nm <- length(marg)
    if (nm > 1) {
      if (!is.null(mnames)) {
        for (i in 2:nm) {
          if (mode(marg[[i]]) == "name") 
            mnames[i] <- as.character(marg[[i]])
          else stop("illegal argument (match)")
        }
      }
      else {
        for (i in 2:nm) {
          if (mode(marg[[i]]) == "name") 
            mnames[i] <- as.character(marg[[i]])
          else stop("illegal argument (match)")
        }
        mnames[1] <= ""
      }
    }
    names(marg) <- mnames
    match <- eval(marg, data)
  }
  else {
    stop("illegal argument (match)")
  }
  m <- length(match)
  mnames <- names(match)
  if (m > 0) {
    for (i in 1:m) {
      if (length(match[[i]]) != n) {
        stop("incorrect length for matching variable")
      }
    }
  }
  iarg <- substitute(include)
  if (mode(iarg) == "name") {
    include <- list(eval(iarg, data))
    names(include) <- as.character(iarg)
  }
  else if (mode(iarg) == "call" && iarg[[1]] == "list") {
    ni <- length(iarg)
    inames <- names(iarg)
    if (ni > 1) {
      if (!is.null(inames)) {
        for (i in 2:ni) {
          if (mode(iarg[[i]]) == "name") 
            inames[i] <- as.character(iarg[[i]])
          else stop("illegal argument (include)")
        }
      }
      else {
        for (i in 2:ni) {
          if (mode(iarg[[i]]) == "name") 
            inames[i] <- as.character(iarg[[i]])
          else stop("illegal argument (include)")
        }
        inames[1] <= ""
      }
    }
    names(iarg) <- inames
    include <- eval(iarg, data)
  }
  else {
    stop("illegal argument (include)")
  }
  ni <- length(include)
  inames <- names(include)
  if (ni > 0) {
    for (i in 1:ni) {
      if (length(include[[i]]) != n) {
        stop("incorrect length for included variable")
      }
    }
  }
  grp <- rep(1, n)
  pd <- 1
  if (m > 0) {
    for (im in 1:m) {
      v <- match[[im]]
      if (length(v) != n) 
        stop("All vectors must have same length")
      if (!is.factor(v)) 
        v <- factor(v)
      grp <- grp + pd * (as.numeric(v) - 1)
      pd <- pd * length(levels(v))
    }
  }
  nn <- (1 + controls) * sum(fail != 0)
  pr <- numeric(nn)
  sr <- numeric(nn)
  tr <- vector("numeric", nn)
  fr <- numeric(nn)
  nn <- 0
  if (!silent) {
    cat("\nSampling risk sets: ")
  }
  set <- 0
  nomatch <- 0
  incomplete <- 0
  ties <- FALSE
  fg <- unique(grp[fail != 0])
  for (g in fg) {
    ft <- unique(t.exit[(grp == g) & (fail != 0)])
    for (tf in ft) {
      if (!silent) {
        cat(".")
      }
      set <- set + 1
      case <- (grp == g) & (t.exit == tf) & (fail != 0)
      ncase <- sum(case)
      if (ncase > 1) 
        ties <- TRUE
      noncase <- (grp == g) & (t.entry <= tf) & (t.exit >= 
                                                   tf) & !case
      ncont <- controls * ncase
      if (ncont > sum(noncase)) {
        ncont <- sum(noncase)
        if (ncont > 0) 
          incomplete <- incomplete + 1
      }
      if (ncont > 0) {
        newnn <- nn + ncase + ncont
        sr[(nn + 1):newnn] <- set
        tr[(nn + 1):newnn] <- tf
        fr[(nn + 1):(nn + ncase)] <- 1
        fr[(nn + ncase + 1):newnn] <- 0
        pr[(nn + 1):(nn + ncase)] <- (1:n)[case]
        noncase.id <- (1:n)[noncase]
        pr[(nn + ncase + 1):(newnn)] <- noncase.id[sample.int(length(noncase.id), 
                                                              size = ncont)]
        nn <- newnn
      }
      else {
        nomatch <- nomatch + ncase
      }
    }
  }
  if (!silent) {
    cat("\n")
  }
  res <- vector("list", 4 + m + ni)
  if (nn > 0) {
    res[[1]] <- sr[1:nn]
    res[[2]] <- map <- pr[1:nn]
    res[[3]] <- tr[1:nn] + origin[map]
    res[[4]] <- fr[1:nn]
  }
  if (m > 0) {
    for (i in 1:m) {
      res[[4 + i]] <- match[[i]][map]
    }
  }
  if (ni > 0) {
    for (i in 1:ni) {
      res[[4 + m + i]] <- include[[i]][map]
    }
  }
  names(res) <- c("Set", "Map", "Time", "Fail", mnames, inames)
  if (incomplete > 0) 
    warning(paste(incomplete, "case-control sets are incomplete"))
  if (nomatch > 0) 
    warning(paste(nomatch, "cases could not be matched"))
  if (ties) 
    warning("there were tied failure times")
  data.frame(res)
}





# track_task_status <- function(directory,
#                               log_file_prefix,log_file_regexpr,needed_log_file_number,
#                               log_content_for_success,
#                               message_for_still_running,message_for_finished){
#   log_files <- list.files(path = directory,pattern = paste0(log_file_prefix,log_file_regexpr),full.names = TRUE)
#   if(length(log_files) < needed_log_file_number){
#     validate(message_for_still_running)
#   }else{
#     log_contents <- sapply(log_files,function(x){
#       fread(x,sep="\n",header = FALSE) %>% tail(n=1) %>% pull(1)
#     })
#     if(log_contents %>% str_detect(log_content_for_success) %>% sum() < needed_log_file_number){
#       validate(message_for_still_running)
#     }else{
#       message_for_finished
#     }
#   }
# }


track_task_status <- function(directory,
                              log_file_prefix,log_file_regexpr,needed_log_file_number,
                              log_content_for_success,
                              message_for_still_running,message_for_finished){
  log_files <- list.files(path = directory,pattern = paste0(log_file_prefix,log_file_regexpr),full.names = TRUE)
  
  if(sum(file.size(log_files)!=0,na.rm = TRUE)<needed_log_file_number){
    validate(message_for_still_running)
  }else if(needed_log_file_number==0) {
    validate("Please start analyzing task first!")
  }else{
    log_contents <- sapply(log_files,function(x){
      fread(x,sep="\n",header = FALSE) %>% tail(n=1) %>% pull(1)
    })
    if(log_contents %>% str_detect(log_content_for_success) %>% sum() < needed_log_file_number){
      validate(message_for_still_running)
    }else{
      message_for_finished
    }
  }
}

# download_RData <- function(file_name,user,authorised_user){
download_RData <- function(file_name,user,authorised_user){
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #It seems downloadHandler function is kind of different!
  #the variable within this function seems must be reactive() format
  #otherwise, the reactive value would always be the first reactive value appeared in reactive()
  
  #So, the file_name, user parameter in this function must be in reactive format.
  
  downloadHandler(
    filename = function(){
      file_name() %>% str_remove_all(".*/")
    },
    content = function(file){
      if(any(user()%in%authorised_user)){
        field_log(field_from_file = file_name(),log_file = paste0("Logs/",user(),"_downloaded_Rdata.txt"))
        
        file.copy(file_name(), file)
      }else{
        # write.table("You have no permission of downloading. Please contact your LUKB administrator", 
        #             file,col.names = FALSE,row.names = FALSE,quote = FALSE)
        No_permission <- "You have no permission of downloading. Please contact your LUKB administrator"
        save(No_permission,file = file)
      }
    },
    contentType = "txt"
  )
}



target_disease_number <- function(dat,target_disease,start_date,end_date){
  dat %>% filter(pid==target_disease) %>% group_by(value) %>% summarise(n=n()) %>% 
    ggplot()+
    geom_col(mapping = aes(x=value,y=n,text=paste0("n:&nbsp;",n,"<br>date:&nbsp;",value))) + 
    geom_vline(xintercept = as_date(c(start_date,end_date)) %>% as.numeric(),linetype=2,color="red") +
    xlab("Date")+ylab("Individual Number") + 
    ggtitle("Target Disease Cases")+
    # labs(title = "**Target Disease Cases**")+
    theme(axis.text = element_text(size=10),axis.title = element_text(size=10),
          plot.title=element_text(hjust = 0.5,size=10,face = "bold"))
  # plot.title=ggtext::element_markdown(hjust = 0.5,size=10,face = "bold"))
}




include_disease <- function(dat,min_prevalence){
  total_n <- dat %>% distinct(eid) %>% nrow
  dat %>% group_by(pid) %>% summarise(disease_n=n()) %>% mutate(total_n=total_n) %>% mutate(prevalence=disease_n/total_n) %>% 
    mutate(type=if_else(prevalence>=min_prevalence,"Included","Excluded")) %>% 
    ggplot() +
    geom_bar(mapping = aes(x=fct_infreq(type) %>% fct_relevel("Excluded",after = Inf)),width = 0.5) + 
    xlab("Diseases")+ylab("Disease Number")+
    ggtitle("Included and Excluded Disease Numbers")+
    # labs(title = "**Included and Excluded Disease Numbers**")+
    theme(axis.text = element_text(size=10),axis.title = element_text(size=10),
          plot.title=element_text(hjust = 0.5,size=10,face = "bold"))
}


individual_disease_counted <- function(dat,start_date,end_date,min_disease_gap,target_disease){
  # disease_per_ind <- dat %>% distinct(eid) %>% #head(10) %>% 
  #   left_join(dat %>% filter(value>=start_date,value<=end_date) %>% 
  #               #filter(eid%in%c(1000077,1001647,1003685)) %>% 
  #               mutate(d_date=if_else(pid==target_disease,value,NA)) %>% 
  #               group_by(eid) %>% fill(d_date,.direction = "downup") %>% ungroup() %>% 
  #               mutate(days_to_next_disease=value-d_date) %>% filter(days_to_next_disease>min_disease_gap) %>% count(eid),
  #             by="eid") %>% mutate(n=replace_na(n,0))
  disease_per_ind <- dat %>% filter(value>=start_date,value<=end_date) %>% count(eid)
  library(scales)
  disease_per_ind %>% count(n,name = "num") %>% 
    ggplot() +
    geom_col(mapping = aes(x=n,y=num,text=paste0("Individuals:&nbsp;",num,"<br>Diseases:&nbsp;",n))) + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    xlab("Number of Diseases per Individual") + ylab("Individual Number (log10)")+
    ggtitle("Number of Individuals with Different Numbers of Diseases")+
    theme(axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=5),
          axis.title = element_text(size=10),
          plot.title=element_text(hjust = 0.5,size=10,face = "bold"))
}


case_control_num <- function(dat,target_disease,start_date,end_date,n_controls){
  cases <- dat %>% filter(pid==target_disease) %>% filter(value>=start_date,value<=end_date) %>% nrow()
  controls <- cases*n_controls
  rbind(cases,controls) %>% as.data.frame() %>% rownames_to_column("type") %>% rename(number=2) %>% 
    ggplot() +
    geom_col(mapping = aes(x=type,y=number,text=paste0("n:&nbsp;",number)),width=0.5) +
    xlab("Type") + ylab("Individual Number") +
    ggtitle("Number of Cases and Controls for Case-Control Analysis")+
    theme(axis.text = element_text(size=10),axis.title = element_text(size=10),
          plot.title=element_text(hjust = 0.5,size=10,face = "bold"))
}

