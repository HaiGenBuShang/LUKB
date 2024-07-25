library(tidyverse)
library(processx)

download_file <- function(file_name,user,authorised_user){
  
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
        field_log(field_from_file = file_name(),log_file = paste0("Logs/",user(),"_generated_file.txt"))
        
        file.copy(file_name(), file)
      }else{
        write.table("You have no permission of downloading. Please contact your LUKB administrator", 
                    file,col.names = FALSE,row.names = FALSE,quote = FALSE)
      }
    },
    contentType = "csv"
  )
}


field_log <- function(user,field_from_file,log_file){
  dat_field <- read.table(field_from_file,header = TRUE,nrows = 1,check.names = FALSE,sep = ",") %>% colnames()
  log_content <- data.frame(filename=field_from_file,
                            fields=paste0(dat_field[-1],collapse = ", "))
  write.table(log_content,file = log_file,
              sep = "\t",col.names = FALSE,row.names = FALSE,append = TRUE)
}


check_file_format <- function(file,must_include_col="eid",sep=","){
  file_info <- read.table(file,header = TRUE,sep = sep,stringsAsFactors = FALSE,check.names = FALSE,nrows = 1)
  colnames(file_info)%in%(must_include_col) %>% any()
}


remove_file <- function(file,time_to_delete){#deleting time in seconds
  remove_command <- paste0("nohup sh -c 'sleep ",time_to_delete,"m; rm ",file,"' > /dev/null 2>&1 &")
  system(remove_command)
}

# generate_file <- function(UKB_file,UKB_field,date_file){
#   write.table(UKB_field,file = paste0(date_file,"_selected_f.txt"),
#               sep = "\t",row.names = FALSE,col.names = FALSE,quote = FALSE)
#   p1 <- processx::process$new(command = "./utilities/ukbconv",
#                               args = c(UKB_file,"csv",paste0("-o",date_file),
#                                        paste0("-i",paste0(date_file,"_selected_f.txt"))),
#                               stdout = paste0(date_file,".log"),
#                               stderr = "2>&1"
#                               # stdout = "|",
#                               # stderr = "|"
#   )
#   p2 <- processx::process$new(command = "rm",
#                               args = paste0(date_file,"_selected_f.txt"))
# 
#   p1
#   
# }

generate_file <- function(UKB_file,UKB_field,date_file,ukbconv_wait=FALSE){
  # write.table(UKB_field,file = paste0(date_file,"_selected_f.txt"),
  #             sep = "\t",row.names = FALSE,col.names = FALSE,quote = FALSE)
  
  #waive using write.table for that the write.table function is finished, but the file is still being writing
  # system(paste0("echo ",UKB_field," > ", paste0(date_file,"_selected_f.txt","; echo $? > dev/null")),wait = TRUE)
  system(paste0("printf '",paste0(UKB_field,collapse = "\n"),"' > ", 
                paste0(date_file %>% str_remove_all("UKB_"),"_S_F.txt","; echo $? > /dev/null")),wait = TRUE)
  
  
  system(command = paste0(#"sleep 5",#to ensure that the last step file writing was finished, we wait for 5 seconds
                          "./utilities/ukbconv ",UKB_file," csv ", 
                          #to ensure that the last step file writing was finished, we wait for 5 seconds
                          paste0("-o",date_file)," ",
                          paste0("-i",paste0(date_file %>% str_remove_all("UKB_"),"_S_F.txt"))," > ", 
                          paste0(date_file,".log")," 2>&1"),
         wait = ukbconv_wait)
  system(command = paste0("sleep 5s; rm ",date_file %>% str_remove_all("UKB_"),"_S_F.txt"))
  
}





recode_file <- function(file_to_be_recoded,field_ID,coding_file){
  dat <- data.table::fread(file_to_be_recoded) %>% as_tibble()
  coding_file_dat <- dat %>% dplyr::select("eid",matches(paste0("_","f",field_ID,"_")))
  
  non_coding_file_dat <- dat %>% dplyr::select(-matches(paste0("_","f",field_ID,"_")))
  
  codings <- fread(coding_file) %>% as_tibble() %>% select(coding,meaning)
  
  dat_longer_tmp <- coding_file_dat %>% pivot_longer(cols = -1,names_to = "type",values_to = "disease")
  
  dat_recoded <- dat_longer_tmp %>% left_join(codings,by=c("disease"="coding")) %>% 
    pivot_wider(id_cols = -disease,names_from = type,values_from = meaning)
  
  dat_recoded %>% left_join(non_coding_file_dat,by="eid")
}



prepare_main <- function(data_file,key_file,file_md5,log_file){
  p <- processx::process$new(command = "bash",
                             args = c("utilities/check_and_unpack.sh",data_file,key_file,file_md5,"utilities"),
                             stdout = log_file,
                             stderr = "2>&1")
  p
}

sys_time <- function(){
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

finished_dataset <- function(dataset_dir){
  log_files <- list.files(path = dataset_dir,pattern = "prepare.log$",full.names = TRUE)
  finishing_info <- sapply(log_files,function(x){
    system(paste("grep 'Check and unpack finished'",x),ignore.stdout = TRUE,ignore.stderr = TRUE)
  })
  
  finished_data <- names(finishing_info[finishing_info==0])
  finished_data %>% str_replace_all(".*/(.*)_prepare.log","\\1") %>% str_c(".enc")
}


finished_data_extraction <- function(data_dir,user,log_finish_info="Output finished"){
  log_files <- list.files(path = data_dir,pattern = paste0(".*_",user,"_.*[0-9].log$"),full.names = TRUE)
  finishing_info <- sapply(log_files,function(x){
    system(paste0("grep '",log_finish_info,"' ",x),ignore.stdout = TRUE,ignore.stderr = TRUE)
  })
  finished_data <- names(finishing_info[finishing_info==0])
  finished_data %>% str_replace_all(".*/(.*).log","\\1") %>% str_c(".csv")
}


dat_summary <- function(UKB_dat,selected_var,subset_var,subset_thres,
                        statistic_count_or_proportion,legend_position,x_lab,remove_na = FALSE){
  proportion_or_count <- statistic_count_or_proportion
  count_lab <- switch(proportion_or_count,
                      proportion = "Proportion",
                      count = "Count (1000s)"
  )
  
  format_cnt <- switch(proportion_or_count,
                       # proportion = waiver(),
                       proportion = function(x) format(x,nsmall = 2),
                       count = function(x) format(round(x/1000))
  )
  
  bar_pos <- switch(
    proportion_or_count,
    proportion = "fill",
    count = "stack"
  )
  
  gg_dat <- if(UKB_dat[[subset_var]] %>% class() == "character"){
    UKB_dat %>% mutate(fill.var= !!sym(subset_var)%in%c(subset_thres))
  }else{
    UKB_dat %>% mutate(fill.var= !!sym(subset_var) >= subset_thres)
  }
  
  gg_dat <- if(remove_na){
    gg_dat %>% mutate(fill.var=ifelse(is.na(!!sym(subset_var)),NA,fill.var)) %>% filter(!is.na(fill.var))
  } else{
    gg_dat %>% mutate(fill.var=ifelse(is.na(!!sym(subset_var)),NA,fill.var))
  }
  
  
  gg_dat <- gg_dat %>% 
    mutate(category=ifelse(fill.var,"Subset","Reference"),color=ifelse(fill.var,"hotpink","grey35")) %>% 
    mutate(color=ifelse(is.na(fill.var),"grey65",color)) %>% 
    mutate(color=factor(color,levels = c("grey35","hotpink","grey65"))) %>% arrange(color)
  
  
  # if(is.numeric(UKB_dat %>% pull(!!selected_var))){
  #   gg_dat %>% 
  #     ggplot2::ggplot(aes(!!sym(selected_var), fill = color, color = alpha(color,alpha = 0))) + geom_density(na.rm = TRUE) + 
  #     
  #     scale_fill_identity(labels=gg_dat$category,breaks=gg_dat$color,guide = "legend")+
  #     scale_color_identity(labels=gg_dat$category,breaks=alpha(gg_dat$color,alpha = 0)) +
  #     
  #     theme(legend.position = legend_position, legend.title = element_blank(),
  #           axis.title.y = element_text(face = "bold"),  panel.grid = element_blank())+
  #     labs(x = x_lab)
  # }else{gg_dat %>%  
  #     ggplot2::ggplot(aes(!!sym(selected_var), fill = color)) + 
  #     # geom_bar(position = "fill", na.rm = TRUE, width = 0.5) + 
  #     geom_bar(position = bar_pos, na.rm = TRUE, width = 0.5) + 
  #     
  #     scale_fill_identity(labels=gg_dat$category,breaks=gg_dat$color,guide = "legend")+
  #     
  #     scale_y_continuous(labels = format_cnt) + 
  #     theme(legend.position = legend_position, legend.title = element_blank(), axis.title.y = element_text(face = "bold"), 
  #           panel.grid = element_blank()) + labs(x = x_lab, 
  #                                                y = count_lab) + coord_flip()
  # }
  
  if(is.numeric(UKB_dat %>% pull(!!selected_var))){
    gg_dat %>% 
      ggplot2::ggplot(aes(!!sym(selected_var), fill = alpha(color,alpha = 0.5), color = color)) + 
      geom_density(na.rm = TRUE) + 
      
      scale_fill_identity(labels=gg_dat$category,breaks=alpha(gg_dat$color,alpha = 0.5),
                          guide=guide_legend(override.aes = list(color=NA)))+
      scale_color_identity(labels=gg_dat$category,breaks=gg_dat$color) +
      
      theme(legend.position = legend_position, legend.title = element_blank(),
            axis.title.y = element_text(face = "bold"),  panel.grid = element_blank())+
      labs(x = x_lab) 
  }else{gg_dat %>%  
      ggplot2::ggplot(aes(!!sym(selected_var), fill = color)) + 
      # geom_bar(position = "fill", na.rm = TRUE, width = 0.5) + 
      geom_bar(position = bar_pos, na.rm = TRUE, width = 0.5) + 
      
      scale_fill_identity(labels=gg_dat$category,breaks=gg_dat$color,guide = "legend")+
      
      scale_y_continuous(labels = format_cnt) + 
      theme(legend.position = legend_position, legend.title = element_blank(), axis.title.y = element_text(face = "bold"), 
            panel.grid = element_blank()) + labs(x = x_lab, 
                                                 y = count_lab) + coord_flip()
  }
  
  
}



#The function ukb_icd_freq_by
#LUKB_ukb_icd_freq_by  
LUKB_ukb_icd_freq_by <- function (data, reference.var, n.groups = 10, 
                                  icd.code = c("I70","I"), 
                                  icd.labels = NULL, 
                                  plot.title = "", legend.col = 1, legend.pos = "right", icd.version = 10, 
                                  freq.plot = FALSE, reference.lab = "Reference variable", 
                                  freq.lab = "UKB disease frequency") {
  if(!is.null(icd.labels)){
    cat("Please make sure  your ICD labels matches your ICD codes!\n")
    if(length(icd.labels)!=length(icd.code)){
      stop("The numbers of ICD labels and of ICD codes are not matched!") 
    } else if (any(icd.labels%in%c("lower","upper"))) {
      stop("Please check your ICD labels!") 
    }
  } else {
    icd.labels <- icd.code
  }
  
  data <- data %>% dplyr::select(reference.var, matches(paste("^diagnoses.*icd",icd.version, sep = ""))) %>% 
    dplyr::filter(!is.na(.[[reference.var]]))
  
  if (is.character(data[[reference.var]])) {
    data[["categorized_var"]] <- data[[reference.var]]
  } else {
    data[["categorized_var"]] <- factor(ggplot2::cut_number(data[[reference.var]], 
                                                            n = n.groups), ordered = TRUE)
  }
  
  df <- data %>% dplyr::group_by(categorized_var) %>% tidyr::nest(.key = "dx")
  code_freq <- function(df, icd.code, icd.labels) {
    f <- purrr::map_dbl(icd.code, ~ukb_icd_prevalence(df,.x,icd.version = 10))
    f <- matrix(f, nrow = 1) %>% as.data.frame()
    names(f) = icd.labels
    return(f)
  }
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  dx_freq <- df %>% dplyr::mutate(freq = purrr::map(dx, code_freq, 
                                                    icd.code,icd.labels)) %>% tidyr::unnest(freq)
  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)
  
  if (is.numeric(data[[reference.var]])) {
    dx_freq[["tile_range"]] <- gsub("\\(|\\[|\\]", "", dx_freq$categorized_var)
    dx_freq <- dx_freq %>% tidyr::separate(tile_range, into = c("lower", "upper"), 
                                           sep = ",", convert = TRUE) %>% dplyr::arrange(lower)
  }
  
  if(freq.plot){
    if (is.numeric(data[[reference.var]])) {
      p <- dx_freq %>% dplyr::mutate(mid = (lower + upper)/2) %>%
        tidyr::gather(key = "disease", value = "frequency",
                      -categorized_var, -lower, -upper, -mid,-dx) %>%
        ggplot2::ggplot(aes(mid, frequency, group = disease,
                            color = disease)) + labs(x = reference.lab,
                                                     y = freq.lab, color = "", fill = "", title = plot.title) +
        theme(title = element_text(face = "bold"), panel.grid = element_blank(),
              panel.background = element_rect(color = NULL,
                                              fill = alpha("grey", 0.1)), legend.key = element_blank(),
              axis.ticks.x = element_blank()) + scale_y_continuous(labels = scales::percent_format(2)) +
        geom_point(size = 2) + geom_line(size = 0.5) +
        guides(color = guide_legend(ncol = legend.col),
               size = FALSE, fill = FALSE) #+ scale_fill_discrete(labels = icd.labels)
      
      # print(p)
    } else {
      p <- dx_freq %>% tidyr::gather(key = "disease", value = "frequency",
                                     -categorized_var,-dx) %>% ggplot2::ggplot(aes(categorized_var,
                                                                                   frequency, group = disease, fill = disease)) +
        labs(x = reference.lab, y = freq.lab, color = "",
             fill = "", title = plot.title) +
        theme(title = element_text(face = "bold"), panel.grid = element_blank(),
              panel.background = element_rect(color = NULL, fill = alpha("grey", 0.1)),
              legend.key = element_blank(), axis.ticks.x = element_blank()) +
        scale_y_continuous(labels = scales::percent_format(2)) +
        geom_bar(stat = "identity", position = "dodge") +
        guides(fill = guide_legend(ncol = legend.col),
               size = FALSE, color = FALSE) #+ scale_fill_discrete(labels = icd.labels)
      
      # print(p)
    }
  }
  
  list(figure=p,dat=dx_freq %>% select(-dx))
  
}

