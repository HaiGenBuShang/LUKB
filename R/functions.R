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

generate_file <- function(UKB_file,UKB_field,date_file){
  write.table(UKB_field,file = paste0(date_file,"_selected_f.txt"),
              sep = "\t",row.names = FALSE,col.names = FALSE,quote = FALSE)
  p1 <- processx::process$new(command = "./utilities/ukbconv",
                              args = c(UKB_file,"csv",paste0("-o",date_file),
                                       paste0("-i",paste0(date_file,"_selected_f.txt"))),
                              stdout = paste0(date_file,".log"),
                              stderr = "2>&1"
                              # stdout = "|",
                              # stderr = "|"
  )
  p2 <- processx::process$new(command = "rm",
                              args = paste0(date_file,"_selected_f.txt"))

  p1
  
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



