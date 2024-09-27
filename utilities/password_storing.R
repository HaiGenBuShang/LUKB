library(sodium)

args <- commandArgs(trailingOnly = TRUE)

passwd <- args[[1]]

passwd_store <- sodium::password_store(passwd)

cat(passwd_store)

