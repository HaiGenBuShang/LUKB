#!/bin/bash

file_prefix=${1}

for i in $(ls Results/${file_prefix}*HR_disease_part*RData)
do

	nohup Rscript utilities/execute_HR_analysis.R ${file_prefix} ${i} > ${i%.RData}.log 2>&1 &
done

