#!/bin/bash

file_prefix=${1}

#ls ${prefix}*

for i in $(ls Results/${file_prefix}_ccwc_part*RData | grep ccwc_part[0-9]*.RData)
do
	nohup Rscript utilities/execute_ccwc.R ${file_prefix} ${i} > ${i%.RData}.log 2>&1 &
	#echo ${file_prefix}
	#echo ${i}
	#echo ""
done
