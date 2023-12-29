#!/bin/bash

data_file=${1}
key_file=${2}
md5=${3}

dir=${4}/

${dir}/ukbmd5 ${data_file} > .md5sum_res.txt

file_md5=$(grep "MD5" .md5sum_res.txt | cut -d '=' -f 3)

if [ "${file_md5}" == "${md5}" ]; then
	#sleep 5m
	${dir}/ukbunpack ${data_file} ${key_file}
	head -n 100 ${data_file}_ukb > UKB_data/preview/${data_file##*/}_ukb_preview
	${dir}/ukbconv ${data_file}_ukb docs
	zip -uj ./UKB_data/ukb_coding.zip ${data_file%.*}.html
	echo "Check and unpack finished!"
else
	#sleep 5m
	echo "The md5 ckeck failed, please redownload the UKB data file or check the MD5."
	rm ${data_file}

fi

rm .md5sum_res.txt
