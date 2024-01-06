#!/bin/bash

data_file=${1}
key_file=${2}
md5=${3}

dir=${4}/

${dir}/ukbmd5 ${data_file} > .md5sum_res.txt

file_md5=$(grep "MD5" .md5sum_res.txt | cut -d '=' -f 3)

if [ "${file_md5}" == "${md5}" ]; then
	#sleep 5m
	${dir}/ukbunpack ${data_file} ${key_file} 2>&1 | tee ${data_file}_unpack.log
	grep 'uncompression failed' ${data_file}_unpack.log > /dev/null
	if [ "$?" == 0 ]; then
		rm ${data_file}_ukb ${data_file}_unpack.log
		printf "\n************************************************************************\nThe key is not paired with the UKB data file, please check the key file.\n************************************************************************\n"
	else
		rm ${data_file}_unpack.log
		head -n 100 ${data_file}_ukb > UKB_data/preview/${data_file##*/}_ukb_preview
		${dir}/ukbconv ${data_file}_ukb docs
		zip -uj ./UKB_data/ukb_coding.zip ${data_file%.*}.html
		echo "Check and unpack finished!"
	fi
else
	#sleep 5m
	echo "The md5 check failed, please redownload the UKB data file or check the MD5."
	rm ${data_file}

fi

rm .md5sum_res.txt
