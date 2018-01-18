options(stringsAsFactors=FALSE) # for compatibile code between us

library(tidyverse)

setwd("~/Documents/UCSC/Junior/Treehouse/Prelim_Threshold_Analysis_Repo/comp4.3_tert8.ckcc.outlier_results")

up_outlier_files=list.files(, "outlier_results_")

outlierResults<-lapply(up_outlier_files, function(x) {
	read_tsv(x, col_types=cols()) %>%
	add_column(sampleID=gsub("outlier_results_", "", x))
	}) 	%>%
	bind_rows()

quantile(outlierResults$sample, c(0.95)) 
# 95% = 5.242984
# the genes that are 5.242984 log2(TPM+1) and above are useful currently
# Should 0.0 TPM be considered in the data?
# What is a cutoff point that is reliably accurate that the values are in the child's DNA for that gene


