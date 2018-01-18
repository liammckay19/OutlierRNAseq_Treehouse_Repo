options(stringsAsFactors=FALSE) # for compatibile code between us

library(tidyverse)
library(ggpubr) # easy for putting graphs onto the same page (just use ggarrange(graph1, graph2, ncol = # of display
                # columns, nrow = #row))


setwd("~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/comp4.3_tert8.ckcc.outlier_results")

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

# each percentile is a percentile of the sample in the sample column

percentileOfEachSampleDf <- outlierResults %>%
  group_by(sampleID) %>%
  summarise(p95 = quantile(sample, c(0.95)))

arrange(percentileOfEachSampleDf, p95) 

# taking the histogram of a bad sample and comparing it to a good sample
# TH01_0069_S01
up_outlier_file_bad=list.files(, "outlier_results_TH01_0069_S01")

bad_sample<-lapply(up_outlier_file_bad, function(x) {
  read_tsv(x, col_types=cols()) %>%
    add_column(sampleID=gsub("outlier_results_TH01_0069_S01", "", x))
}) 	%>%
  bind_rows()

percentileOfEachSampleDf_bad_sample <- bad_sample %>%
  group_by(sample) %>%
  summarise(p95 = quantile(sample, c(0.95)))

p1 <- ggplot(percentileOfEachSampleDf_bad_sample, aes(p95)) + 
  geom_histogram(binwidth = 0.05) +
  xlim(c(2,6))
p2 <- ggplot(percentileOfEachSampleDf, aes(p95)) + 
  geom_histogram(binwidth = 0.05)

ggarrange(p1, p2, 
          labels = c("Bad", "All_Samples"))
