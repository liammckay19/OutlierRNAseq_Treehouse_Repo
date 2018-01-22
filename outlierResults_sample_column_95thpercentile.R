# OutlierResults_sample_column_95thpercentile.r

# - finds 95th percentile overall of comp4.3_tert8.ckcc.outlier_results
# - compares the lowest 95th percentile sample individually
#   to overall, middle, and best 95th percentile score


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
  summarise(p95 = quantile(sample, c(0.95))) %>%
  arrange(desc(p95))


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

p_bad <- ggplot(percentileOfEachSampleDf_bad_sample, aes(p95)) + 
  geom_histogram(binwidth = 0.05) +
  xlim(c(2,6))
p_overall <- ggplot(percentileOfEachSampleDf, aes(p95)) + 
  geom_histogram(binwidth = 0.05)

ggarrange(p_bad, p_overall, 
          labels = c("Bad", "All_Samples"))


#---comp 1--  comparing a good sample and a bad sample

summary(percentileOfEachSampleDf)
# TH01_0062_S01 is closest to the mean (5.159468 ~ Mean   :5.154)

sample_file_good=list.files(, "outlier_results_TH01_0062_S01")

good_sample<-lapply(sample_file_good, function(x) {
  read_tsv(x, col_types=cols()) %>%
    add_column(sampleID=gsub("outlier_results_TH01_0062_S01", "", x))
}) 	%>%
  bind_rows()

percentileOfEachSampleDf_good_sample <- good_sample %>%
  group_by(sample) %>%
  summarise(p95 = quantile(sample, c(0.95)))

p_good <- ggplot(percentileOfEachSampleDf_good_sample, aes(p95)) + 
  geom_histogram(binwidth = 0.1) 
p_bad <- ggplot(percentileOfEachSampleDf_bad_sample, aes(p95)) + 
  geom_histogram(binwidth = 0.1) 

ggarrange(p_bad, p_good, 
          labels = c("Bad", "Good"))


#---comp 2--  comparing a Best sample and a bad sample

summary(percentileOfEachSampleDf)
# TH03_0008_S01 is highest 95th percentile (5.971084)

sample_file_best=list.files(, "outlier_results_TH03_0008_S01")

best_sample<-lapply(sample_file_best, function(x) {
  read_tsv(x, col_types=cols()) %>%
    add_column(sampleID=gsub("outlier_results_TH03_0008_S01", "", x))
}) 	%>%
  bind_rows()

percentileOfEachSampleDf_best_sample <- best_sample %>%
  group_by(sample) %>%
  summarise(p95 = quantile(sample, c(0.95)))

p_best <- ggplot(percentileOfEachSampleDf_best_sample, aes(p95)) + 
  geom_histogram(binwidth = 0.1) +
  xlim(c(0,18))
p_bad <- ggplot(percentileOfEachSampleDf_bad_sample, aes(p95)) + 
  geom_histogram(binwidth = 0.1) +
  xlim(c(0,18))

ggarrange(p_bad, p_best, 
          labels = c("Bad", "Best"))


