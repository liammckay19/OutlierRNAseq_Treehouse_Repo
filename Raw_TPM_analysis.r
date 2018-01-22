# Raw_TPM_analysis.r

options(stringsAsFactors=FALSE) # for compatibile code between us

library(tidyverse)
library(ggpubr) # easy for putting graphs onto the same page (just use ggarrange(graph1, graph2, ncol = # of display
# columns, nrow = #row))


setwd("~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/ckcc_rsem_genes_results")



sample_file_list=list.files(, "TH0")

rawTPMDf<-lapply(sample_file_list, function(x) {
      read_tsv(x, col_types=cols()) %>%
      add_column(sampleID=gsub("outlier_results_", "", x))
      }) 	%>%
      bind_rows()

TPMDf <- rawTPMDf %>%
  select(TPM)

no_zero_TPMDf <- TPMDf %>%
  filter(TPM > 0)
summary(TPMDf)
  

Log2TPMDf <- log2(TPMDf-1)
countLog2TPMDf <- count(Log2TPMDf, vars = TPM)

ggplot(countLog2TPMDf, aes(vars, log2(n))) + geom_point() # weird binning was observed
# it does spread in a different way



ggplot(log2(TPMDf-1), aes(TPM)) + # with zeros
  geom_histogram() # plot of bell curve centered at 3 
ggplot(log2(no_zero_TPMDf-1), aes(TPM)) + # no zeros 
  geom_histogram() + 
  geom_vline(xintercept = 0) # plot of bell curve centered at 3 


percentileOfEachTPMDf <- rawTPMDf %>%
  group_by(sampleID) %>%
  summarise(p95 = quantile(TPM, c(0.95)), p75 = quantile(TPM, c(0.75))) %>%
  arrange(desc(p95))

# binwidth of 1 for 95th percentile of TPM across all genes
ggplot(percentileOfEachTPMDf, aes(p95)) + geom_histogram(binwidth = 1)

# binwidth of 0.1 for 75th percentile of TPM across all genes
ggplot(percentileOfEachTPMDf, aes(p75)) + geom_histogram(binwidth = 0.1)



sum(TPMDf)
# = 1.46e+08 total TPM

countOfTPMDf <- count(TPMDf, vars = TPM)

count(TPMDf, TPM > 0)
# `TPM > 0`       n
# <lgl>       <int>
#   1 F         4690960
#   2 T         4141748

head(countOfTPMDf)
# confirms above works  (the vars = 0; n = 4690960)

