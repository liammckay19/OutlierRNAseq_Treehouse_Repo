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

ggplot(log10(rawTPMDf_nozeros), aes(sample)) + 
  geom_point() +
  ylim(c(0,10000))


