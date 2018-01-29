# for next week, for each sample, find the number of genes with zero counts and compare 
# the numbers to the 95th pctl (e.g. scatterplot, #genes with zero counts vs 95th percentile, 
# one point per sample)


options(stringsAsFactors=FALSE) # for compatibile code between us

library(tidyverse)
library(gridExtra) # easy for putting graphs onto the same page (just use ggarrange(graph1, graph2, ncol = # of display
# columns, nrow = #row))


setwd("~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/comp4.3_tert8.ckcc.outlier_results")


# find the number of genes with zero counts
# number of samples that are 0 and not zero
count(outlierResults_sample, sample == 0)
#   `sample == 0`       n
#   <lgl>           <int>
#   1 F             4124131
#   2 T             4428695

# get dataframe overall to compute for each sample file the 


eachSampleZeroCount <- outlierResults %>%
  group_by(sampleID) %>%
  count(sample) %>%
  filter(sample == 0)



# 4428695 is the number of samples with 0 count
