# ltmckay
# Liam McKay
# Treehouse Initiative


# (Liam)
# I did all this because I wanted to see various ways of visualizing the data 
# to find patterns that might jump out at me. Also it was a great exercise 
# for learning ggplot2

options(stringsAsFactors=FALSE) # for compatibile code between us

library(tidyverse)

setwd("~/Documents/UCSC/Junior/Treehouse/Prelim_Threshold_Analysis_Repo/comp4.3_tert8.ckcc.outlier_results")

####------- Code for using 1 data file only -------####
up_outlier_files_small = list.files(, "outlier_results_TH01_0053_S01") 
# so my computer can handle the data set faster for initial testing
outlierResultsSmall <- lapply(up_outlier_files_small, function(x) {
  read_tsv(x, col_types=cols()) %>% 
    add_column(sampleID=gsub("outlier_results_TH01_0053_S01", "", x))
}) 	%>% 
  bind_rows()

quantile(outlierResultsSmall$sample, c(0.95))
# 95% = 5.203984

df_small <- outlierResultsSmall %>%
  select(sample, pc_percentile) 

# test to visualize raw data 
qplot(sample, pc_percentile, data = df_small, xlab="log2(TPM+1)")

# reverse log2(TPM-1) to get just TPM:
log2Tpm <- df_small$sample + 1
Tpm <- 2^log2Tpm
Tpm

# attempting to make histogram of data without using hist() so I can plot it on a log10 scale

# using raw TPM
round(Tpm, digits = 1) # basically a bin width of 0.01
TpmDataFrame <- data.frame(Tpm)
TpmDataFrame
sort(Tpm, decreasing = TRUE)
TpmAndCount <- count(TpmDataFrame, vars = Tpm)
TpmAndCount
ggplot(TpmAndCount, aes(vars, log10(n))) + geom_point()


# using log2(TPM)
log2TpmRounded = round(log2Tpm, digits = 1) # bin width of 0.1
log2TpmRounded
log2TpmDf <- data.frame(log2TpmRounded) 

# first attempt at making a log10 histogram dot plot, but log10(count) was not possible (as far as I know)
ggplot(log2TpmDf, aes(log2TpmRounded)) + 
  geom_dotplot(inherit.aes = TRUE, binwidth = 0.01) +
  ylim(c(0,100))

hist(log2TpmDf$log2TpmRounded)


log2TpmAndCount <- count(log2TpmDf, vars = log2Tpm)

ggplot(log2TpmAndCount, aes(vars, log10(n))) + 
  geom_point() 

# using log10(tpm)
log10Tpm <- log10(Tpm)
log10TpmRounded <- round(log10Tpm, digits = 1)
log10TpmDf <- data.frame(log10TpmRounded)
log10DataFrameAndCount <- count(log10TpmDf, vars = log10Tpm)

ggplot(log10DataFrameAndCount, aes(vars, log10(n))) + 
  geom_point() +
  xlab("log10(TPM)")

