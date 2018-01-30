options(stringsAsFactors=FALSE) # for compatibile code between us

library(tidyverse)
library(gridExtra) # easy for putting graphs onto the same page (just use ggarrange(graph1, graph2, ncol = # of display
# columns, nrow = #row))


setwd("~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/comp4.3_tert8.ckcc.outlier_results")

up_outlier_files=list.files(, "outlier_results_")
outlierResults<-lapply(up_outlier_files, function(x) {
  read_tsv(x, col_types=cols()) %>%
    add_column(sampleID=gsub("outlier_results_", "", x))
}) 	%>%
  bind_rows()
percentileOfEachSampleDf <- outlierResults %>%
  group_by(sampleID) %>%
  summarise(p95 = quantile(sample, c(0.95)),p75 = quantile(sample, c(0.75))) %>%
  arrange(desc(p95))

## created my own function to concatenate two columns and compare them in ggplot with a 
# comparison needed 
# (basically generalizes what is written from the tutorial)
createBoundedData <- function(col1, col2, nameOfComparison, name1, name2) {
  ## very useful tutorial for plotting two histograms together
  # https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r/3557042
  # p95df <- data.frame(percentileOfEachSampleDf$p95)
  # p95df <- rename(p95df, 'value' = percentileOfEachSampleDf.p95)
  # 
  # p75df <- data.frame(percentileOfEachSampleDf$p75) 
  # p75df <- rename(p75df, 'value' = percentileOfEachSampleDf.p75)
  # 
  # p95df$percentile <- '95th'
  # p75df$percentile <- '75th'
  # 
  # bothPercentiles <- rbind(p95df, p75df)
  
  col1df <- data.frame(col1)
  col1df <- rename(col1df, 'value' = col1)
  
  
  col2df <- data.frame(col2)
  col2df <- rename(col2df, 'value' = col2)
  
  col1df$nameOfComparison <- name1
  col2df$nameOfComparison <- name2
  
  bothBoundedData <- rbind(col1df, col2df)
  colnames (bothBoundedData) <- c("values", nameOfComparison)
  return(bothBoundedData)
}
bothPercentilesOne <- createBoundedData(percentileOfEachSampleDf$p95, 
                                        percentileOfEachSampleDf$p75, 
                                        "percentile", '95th', '75th')
ggplot(bothPercentilesOne, aes(values, fill = percentile)) + 
  geom_histogram(alpha = 0.5, position = 'identity', aes(y=..density..)) + 
  ggtitle("95th and 75th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y=..density..)) +
  xlab("sample")