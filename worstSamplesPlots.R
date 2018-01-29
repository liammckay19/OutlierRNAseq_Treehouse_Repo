
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

worstSamplesNames <- percentileOfEachSampleDf %>%
  select(p95, sampleID) %>%
  filter(p95 <= quantile(p95, c(0.15))) %>%
  mutate(names = gsub("T", "outlier_results_T",sampleID))
  
worstSamples<-lapply(worstSamplesNames$names, function(x) {
  read_tsv(x, col_types=cols()) %>%
    add_column(sampleID=gsub("outlier_results_", "", x))
}) 	%>%
  bind_rows()

ggplot(worstSamples, aes(sample, color = sampleID)) + 
  geom_histogram(binwidth = 0.1) +
  ggtitle("Bump In Worst Samples") +
  ylim(0, 2e+04)
# interesting graph of all the worst samples in a total histogram

splitDF <- split(worstSamples, worstSamples$sampleID)
plotTest<-data.frame(splitDF[1])
ggplot(plotTest, aes(sample)) + 
  geom_histogram(alpha = 0.5,  position = 'identity', aes(y=..density..)) + 
  ggtitle("95th Percentiles of All Samples and the worst sample") +
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))


for (x in splitDF) {
  plotdfSample <- data.frame(splitDF[x])
  p<-ggplot(plotdfSample, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
}

# need to make more efficient
{
  x <- 0
  ggplot(splitDF$TH01_0061_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0069_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0120_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0122_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0124_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0125_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0126_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0127_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0128_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0129_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0130_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0131_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0132_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0133_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH01_0134_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH02_0079_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH02_0080_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH02_0088_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH02_0096_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH03_0004_S03, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  x = x+1
  ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
  ggplot(splitDF$TH03_0019_S01, aes(sample)) + 
    geom_histogram() + 
    geom_density(alpha = 0, position = 'identity', aes(y=..density..))
  
  
  
}
x <- 0
ggplot(splitDF$TH01_0061_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0069_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0120_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0122_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0124_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0125_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0126_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0127_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0128_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0129_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0130_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0131_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0132_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0133_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH01_0134_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH02_0079_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH02_0080_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH02_0088_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH02_0096_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH03_0004_S03, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))
x = x+1
ggsave(x, plot = last_plot(), "png", "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/")
ggplot(splitDF$TH03_0019_S01, aes(sample)) + 
  geom_histogram() + 
  geom_density(alpha = 0, position = 'identity', aes(y=..density..))




