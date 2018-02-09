

options(stringsAsFactors = FALSE) # for compatibile code between us

library(tidyverse)
library(gridExtra) # easy for putting graphs onto the same page (just use ggarrange(graph1, graph2, ncol = # of display
# columns, nrow = #row))
# Create plots of 95th vs 50,60,75,80,85-93 percentile

setwd(
  "~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/comp4.3_tert8.ckcc.outlier_results"
)

up_outlier_files = list.files(, "outlier_results_")

outlierResults <- lapply(up_outlier_files, function(x) {
  read_tsv(x, col_types = cols()) %>%
    add_column(sampleID = gsub("outlier_results_", "", x))
}) 	%>%
  bind_rows()



percentileOfEachSampleDf <- outlierResults %>%
  group_by(sampleID) %>%
  summarise(
    p95 = quantile(sample, c(0.95)),
    p75 = quantile(sample, c(0.75))
    ,
    p80 = quantile(sample, c(0.80)),
    p60 = quantile(sample, c(0.60))
    ,
    p50 = quantile(sample, c(0.50)),
    p85 = quantile(sample, c(0.85))
    ,
    p86 = quantile(sample, c(0.86)),
    p90 = quantile(sample, c(0.90))
    ,
    p87 = quantile(sample, c(0.87)),
    p91 = quantile(sample, c(0.91))
    ,
    p88 = quantile(sample, c(0.88)),
    p92 = quantile(sample, c(0.92))
    ,
    p89 = quantile(sample, c(0.89)),
    p93 = quantile(sample, c(0.93))
  ) %>%
  arrange(desc(p95))
{
  p95p50 <-
    gather(
      percentileOfEachSampleDf,percentile,sample,p95,p50
    )
  p95p60 <-
    gather(
      percentileOfEachSampleDf,percentile,sample,p95,p60
    )
  p95p75 <-
    gather(
      percentileOfEachSampleDf,percentile,sample,p95,p75
    )
  p95p80 <-
    gather(
      percentileOfEachSampleDf,percentile,sample,p95,p80
    )
  p95p85 <-
    gather(
      percentileOfEachSampleDf,percentile,sample,p95,p85
    )
  p95p85p88p92 <-
    gather(
      percentileOfEachSampleDf,percentile,sample,p95,p85,p88,p92
    )
  }



ggplot(p95p50, aes(sample, fill = percentile)) +
  geom_histogram(alpha = 0.5, position = 'identity', aes(y = ..density.., color = percentile), binwidth = 0.1) +
  ggtitle("95th and 50th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..)) +
  xlab("log2(TPM+1) cutoff")

ggplot(p95p60, aes(sample, fill = percentile)) +
  geom_histogram(alpha = 0.5, position = 'identity', aes(y = ..density.., color = percentile), binwidth = 0.1) +
  ggtitle("95th and 60th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..)) +
  xlab("log2(TPM+1) cutoff")

ggplot(p95p75, aes(sample, fill = percentile)) +
  geom_histogram(alpha = 0.5, position = 'identity', aes(y = ..density.., color = percentile), binwidth = 0.1) +
  ggtitle("95th and 75th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..)) +
  xlab("log2(TPM+1) cutoff")

ggplot(p95p85, aes(sample, fill = percentile)) +
  geom_histogram(alpha = 0.5, position = 'identity', aes(y = ..density.., color = percentile), binwidth = 0.1) +
  ggtitle("95th and 85th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..)) +
  xlab("log2(TPM+1) cutoff")

ggplot(percentileOfEachSampleDf, aes(p95)) +
  geom_histogram(alpha = 0.5, position = 'identity', aes(y = ..density..)) +
  ggtitle("95th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..)) +
  xlab("log2(TPM+1) cutoff")


# colnames(percentileOfEachSampleDf)[1]<-'sampleID'


# Multiple plot of 50-85
paFifty <-
  ggplot(p95p50, aes(values, fill = percentile)) +
  geom_histogram(
    binwidth = 0.1,
    alpha = 0.5,
    position = 'identity',
    aes(y = ..density..)
  ) +
  ggtitle("95th and 50th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..))

pbSixty <-
  ggplot(p95p60, aes(values, fill = percentile)) +
  geom_histogram(
    binwidth = 0.1,
    alpha = 0.5,
    position = 'identity',
    aes(y = ..density..)
  ) +
  ggtitle("95th and 60th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..))

pcSeventyFive <-
  ggplot(p95p75, aes(values, fill = percentile)) +
  geom_histogram(
    binwidth = 0.1,
    alpha = 0.5,
    position = 'identity',
    aes(y = ..density..)
  ) +
  ggtitle("95th and 75th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..)) +
  xlab("sample")

pdEighty <-
  ggplot(p95p80, aes(values, fill = percentile)) +
  geom_histogram(
    binwidth = 0.1,
    alpha = 0.5,
    position = 'identity',
    aes(y = ..density..)
  ) +
  ggtitle("95th and 80th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..))

peEightyFive <-
  ggplot(p95p85, aes(values, fill = percentile)) +
  geom_histogram(
    binwidth = 0.1,
    alpha = 0.5,
    position = 'identity',
    aes(y = ..density..)
  ) +
  ggtitle("95th and 85th Percentiles of All Samples") +
  geom_density(alpha = 0, position = 'identity', aes(y = ..density..))

# Create large column of all plots
grob <-
  arrangeGrob(
    grobs = list(paFifty, pbSixty, pcSeventyFive, pdEighty, peEightyFive),
    nrow = 5,
    ncol = 1
  )
grid.arrange(grob)
