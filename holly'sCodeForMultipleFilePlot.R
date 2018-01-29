# holly's code

nfp<-outlierResults %>% group_by(sampleID) %>% summarize(nfp=quantile(sample, 0.95))

fifteenth=quantile(nfp$nfp, 0.15)
worst15pctSamples<-nfp %>% filter(nfp<fifteenth)


for (thisSample in worst15pctSamples$sampleID){
  thisSample=worst15pctSamples$sampleID
  p<-ggplot(outlierResults %>% filter(sampleID==thisSample)) + geom_histogram(aes(x=sample)) + ggtitle(thisSample)
  ggsave(p, paste0(thisSample, ".png"))
}