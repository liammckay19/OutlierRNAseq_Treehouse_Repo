# Plots.r
# ------------------------------------------------------------------------------
# Plots
# ------------------------------------------------------------------------------





# make file batch of plots
nfp <-
	outlierResults %>% group_by(sampleID) %>% summarize(nfp = quantile(sample, 0.95))

fifteenth = quantile(nfp$nfp, 0.15)
worst15pctSamples <- nfp %>% filter(nfp < fifteenth)

thisSample <- NULL
for (thisSample in worst15pctSamples$sampleID) {
	print(thisSample)
	df <- outlierResults %>% filter(sampleID == thisSample)
	
	p <- ggplot(df) +
		geom_histogram(aes(sample), binwidth = 0.1) +
		ggtitle(thisSample)
	
	ggsave(
		paste0(thisSample, ".png"),
		plot = p,
		"png",
		"~/Documents/UCSC/Junior/Treehouse/OutlierRNAseq_Treehouse_Repo/WorstPercentilePlots/"
	)
	
}
