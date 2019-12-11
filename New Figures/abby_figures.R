library(phyloseq)
library(ggplot2)
library(dplyr)

#run the "read_phyloseq" script first to create a phyloseq object called "apples"

#normalize sample counts to the median number of reads
total <- median(sample_sums(apples))
standf <- function(x, t=total) round(t*(x/sum(x)))
apples_normal <- transform_sample_counts(apples, standf)

plot_bar(apples_normal, fill = "Phylum")

#convert phyloseq object to dataframe
applesdf <- psmelt(apples)
normapplesdf <- psmelt(apples_normal)

#normalized abundance plot
ggplot() + geom_bar(aes(y = Abundance, x = tissue, fill = Phylum), data = normapplesdf, stat = "identity") + facet_wrap("mngmt") + theme_bw() + theme(axis.text.x=element_text(angle=90, hjust=1))


applesdf <- psmelt(apples)
total_abundance <- sum(applesdf$Abundance)
applesreduced <- filter(applesdf, Abundance > total_abundance*.001)

#qPCR plot?
ggplot() + geom_bar(aes(y = Abundance, x = tissue), data = applesdf, stat = "identity") + facet_wrap("mngmt") + theme_bw() + theme(axis.text.x=element_text(angle=90, hjust=1))
