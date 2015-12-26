#!/usr/local/bin/Rscript

library(ggplot2)
library(grid)
library(gridExtra)

args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
title <- args[2]

data <- read.csv(file)

prep <- ggplot(data, aes(factor(""), prep.time)) + geom_boxplot() + coord_flip() + scale_x_discrete("") + ggtitle("Prep time") + ylab("Prep time (s)") + xlab("")

sync <- ggplot(data, aes(x=false.probability, y=time)) + geom_point(alpha = 0.5) + stat_smooth() + ylab("time (s)") + xlab("Bloom false probability") + ggtitle("Sync time")

bsize <- ggplot(data, aes(x=false.probability, y=bloom.size/(1024*1024))) + geom_point(alpha = 0.5) + stat_smooth() + ylab("Size (MB)") + xlab("Bloom false probability") + ggtitle("Bloom Size")

dsize <- ggplot(data, aes(x=false.probability, y=data.size/(1024*1024))) + geom_point(alpha = 0.5) + stat_smooth() + ylab("Size (MB)") + xlab("Bloom false probability") + ggtitle("Data Size")

outfile <- sprintf("%s.png", file)

png(filename = outfile, width = 1200, height = 600)
grid.arrange(prep, sync, bsize, dsize, ncol=2, nrow=2)
dev.off()
