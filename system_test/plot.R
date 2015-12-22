#!/usr/local/bin/Rscript

library(ggplot2)
library(grid)
library(gridExtra)

args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
title <- args[2]

data <- read.csv(file)

time <- ggplot(data, aes(x=false.probability, y=time)) + geom_point(alpha = 0.5) + stat_smooth() + ylab("time (s)") + xlab("Bloom false probability") + ggtitle("Sync time")

prep <- ggplot(data, aes(x=false.probability, y=prep.time)) + geom_point(alpha = 0.5) + stat_smooth() + ylab("time (s)") + xlab("Bloom false probability") + ggtitle("Prep time")

bsize <- ggplot(data, aes(x=false.probability, y=bloom.size/(1024*1024))) + geom_point(alpha = 0.5) + stat_smooth() + ylab("Size (MB)") + xlab("Bloom false probability") + ggtitle("Bloom Size")

dsize <- ggplot(data, aes(x=false.probability, y=data.size/(1024*1024))) + geom_point(alpha = 0.5) + stat_smooth() + ylab("Size (MB)") + xlab("Bloom false probability") + ggtitle("Data Size")

outfile <- sprintf("%s.png", file)

png(filename = outfile, width = 1200, height = 600)
grid.arrange(time, prep, bsize, dsize, ncol=2, nrow=2)
dev.off()
