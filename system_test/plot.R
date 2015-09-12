#!/usr/local/bin/Rscript

library(ggplot2)

args <- commandArgs(trailingOnly = TRUE)
file <- args[1]
title <- args[2]

data <- read.csv(file)

ggplot(data, aes(x=false.probability, y=time)) + geom_point(alpha = 0.5) + stat_smooth() + ylab("time (s)") + xlab("Bloom false probability") + ggtitle(title)

outfile <- sprintf("%s.png", file)
ggsave(file=outfile)
