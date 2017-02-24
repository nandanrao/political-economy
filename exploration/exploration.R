library(ggplot2)
library(dplyr)
show <- function (c) ggsave("/tmp/chart.png", plot=c, width = 9.8, height = 7, dpi=72)



pacs16.raw <- read.csv("../data/pacs16.txt", sep=",", quote= "|", header=FALSE)


qplot(rnorm(100)) %>% show()
