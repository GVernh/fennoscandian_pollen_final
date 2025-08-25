### make bins of 500 years ###

library(tidyverse)

# load bigdf_genera

# set up cut-off values 
breaks <- c(-60, 0, 100, 200, 300, 400, 600, 800, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000, 5500, 6000,
            6500, 7000, 7500, 8000, 8500, 9000, 9500, 10000)

# specify interval/bin labels
time.intervals <- c("[-60-0)","[0-100)", "[100-200)", "[200-300)", "[300-400)", "[400-600)","[600-800)", "[800-1000)",
          "[1000-1500)", "[1500-2000)", "[2000-2500)", "[2500-3000)", "[3000-3500)", "[3500-4000)", "[4000-4500)",
          "[4500-5000)", "[5000-5500)", "[5500-6000)", "[6000-6500)", "[6500-7000)", "[7000-7500)", "[7500-8000)",
          "[8000-8500)", "[8500-9000)", "[9000-9500)", "[9500-10000)")

# bucketing values into bins
group_tags <- cut(bigdf_genera$meantimes, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=time.intervals)

bigdf_genera_intervals <- bigdf_genera
bigdf_genera_intervals$time.intervals <- group_tags

# Sort
bigdf_genera_intervals <- bigdf_genera_intervals %>% select(dataset_ID, meantimes, time.intervals, everything())
bigdf_genera_intervals <- subset(bigdf_genera_intervals, select = -c(dataset_ID))

# group by
by_bigdf_genera_intervals <- bigdf_genera_intervals %>% group_by(time.intervals)
by_bigdf_genera_intervals <- by_bigdf_genera_intervals %>%
  group_by(time.intervals) %>%
  summarise(
    meantimes = mean(meantimes),
    across(3:255,na.rm=TRUE,sum))

# Save
genera_intervals.df <- by_bigdf_genera_intervals
#save(genera_intervals.df,file="genera_intervals.Rda")
#load("genera_intervals.Rda")
