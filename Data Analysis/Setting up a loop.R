## Data science small group

# how to set up a loop

# loop for data cleaning

data <- VOI_data[main]
outliers <- list()
for (ii in colnames(data)) {
  col <- data[ ,ii]
  outliers[[ii]] <- boxplot.stats(col)$out
  
  #alternatively
  #boxplot.stats(VOI_data[ , ii])$out
  
  
}