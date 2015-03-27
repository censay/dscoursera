corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
  correlate <- vector("numeric", length=0)
  comp <- complete(directory)
  for (i in 1:length(files_list)) {
    if (comp[i,"nobs"] > threshold){
      readings <- read.csv(files_list[i])
      x <- readings[,"sulfate"]
      y <- readings[,"nitrate"]
      a <- x[complete.cases(x,y)]
      b <- y[complete.cases(x,y)]
      correlate <- append(correlate, cor(a,b))
    }
  }
  correlate
}