complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
  final <- data.frame()
  for (i in id){
    rawCSV <- read.csv(files_list[i])
    x <- rawCSV[,"sulfate"]
    y <- rawCSV[,"nitrate"]
    obs <- sum(complete.cases(x,y))
    final <- rbind(final, c(i, obs))
  }
  colnames(final) <- c("id","nobs")
  final
}