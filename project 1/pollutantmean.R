pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, pattern="*.csv", full.names = TRUE)
  measures <- vector()
  for(i in id) {
    rawCSV <- read.csv(files_list[i])
    measures <- append(measures, rawCSV[,pollutant])
  }
  mean(measures[!is.na(measures)])  
 }