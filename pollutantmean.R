

pollutantmean <- function(directory, pollutant, id = 1:332) {
  dat <- data.frame()
  for(i in id) {
    filepath <- file.path(directory, paste(sprintf("%03d",i), ".csv", sep = ""))
    dat <- rbind(dat, read.csv(filepath))
  }
  mean(dat[[pollutant]], na.rm = TRUE)
}


