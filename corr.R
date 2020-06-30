source("complete.R")

corr <- function(directory, threshold = 0) {
  df <- complete("data/specdata", 1:332)
  csv_subset <- subset(df, df$nobs > threshold)
  x <- nrow(csv_subset)
  dat <- data.frame()
  cor_vec <- numeric()
  if(x >0) {
    for(i in 1:x) {
    fid <- csv_subset$id[[i]]
    filepath <- file.path(directory, paste(sprintf("%03d",fid), ".csv", sep = ""))
    dat <- read.csv(filepath)
    dat <- dat[complete.cases(dat), ]
    cor_vec <- c(cor_vec, cor(dat$sulfate, dat$nitrate))
    }
  }
 cor_vec
}

