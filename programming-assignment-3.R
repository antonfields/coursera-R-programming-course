
# Change the working directory to the directory where the data is stored
dir <- "/data/rprog_data_ProgAssignment3-data"
wd <- getwd()
new_wd <- paste(wd, dir, sep = "")
setwd(new_wd)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

str(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


source("best.R")
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

source("rankhospital.R")
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

source("rankall.R")
head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$Hospital.Name)
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$Hospital.Name)
rankall("pneumonia", "worst")
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$Hospital.Name)
