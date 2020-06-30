

complete <- function(directory, id) {
  csvfiles <- file.path(directory, sprintf("%03d.csv", id))
  data.frame(id = id,
             nobs = sapply(csvfiles, function(f) sum(complete.cases(read.csv(f)))),
             row.names = NULL
             )
}


