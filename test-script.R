library(tidyverse)

names(hw1_data)

head(hw1_data,2)
nrow(hw1_data)
tail(hw1_data, 2)
hw1_data$Ozone[47]
sum(is.na(hw1_data$Ozone))

mean(!is.na(hw1_data$Ozone))
mean(hw1_data$Ozone)

hw1_data %>%
  filter(!is.na(Ozone)) %>%
  summarise(mean(Ozone))

hw1_data %>%
  filter(!is.na(Ozone)) %>%
  filter(Ozone > 31 & Temp > 90) %>%
  summarise(mean(Solar.R))

hw1_data %>%
  filter(Month == 6) %>%
  summarise(mean(Temp))

hw1_data %>%
  filter(!is.na(Ozone), Month == 5) %>%
  summarise(max(Ozone))


cube <- function(x, n) {
  x^3
}
cube(3)

x <- 1:10
if(x > 5) {
  x <- 0
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}
y

source("complete.R")

threshold <- 150
directory <- "data/specdata"
i <- 1

corr <- function(directory, threshold = 0) {
  df <- complete("data/specdata", 1:332)
  csv_subset <- df %>% filter(nobs >= threshold)
  x <- nrow(csv_subset)
  dat <- data.frame()
  cor_vec <- numeric()
  for(i in 1:x) {
    fid <- csv_subset$id[[i]]
    filepath <- file.path(directory, paste(sprintf("%03d",fid), ".csv", sep = ""))
    dat <- read.csv(filepath)
    dat <- dat[complete.cases(dat), ]
    cor_vec <- c(cor_vec, cor(dat$sulfate, dat$nitrate))
  }
  cor_vec
}

cr <- corr("data/specdata", 150)
head(cr)
summary(cr)

dat %>% filter(ID == 2) %>% summarise(cor(sulfate, nitrate))

options(digits = 8)
source("pollutantmean.R")
source("complete.R")
source("corr.R")

pollutantmean("data/specdata", "sulfate", 1:10)
pollutantmean("data/specdata", "nitrate", 70:72)
pollutantmean("data/specdata", "sulfate", 34)
pollutantmean("data/specdata", "nitrate")

cc <- complete("data/specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("data/specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42, sample.kind = "Rounding")
cc <- complete("data/specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("data/specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("data/specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("data/specdata", 2000)                
n <- length(cr)                
cr <- corr("data/specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


library(datasets)
data(iris)
?iris
class(iris)
names(iris)

mean(iris$Sepal.Length[iris$Species=="virginica"])

tapply(iris$Sepal.Length, iris$Species, mean)
lapply(iris$Sepal.Length, mean(iris$Species))

apply(iris[, 1:4], 2, mean)


data(mtcars)
head(mtcars)

library(tidyverse)

tb <-
  mtcars %>%
  group_by(cyl) %>%
  summarise(avg_hp = mean(hp))

tb

sapply(split(mtcars$mpg, mtcars$cyl), mean)
tapply(mtcars$cyl, mtcars$mpg, mean)
split(mtcars, mtcars$cyl)
tapply(mtcars$mpg, mtcars$cyl, mean)
apply(mtcars, 2, mean)
mean(mtcars$mpg, mtcars$cyl)
lapply(mtcars, mean)
sapply(mtcars, cyl, mean)
with(mtcars, tapply(mpg, cyl, mean))

head(mtcars)


round(abs(mean(mtcars$hp[mtcars$cyl==4])-mean(mtcars$hp[mtcars$cyl==8])),0)

iris %>%
  group_by(Species) %>%
  summarise(avgSL = mean(Sepal.Length))


m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

m2 <- solve(m1)
m2
m1 * m2

set.seed(1)
rpois(5, 2)

set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
plot(x,y)
