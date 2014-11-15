
#simple function
add <- function(x,y) {
  x + y
}

#function with default argument
getAbove <- function(v, n = 10) {
  aboveValues <- v > n
  x[aboveValues]
}

# calculate the mean of a column
columnMean <- function(m, removeNA=TRUE) {
  nc <- ncol(m) # calculate the number of col
  means<-numeric(nc) # initializate an array  with 0
  for(i in 1:nc) {
    means[i] <- mean(m[,i], na.rm = removeNA) # calculate the means for each column
  }
  means
}
