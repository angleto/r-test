complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filelist <- dir(directory)
  itemlist <- lapply(filelist, function(x){list( intvalue = as.integer(gsub("([0-9]+)\\.csv","\\1", x)), filename = x)})
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  selected_files_tmp <- lapply(itemlist,function(x){if(x[[1]] %in% id){x} else {NA}})
  selected_files <- selected_files_tmp[!is.na(selected_files_tmp)]
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  datalist <- sapply(selected_files,
                     function(item){
                       fullfn <- paste(directory, "/", item[[2]], sep="")
                       data_all <- read.csv(fullfn)
                       complete_cases <- sum(complete.cases(data_all))
                       c(item[[1]], complete_cases)
                     }                  
  )
  
  datalist <- t(datalist)
  colnames(datalist) <- c("id", "nobs")
  as.data.frame(datalist)
}