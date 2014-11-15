pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filelist <- dir(directory)
  itemlist <- lapply(filelist, function(x){list( intvalue = as.integer(gsub("([0-9]+)\\.csv","\\1", x)), filename = x)})

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  pollutant_cols <- c("sulfate", "nitrate")
  if(sum(pollutant == pollutant_cols) < 1) {
    return(NA)
  }
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  selected_files_tmp <- lapply(itemlist,function(x){if(x[[1]] %in% id){x[[2]]} else {NA}})
  selected_files <- selected_files_tmp[!is.na(selected_files_tmp)]
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  datalist <- lapply(selected_files,
                     function(item){
                       fullfn <- paste(directory, "/", item, sep="")
                       data_all <- read.csv(fullfn)
                       data_pollutant <- data_all[pollutant]
                       data <- data_pollutant[!is.na(data_pollutant)]
                     })

  mean(unlist(datalist))
}