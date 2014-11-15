corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  f_list <- complete(directory)
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  f_list_w_thresh <- lapply(1:nrow(f_list), function(i) {
    fn <- list.files(directory,paste("^0*",f_list[[1]][[i]],".csv$",sep=""), full.names=TRUE)
    file_elems_n <- 
      if(f_list[[2]][[i]] >= threshold) {
        fn
      } else {
        NA
      }
  })

  filt_f_list <- f_list_w_thresh[!is.na(f_list_w_thresh)]

  ## Return a numeric vector of correlations
  datalist <- lapply(filt_f_list,
                     function(item){
                       data_all <- read.csv(item)
                       data_pollutant_raw <- data_all[c("sulfate", "nitrate")]
                       data_pollutant_filt <- data_pollutant_raw[complete.cases(data_pollutant_raw),]
                       sulfate <- data_pollutant_filt[[1]]
                       nitrate <- data_pollutant_filt[[2]]
                       c<-cor(x=sulfate,y=nitrate)
                     })
  unlist(datalist[!is.na(datalist)])
}