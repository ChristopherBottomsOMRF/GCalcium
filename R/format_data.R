#' @title Reformat and label data into GCalcium format
#'
#' @description `format_data` changes the format of data from wide time series to long format with labeled columns. If data is already in long format, the function simply re-labels the columns
#'
#' @param data A data set with observation times in the first row or column, and observed values from trials in each following row or column.
#' @return Data frame with labeled time and trial columns
#' @examples
#' df.new <- format_data(GCaMP)
#' @export

format_data <- function(data) {

  ### Transpose data if not in long format
  if(ncol(data) > nrow(data)){
    data <- t(data)
    data <- as.data.frame(data)
    row.names(data) <- c()
  }

  ### Label time
  colnames(data)[1] <- "Time"

  ### label iters
  trial.nums <- 1:(ncol(data) - 1)

  ### Label trials
  colnames(data)[2:ncol(data)] <- sprintf("Trial%d", trial.nums)

  return(data)

}

