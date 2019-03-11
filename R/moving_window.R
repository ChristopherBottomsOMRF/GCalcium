#' @title Summarize data within windows of time
#'
#' @description `moving_window` summarizes data within windows of a certain length.
#'
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trial a single trial number
#' @param Window.length length of time each window encompasses
#' @param FUN a function to apply to each window
#' @return a data frame with start and stop times of each window, the chronological number of each window, and summarized values
#' @examples
#' ### Format data frame
#' df.new <- format_data(GCaMP)
#'
#' ### In trial 5, how does the average fluorescence change in 1 second time frames?
#' moving_window(Dataframe = df.new, Trial = 5, Window.length = 1, FUN = mean)
#' @export

moving_window <- function(Dataframe, Trial, Window.length, FUN = mean){

  window.num <- 1

  # blank vectors
  vals <- c()
  start.times <- c()
  stop.times <- c()
  window.nums <- c()

  # make first
  window.start <- min(Dataframe[,1])
  trialind <- Trial + 1

  while(window.start < max(Dataframe[,1])){

    window.stop <- window.start + Window.length
    window.matrix <- as.matrix(
      Dataframe[which(Dataframe[,1] >= window.start &
                        Dataframe[,1] < window.stop), trialind]
    )

    # new vals to add
    summary.vals <- FUN(window.matrix)

    # add to frames before repeating
    vals <- append(vals, summary.vals)
    start.times <- append(start.times, window.start)
    stop.times <- append(stop.times, window.stop)
    window.nums <- append(window.nums, window.num)

    window.start <- window.start + Window.length
    window.num <- window.num + 1

  }

  df <- as.data.frame(cbind(Summary.vals = vals,
              Start.time = start.times,
              Stop.time = stop.times,
              Window.num = window.nums)
  )

  return(df)

}
