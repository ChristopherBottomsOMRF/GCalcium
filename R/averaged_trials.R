#' @title Averages signals across trials
#'
#' @description `averaged_trials` averages values over each time point, across the specified trials
#'
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trials numbers of trials to be averaged across
#' @return a data frame with observation times and averaged values
#' @examples
#' ### Format data frame
#' df.new <- format_data(GCaMP)
#'
#' ### Plot the average fluorescence signal across trials 1-5
#' df.1thru5 <- averaged_trials(df.new, 1:5)
#'
#' plot(x = df.1thru5$Time, df.1thru5$Values)
#' @export

averaged_trials <- function(Dataframe, Trials){

  trialinds <- Trials + 1

  TrialMeans <- rowMeans(Dataframe[trialinds])
  return.df <- as.data.frame(cbind(Dataframe[,1], TrialMeans))

  colnames(return.df) <- c("Time", "Values")

  return(return.df)

}
