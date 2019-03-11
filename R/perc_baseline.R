#' @title Find the percent change from baseline
#'
#' @description `perc_baseline` calculates the percent change from a user-specified baseline period
#'
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Baseline.times range of time from Baseline.frame to compute the baseline value from
#' @param Baseline.frame a GCalcium-format data frame or matrix containing the baseline period. If frame is not specified, Dataframe is automatically used
#' @return a GCalcium-format data frame with values transformed to percent baseline
#' @examples
#' ### Format data frame
#' df.new <- format_data(GCaMP)
#'
#' ### Transform into percent baseline: relative to -3s to -1s before epoc
#' perc_baseline(Dataframe = df.new, Baseline.times = c(-3, -1))
#' @export

perc_baseline <- function(Dataframe, Baseline.times, Baseline.frame = FALSE) {

  ### Use Dataframe if Baseline.frame is not specified
  if(Baseline.frame == FALSE){
    Baseline.frame <- Dataframe
  }

  ### Make stuff for call
  baseline.start <- Baseline.times[1]
  baseline.stop <- Baseline.times[2]

  ### Get baseline mean and sd for each trial
  onset.df <- Baseline.frame[which(Baseline.frame[,1] >= baseline.start &
                                     Baseline.frame[,1] <= baseline.stop), 2:ncol(Baseline.frame)]

  baseline.means <- apply(onset.df, 2, mean)

  ###
  frame.list <- sapply(1:(length(baseline.means)), function(trialnum, Dataframe, baseline.means){

    trialind <- trialnum + 1

    base.mean <- baseline.means[[trialnum]]

    mean.difs <- ( (Dataframe[trialind] - base.mean) / base.mean ) * 100

  },
  Dataframe = Dataframe,
  baseline.means = baseline.means)

  df <- do.call(cbind, frame.list)
  df <- cbind(Time = Dataframe[1], df)

  return(df)

}
