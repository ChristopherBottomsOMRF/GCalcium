#' @title Compare mean activity between trials
#'
#' @description `between_trial_change` finds the difference in means during same time range between sets of trials.
#'
#' @param Dataframe a GCaMP-format data frame or matrix
#' @param TrialRange1 range of trial numbers to be compared to second set of trials
#' @param TrialRange2 range of trial numbers to be compared to first set of trials
#' @param Time.period range of time to be compared between sets of trials
#' @return Number representing mean difference of trial set 2 and trial set 1
#' @examples
#' # How much did the mean value change 2s after epoc between trials 1-5 and trials 6-10?
#' df.new <- format_data(data = GCaMP)
#' between_trial_change(Dataframe = df.new, TrialRange1 = c(1, 5),
#' TrialRange2 = c(6, 10), Time.period = c(0, 2))
#' @export

between_trial_change <- function(Dataframe, TrialRange1, TrialRange2, Time.period = c(min(Dataframe[1]), max(Dataframe[1]))){

  trial1ind <- TrialRange1 + 1
  trial2ind <- TrialRange2 + 1

  start.time <- Time.period[1]
  stop.time <- Time.period[2]

  ### Subsets based on time and trials
  sub1 <- Dataframe[Dataframe[1] >= start.time & Dataframe[1] <= stop.time, ]

  t1.sub <- as.matrix(sub1[,trial1ind])
  t2.sub <- as.matrix(sub1[,trial2ind])

  ### Trial numbers
  t1.mean <- mean(t1.sub)
  t2.mean <- mean(t2.sub)

  mean.change <- t2.mean - t1.mean

  return(mean.change)

}
