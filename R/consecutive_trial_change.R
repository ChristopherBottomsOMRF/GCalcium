#' @title Compare mean activity in consecutive trials
#'
#' @description `consecutive_trial_change` finds the change in means between consecutive trials.
#'
#' @param Dataframe a GCaMP-format data frame or matrix
#' @param Trials range of trial numbers to be compared
#' @param Time.period range of time to be compared
#' @return Data frame with the "Mean.Change" column representing differences in means between trial n and trial n + 1 for the user-inputted range of trials.
#' @examples
#' ### How much did the mean value change after epoc between consecutive trials in trials 1-10?
#' df.new <- format_data(data = GCaMP)
#' consecutive_trial_change(Dataframe = df.new, Trials = c(1, 10), Time.period = c(0, 4))
#' @export

consecutive_trial_change <- function(Dataframe, Trials, Time.period){

  blank.frame <- data.frame()

  ### Create indices
  ind.start1 <- Trials[1] + 1
  ind.start2 <- ind.start1 + 1
  ind.stop <- Trials[2] + 1

  ### Repeat across consecutive trials
  while(ind.start1 < ind.stop){

    trialnum <- (ind.start1 - 1) + .5

    # Subsets
    t1.sub <- as.matrix(Dataframe[,ind.start1])
    t2.sub <- as.matrix(Dataframe[,ind.start2])

    # Means
    t1.mean <- mean(t1.sub)
    t2.mean <- mean(t2.sub)

    mean.change <- t2.mean - t1.mean

    # Fin
    final.vector <- c(trialnum, mean.change)

    blank.frame <- rbind(blank.frame, final.vector)

    ind.start1 <- ind.start1 + 1
    ind.start2 <- ind.start2 + 1

  }

  colnames(blank.frame) <- c("Trial", "Mean.Change")

  return(blank.frame)

}
