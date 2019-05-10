#' @title Centers individual or multiple trials
#'
#' @description `center_trials` subtracts summarized data during a user-specified time period from trial values
#'
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trials a single trial number or vector of trial numbers to center
#' @param Baseline.times range of time from Baseline.frame to compute the baseline value from
#' @param Baseline.frame a GCalcium-format data frame or matrix containing the baseline period. If frame is not specified, Dataframe is automatically used
#' @param Method the type of centering to be used
#' @return a GCalcium-format data frame with centered values
#' @examples
#' ### Format data frame
#' df.new <- format_data(GCaMP)
#'
#' ### Transform into percent baseline: relative to -3s to -1s before epoc
#' center_trials(Dataframe = df.new, Trials = c(1, 2, 3), Baseline.times = c(-3, -1), Method = 'perc_baseline')
#' @export

center_trials <- function(Dataframe, Trials, Baseline.times, Baseline.frame = FALSE, Method = 'perc_baseline') {

  trial.inds <- c(Trials + 1)
  trial.df <- Dataframe[trial.inds]

  ### Use Dataframe if Baseline.frame is not specified
  if(Baseline.frame == FALSE){
    Baseline.frame <- Dataframe
  }

  ### Make stuff for call
  baseline.start <- Baseline.times[1]
  baseline.stop <- Baseline.times[2]

  ### Isolate baseline df for computation
  baseline.df <- Baseline.frame[Baseline.frame[,1] >= baseline.start &
                               Baseline.frame[,1] <= baseline.stop,
                               trial.inds]

  ##### Methods ---------------

  ### Percent baseline
  if(Method == 'perc_baseline'){

    base.means <- apply(baseline.df, 2, mean)

    values.c <- mapply(perc_baseline, trial.df, base.means)

  } else if(Method == 'z_score') {

    base.means <- apply(baseline.df, 2, mean)
    base.sdeez <- apply(baseline.df, 2, sd)

    values.c <- mapply(z_score, trial.df, base.means, base.sdeez)

  } else {
    print('Please enter valid centering method.')
    break
  }

  return(values.c)

}
