#' @title Centers individual or multiple trials
#'
#' @description `center_trials` subtracts summarized data during a user-specified time period from trial values
#'
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trials a single trial number or vector of trial numbers to center
#' @param Baseline.times range of time from Baseline.frame to compute the baseline value from
#' @param Baseline.frame a GCalcium-format data frame or matrix containing the baseline period. If frame is not specified, Dataframe is automatically used
#' @param Method the type of centering to be used
#' @param Summary.type the type of summary statistic to be centered by
#' @return a GCalcium-format data frame with centered values
#' @examples
#' ### Format data frame
#' df.new <- format_data(GCaMP)
#'
#' ### Transform into percent baseline: relative to -3s to -1s before epoc
#' center_trials(Dataframe = df.new, Trials = c(1, 2, 3), Baseline.times = c(-3, -1), Method = 'perc.baseline')
#' @export

center_trials <- function(Dataframe, Trials, Baseline.times, Baseline.frame = NULL, Method = 'perc.baseline',
                          Summary.type = 'mean') {

  ### Use dataframe period as baseline frame if unspecified/null
  if(is.null(Baseline.frame)){
    baseline.frame <- Dataframe
  } else {
    baseline.frame <- Baseline.frame
  }

  ##### Get baseline summary stats --------------

  ### Index stuff
  trial.inds <- c(Trials + 1)
  trial.df <- Dataframe[trial.inds]

  ### Baseline times
  baseline.start <- Baseline.times[2]
  baseline.stop <- Baseline.times[1]

  ### Isolate baseline df for computation
  baseline.df <- baseline.frame[baseline.frame[,1] >= baseline.stop &
                                  baseline.frame[,1] <= baseline.start,
                                trial.inds]

  ### Get summary values using given summary type
  summ.vals <- apply(baseline.df, 2, function(x){
    summ.text <- paste0(Summary.type, '(x)')
    summ.text <- eval(parse(text = summ.text))
    return(summ.text)
  })
  sdeez <- apply(baseline.df, 2, sd)

  ### Choose function based on user input
  summ_func <- switch(Method,

         perc.baseline = function(df = trial.df, sv = summ.vals){
           mapply(perc_baseline, df, sv)
         },

         z.score = function(df = trial.df, m = summ.vals, s = sdeez){
           mapply(z_score, df, m, s)
         }

  )

  ### Use function to get return dataframe
  return.df <- summ_func()
  return.df <- as.data.frame(return.df)
  return.df <- cbind(Time = Dataframe[,1], return.df) # add time labels back in

  return(return.df)

}
