#' @title Get mean activity change within a trial
#'
#' @description `within_trial_change` finds the change in mean values between beginning and end of entered time range.
#'
#' @importFrom dplyr case_when
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trial a single trial number
#' @param Beg.period time frame of period 1 to be compared to period 2
#' @param End.period time frame of period 2 to be compared to period 1
#' @return vector with AUC for each curve in a trial
#' @examples
#' # In trial 1, how much did the mean value change between -2s and 0s, and 0 and 2s?
#' df.new <- format_data(data = GCaMP)
#' within_trial_change(Dataframe = df.new, Trial = 1, Beg.period = c(-2, 0), End.period = c(0, 2))
#' @export

within_trial_change <- function(Dataframe, Trial, Beg.period = FALSE, End.period = FALSE){

  #
  trialind <- Trial + 1

  ### Default of first second and last second
  case_when(
    Beg.period == FALSE ~ c(min(Dataframe[1]), min(Dataframe[1]) + 1),
    Beg.period == TRUE ~ Beg.period
  )

  case_when(
    End.period == FALSE ~ c(max(Dataframe[1]) - 1, max(Dataframe[1])),
    End.period == TRUE ~ End.period
  )

  ### Extract time period values
  beg.start <- Beg.period[1]
  beg.stop <- Beg.period[2]
  end.start <- End.period[1]
  end.stop <- End.period[2]

  ### Beginning period subset
  beg1 <- Dataframe[Dataframe[1] >= beg.start & Dataframe[1] <= beg.stop,]
  beg.mean <- mean(beg1[,trialind])

  ### Ending period subset
  end1 <- Dataframe[Dataframe[1] >= end.start & Dataframe[1] <= end.stop,]
  end.mean <- mean(end1[,trialind])

  mean.change <- end.mean - beg.mean

  return(mean.change)

}
