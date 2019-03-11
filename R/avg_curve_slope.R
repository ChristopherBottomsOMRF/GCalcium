#' @title Get average curve slopes
#'
#' @description `avg_curve_slope` finds the average curve slopes for a trial using inflect_points_df and lm
#'
#' @note curves of a single value will result in the average slope being labeled as NA
#' @importFrom stats lm
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trial a single trial number
#' @return vector of average rate of change for each curve
#' @examples
#' df.new <- format_data(data = GCaMP)
#' avg_curve_slope(Dataframe = df.new, Trial = 1)
#' @export

avg_curve_slope <- function(Dataframe, Trial){

  Summaryframe <- inflect_points_df(Dataframe = Dataframe, Trial = Trial)
  Summaryframe$Curve.Num <- as.factor(Summaryframe$Curve.Num)

  ### Get all info
  curveline <- with(Summaryframe,
                    by(Summaryframe, Curve.Num,
                       function(Summaryframe) lm(Raw.Values ~ Time, data = Summaryframe)))

  ### Extract slopes
  ROCs <- sapply(1:nrow(curveline), function(curvenum){
    curveline[[curvenum]]$coefficients[[2]]
  })

  return(ROCs)

}
