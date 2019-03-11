#' @title Find inflection points and curve numbers
#'
#' @description `inflect_points_df` finds inflection points of activity waves, returning a summarized data frame
#'
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trial a single trial number
#' @return data frame with variables indicating the time, raw values, curve numbers, and inflection points corresponding to each data point of the input
#' @examples
#' df.new <- format_data(GCaMP)
#' inflect_points_df(Dataframe = df.new, Trial = 1)
#' @export

inflect_points_df <- function(Dataframe, Trial){

  Trial.ind <- Trial + 1

  ### Make data frame of time, inflection points, and raw values
  times <- Dataframe[,1]
  raw.vals <- Dataframe[,Trial.ind]
  inf.pts <- as.data.frame(inflect_points(raw.vals))

  ### Classify individual curves
  shape.nums <- as.data.frame(sapply(inf.pts, function(pts) cumsum(pts != 0) + 1L))

  ### Make full data frame
  df <- as.data.frame(cbind(times, raw.vals, shape.nums, inf.pts))
  names(df) <- c("Time", "Raw.Values", "Curve.Num", "Inf.Pts")

  return(df)

}
