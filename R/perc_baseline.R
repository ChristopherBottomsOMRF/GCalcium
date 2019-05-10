#' @title Find the percent change from baseline mean
#'
#' @description `perc_baseline` calculates the percent change from the average of a baseline period
#'
#' @param xvals a vector
#' @param base.mean average value of the baseline period
#' @return a vector with values transformed to percent baseline
#' @examples
#' ### Format data frame
#' df.new <- format_data(GCaMP)
#'
#' ### Transform trial 1 into percent baseline: relative to -3s to -1s before epoc
#' basemean <- mean(df.new$Trial1[df.new$Time > -3 & df.new$Time < -1])
#' perc_baseline(xvals = df.new$Trial1, base.mean = basemean)
#' @export

### Add function
perc_baseline <- function(xvals, base.mean){

  perc.baseline <- ( (xvals - base.mean) / base.mean ) * 100
  return(perc.baseline)

}
