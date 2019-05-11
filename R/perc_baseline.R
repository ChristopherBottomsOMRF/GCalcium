#' @title Find the percent change from baseline
#'
#' @description `perc_baseline` calculates the percent change from a user-specified baseline
#'
#' @param xvals vector of numbers
#' @param base.val the baseline value to be centered by
#' @return a vector with values transformed to percent baseline
#' @examples
#' ### Format data frame
#' # Format data
#' df.new <- format_data(GCaMP)
#'
#' # Split data
#' basevals <- df.new$Trial1[df.new$Time <= 0]
#' eventvals <- df.new$Trial1[df.new$Time > 0]
#'
#' # Find baseline (pre-epoc) values
#' base.mean <- mean(basevals)
#'
#' # Compute values
#' perc_baseline(x = eventvals, base.val = base.mean)
#' @export

perc_baseline <- function(xvals, base.val) {
  p.baseline <- ( (xvals - base.val) / base.val ) * 100
  return(p.baseline)
}
