#' @title Find inflection pooints
#'
#' @description `inflect_points` finds inflection points of waveform data
#'
#' @param x A vector of numbers
#' @return Inflection points of the input vector
#' @examples
#' df.new <- format_data(GCaMP)
#' inflect_points(df.new$Trial1)
#' @export

inflect_points <- function (x){

  # Find inflection points
  inf.pts <- diff(sign(diff(x, na.pad = TRUE)))

  # Add NAs since we removed 2 points by doing first difference twice
  inf.pts <- append(inf.pts, 0)
  inf.pts <- append(inf.pts, 0)

  return(inf.pts)

}
