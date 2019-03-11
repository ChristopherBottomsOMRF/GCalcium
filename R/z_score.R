#' @title Transform values into z scores
#'
#' @description `z_score` transforms input values to z scores. Allows user input of mu and sigma values for comparing distributions.
#'
#' @importFrom stats sd
#' @param xvals vector of numbers
#' @param mu the population mean
#' @param sigma the population standard deviation
#' @return a numeric vector of z scores
#' @examples
#' # Format data
#' df.new <- format_data(GCaMP)
#'
#' # Split data
#' basevals <- df.new$Trial1[df.new$Time <= 0]
#' eventvals <- df.new$Trial1[df.new$Time > 0]
#'
#' # Find baseline (pre-epoc) values
#' base.mu <- mean(basevals)
#' base.sigma <- sd(basevals)
#'
#' # Compute values
#' z_score(x = eventvals, mu = base.mu, sigma = base.sigma)
#' @export

z_score <- function(xvals, mu = FALSE, sigma = FALSE){

  ### mean
  if(mu == FALSE){
    mu <- mean(xvals)
  }

  ### sd
  if(sigma == FALSE){
    sigma <- sd(xvals)
  }

  ### fin
  z.score <- (xvals - mu) / sigma

  return(z.score)

}

