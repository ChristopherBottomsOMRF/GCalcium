#' @title Transform values into z scores
#'
#' @description `z_score` transforms input values to z scores. Allows user input of mu and sigma values for comparing distributions.
#'
#' @importFrom stats sd mad median
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
#' z_score(xvals = eventvals, mu = base.mu, sigma = base.sigma)
#' @export

z_score <- function(xvals, mu = NULL, sigma = NULL,
                    metric.type = 'mean',
                    mad.const = 1.4826){

  ### mean
  if(is.null(mu) & metric.type == 'median'){
    mu <- median(xvals)
  } else {
    mu <- mean(xvals)
  }

  if(is.null(sigma) & metric.type == 'median'){
    sigma <- mad(x = xvals, constant = mad.const)
  } else {
    sigma <- sd(x = xvals)
  }

  ### Choose function based on user input

  z.score <- (xvals - mu) / sigma

  return(z.score)

}

