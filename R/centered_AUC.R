#' @title Compute centered area under curve
#'
#' @description `centered_AUC` computes the area under the curve (AUC) after centering values using a specified function, such as mean or min. Computed using trapezoidal integration.
#'
#' @importFrom caTools trapz
#' @importFrom stats sd
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trial a single trial number
#' @param FUN a function to apply to each window
#' @return Data frame of AUCs and curve number for each curve
#' @examples
#' # Get AUCs for trial 2, centered at the mean
#' df.new <- format_data(data = GCaMP)
#' centered_AUC(Dataframe = df.new, Trial = 2, FUN = mean)
#' @export

centered_AUC <- function(Dataframe, Trial, FUN = mean){

  Summaryframe <- inflect_points_df(Dataframe = Dataframe, Trial = Trial)
  Summaryframe$Raw.Values.c <- Summaryframe$Raw.Values - FUN(Summaryframe$Raw.Values)
  Summaryframe$Curve.Num <- as.factor(Summaryframe$Curve.Num)

  ### Get all info
  curve.AUCS <- sapply(split(Summaryframe, Summaryframe$Curve.Num), function(df) trapz(df$Time, df$Raw.Values.c))

  ### Add curve numbers and put into data frame
  AUC.df <- data.frame(Curve.Num = 1:length(curve.AUCS),
                       AUC = curve.AUCS)

  return(AUC.df)

}