#' @title Plot specified trials
#'
#' @description `plot_trials` plots the user-inputted trials. This function automatically adjusts the x- and y- axis limits to fit all data
#'
#' @importFrom graphics legend lines par plot
#' @param Dataframe a GCalcium-format data frame or matrix
#' @param Trials set of trials to be plotted
#' @param ... extra commands to be called to the blank base plot
#' @return a single plot of user-specified trials
#' @examples
#' ### Format data frame
#' df.new <- format_data(GCaMP)
#'
#' ### Specify and plot trials
#' my.trials <- c(1, 2, 7, 8)
#' plot_trials(Dataframe = df.new, Trials = my.trials)
#' @export

plot_trials <- function(Dataframe, Trials, ...){

  trial.inds <- Trials + 1
  num.trials <- length(Trials)
  leg.labels <- c()
  leg.colors <- c()

  # min/max x/y values
  x.min <- min(Dataframe[1])
  x.max <- max(Dataframe[1])

  y.min <- min(sapply(Dataframe[trial.inds], min))
  y.max <- max(sapply(Dataframe[trial.inds], max))

  # better palette
  color.vector <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3',
                    'mediumorchid2', 'turquoise3', 'wheat4', 'slategray2')

  # default plot to fit all lines
  #plot.new()
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  plot(x = Dataframe[,1], y = Dataframe[,trial.inds[1]],
       xlim = c(x.min, x.max), ylim = c(y.min, y.max), type = 'n',
       xlab = 'Time', ylab = 'Values')

  # draw lines
  for(i in 1:num.trials){

    lines(x = Dataframe[,1], y = Dataframe[,trial.inds[i]], col = color.vector[i])
    leg.labels <- append(leg.labels, paste0("Trial ", Trials[i]))
    leg.colors <- append(leg.colors, color.vector[i])

  }

  legend(x.max + abs(0.005*y.max), y.max, legend = leg.labels,
         col = leg.colors, lty=1, cex=0.8,
         title="Trial", text.font=2)

}
