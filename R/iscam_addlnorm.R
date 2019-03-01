#' iscam_addlnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays
#' a log normal density function.
#' @param x a vector of numeric values.
#' @keywords lognormal
#' @import graphics ggplot2
#' @export
#' @examples
#' data("mtcars")
#' iscam_addlnorm(mtcars$wt)

iscam_addlnorm <- function(x) {
  df <- data.frame(x) # Converting numeric vector x to a data frame
  ggplot(df, aes(x)) +
    geom_histogram(aes_string(y = ("..density..")), # Set density as y axis
                   binwidth = 0.1,
                   colour = "black", # Color of outline of histogram
                   fill = "white") +
    stat_function(geom = "line", # Density curve
      fun = dlnorm, # Log norm density curve
      args = list(meanlog = mean(log(x)), sdlog = sd(log(x))), # Inputs for log normal dist.
      colour = "red") +
    labs(y = "Percent")
}
