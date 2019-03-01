#' iscam_addexp Function
#'
#' This function creates a histogram of the inputted variable and overlays
#' an exponential density function with lambda = 1/mean.
#' @param x a vector of numeric values.
#' @keywords exponential
#' @export
#' @import graphics ggplot2
#' @examples
#' data("ChickWeight")
#' iscam_addexp(ChickWeight$weight)

iscam_addexp <- function(x) {
  df <- data.frame(x) # Convert inputted numeric vector into data frame
  ggplot(df, aes(x)) +
    geom_histogram(aes_string(y = ("..density..")), # Density on y axis
      #geom_histogram(aes(y = (..count..)/sum(..count..)), if you want percents on y-axis
      binwidth = (max(x) - min(x)) / 20,
      colour = "black", # Color of histogram outline
      fill = "white") +
    stat_function(geom = "line", # Density curve
      fun = dexp, # Exponential curve
      args = list(rate = 1 / mean(x)), # Rate parameter for exponential dist.
      colour = "red") +
    labs(y = "Density", x = deparse(substitute(x))) # Labels for y and x axis
}
