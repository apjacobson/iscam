#' iscam_addnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays
#' a normal density function.
#' @param x a vector of numeric values.
#' @keywords normal
#' @export
#' @import stats graphics ggplot2
#' @examples
#' data("mtcars")
#' iscam_addnorm(mtcars$wt)

iscam_addnorm <- function(x) {
  df <- data.frame(x) # Converting numeric vector into a data frame
  min = 0 # Min of x axis
  max = max(x) # Max of x axis
  myseq = seq(min, max, .001) # Create numeric sequence from min to max by .001
  ggplot(df, aes(x)) +
    geom_histogram(aes(y = (..density..)), # Histogram of data
      binwidth = (max(x) - min(x)) / 20,
      colour = "black",
      fill = "white") +
    stat_function(geom = "line", # Creating density curve
      fun = dnorm, # Normal density
      args = list(mean = mean(x), sd = sd(x)), # Inputs for normal pdf
      colour = "red") +
    labs(y = "Density", x = deparse(substitute(x)))
}
