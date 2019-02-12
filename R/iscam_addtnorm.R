#' iscam_addtnorm Function
#'
#' This function creates a histogram of the inputted variable and overlays a t density function with df degrees of freedom
#' and a normal density function.
#' @param x a vector of numeric values.
#' @param df degrees of freedom
#' @keywords t
#' @export
#' @import stats graphics ggplot2
#' @examples
#' data("mtcars")
#' iscam_addtnorm(mtcars$wt, 3)

iscam_addtnorm <- function(x, df){
    data <- data.frame(x) # Converting inputted variable into a data frame
    ggplot(data, aes(x)) +
      geom_histogram( # Histogram of inputted variable
        aes_string(y = ("..density..")),
        binwidth = (max(x) - min(x)) / 20,
        colour = "black",
        fill = "white"
      ) +
      stat_function( # t Density curve
        geom = "line",
        fun = dt,
        args = list(df = df),
        aes(colour = "t")
      ) +
      stat_function( # Normal density curve
        geom = "line",
        fun = dnorm,
        args = list(mean = mean(x), sd = sd(x)),
        aes(colour = "Normal")
      ) +
      labs(y = "Density", x = deparse(substitute(x))) +
      # Editing legend
      scale_colour_manual("Distribution", values = c("red", "green"))
}
