#' iscam_chisqprob Function
#'
#' This function calculates the upper tail probability for the chi-square distribution
#' @param xval x value
#' @param df degrees of freedom
#' @keywords chi square
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_chisqprob(10, 4)

iscam_chisqprob <- function(xval, df) {
  maxx <- max(20, xval, df)  # finding max x value
  mydf <- data.frame(x = c(0, maxx))  # creating data frame of values from 0 to max
  chisqprob <- pchisq(xval, df, lower.tail = FALSE)  # chi square probability
  showprob <- format(chisqprob, digits = 4)
  mytitle <-
    paste("Chi-Square Distribution with", df, "Degrees of Freedom")
  mysubtitle <- paste("P(X \u2265", xval, ") =", showprob)
  p <- ggplot(mydf, aes(x = x)) +
    stat_function(fun = dchisq,  # creating t density curve
                  args = list(df = df),
                  color = "dodgerblue") +
    geom_segment(aes(  # horizontal line at the bottom of curve
      x = 0,
      xend = 20,
      y = 0,
      yend = 0
    ), color = "dodgerblue") +
    labs(
      x = "Chi-square values",
      y = "",
      title = mytitle,
      subtitle = mysubtitle
    ) +
    theme_bw(12, "serif") +
    theme(
      plot.subtitle = element_text(color = "dodgerblue4"),
      axis.title.y = element_blank(),  # getting rid of y axis
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    stat_function(  # shading in t density curve
      fun = dchisq,
      args = list(df = df),
      xlim = c(xval, maxx),
      geom = "area",
      color = "dodgerblue4",
      fill = "dodgerblue4"
    )
  print(p)
  cat(c("Chi-square probability:", showprob), "\n")
}