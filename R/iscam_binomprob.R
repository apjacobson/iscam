#' iscam_binomprob Function
#'
#' This function calculates tail probabilities from the binomial distribution.  
#' @param k number of successes of interest (must be an integer)
#' @param n number of trials (zero or more)
#' @param prob probability of success on each trial
#' @param lower.tail logical; a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive) 
#' @keywords binomial
#' @import stats graphics ggplot2
#' @export
#' @examples
#' iscam_binomprob(20, 30, 0.5, lower.tail = TRUE)
#' iscam_binomprob(6, 20, 0.4, lower.tail = FALSE)

iscam_binomprob <- function(k, n, prob, lower.tail) {
  myTitle <-
    substitute(paste("Binomial (", n == x1, ", ", pi == x2, ")",),
               list(x1 = n, x2 = prob))  # graph's main title
  thisx <- 0:n  # range of data (number of trials)
  if (lower.tail) {
    this.prob <- pbinom(k, n, prob)  # calculating binomial probability
    showprob <-
      format(this.prob, digits = 4)  # formatting probability to include only 4 sig. figs.
    mySubtitle <-
      paste("P(X \u2264", k, ") =", showprob)  # creating subtitle
    df <-
      data.frame(x = thisx, y = dbinom(thisx, n, prob))  # putting data into data frame
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) +
      geom_bar(  # creating bar graph
        stat = "identity",
        col = "black",
        fill = "grey",
        alpha = .2
      ) +
      geom_bar(
        stat = "identity",  # fills in part of bargraph
        data = subset(df, x <= k),
        colour = "black",
        fill = "#3366FF",
        alpha = .7
      ) +
      labs(  # label names
        x = "Number of Successess",
        y = "Probability",
        title = myTitle,
        subtitle = mySubtitle
      ) +
      theme_bw(16, "serif") +
      theme(plot.subtitle = element_text(color = "#3366FF"))
    cat("Probability", k, "and below =", this.prob, "\n") #output the binom. prob.
  }
  if (!lower.tail) {
    this.prob <- 1 - pbinom(k - 1, n, prob)
    showprob <- format(this.prob, digits = 4)
    mySubtitle <- paste("P(X \u2265", k, ") =", showprob)
    df <- data.frame(x = thisx, y = dbinom(thisx, n, prob))
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) +
      geom_bar(
        stat = "identity",
        col = "black",
        fill = "grey",
        alpha = .2
      ) +
      geom_bar(
        stat = "identity",
        data = subset(df, x >= k),
        colour = "black",
        fill = "#3366FF",
        alpha = .7
      ) +
      labs(
        x = "Number of Successes",
        y = "Probability",
        title = myTitle,
        subtitle = mySubtitle
      ) +
      theme_bw(16, "serif") +
      theme(plot.subtitle = element_text(color = "#3366FF"))
    cat("Probability", k, "and above =", this.prob, "\n")
  }
  print(plot1)
}
