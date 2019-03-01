#' iscam_hyperprob Function
#'
#' This function calculates tail probabilities from the hypergeometric distribution.
#' @param k number of successes of interest (integer) or difference in conditional proportions
#' @param total total number of observations in the study
#' @param succ overall number of successes
#' @param n number of observations in group A
#' @param lower.tail a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#' @keywords hypergeometric probability
#' @export
#' @import graphics ggplot2
#' @examples
#' iscam_hyperprob(2, 52, 5, 13, lower.tail = FALSE)

iscam_hyperprob <- function(k, total, succ, n, lower.tail) {
  x = NULL
  # Converting proportion into number of successes
  if (k < 1 & k > 0) {
    k <- round((k * (total - n) * n + succ * n) / total)
  }
  fail <- total - succ  # Number of failures
  thisx <- max(0, n - fail):min(n, succ)  # creating x axis
  df <- data.frame(x = thisx, y = dhyper(thisx, succ, fail, n))
  myTitle <-
    paste("Hypergeometric (N = ", total, ", M = ", succ, ", n = ", n, ")", sep = "")
  if (lower.tail) {
    this.prob <- phyper(k, succ, fail, n)  # Hypergeometric probability
    showprob <- format(this.prob, digits = 4)  # formatting hyper. prob.
    mySubtitle <-
      paste("P(X \u2264 ", k, ") = ", showprob, sep = "")
    plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.25)) +
      geom_bar(  # Setting up Bar graph
        stat = "identity",
        col = "black",
        fill = "grey",
        alpha = .2
      ) +
      geom_bar(
        stat = "identity",  # fills in part of bar graph
        data = subset(df, x <= k),
        colour = "black",
        fill = "#3366FF",
        alpha = .7
      ) +
      labs(  # Graph labels
        x = "Number of Successess",
        y = "Probability",
        title = myTitle,
        subtitle = mySubtitle
      ) +
      theme_bw(16, "serif") +  # editing font
      theme(plot.subtitle = element_text(color = "#3366FF"))
    cat("Probability", k, "and below =", this.prob, "\n")  # output
  }
  if (!lower.tail) {
    this.prob <- 1 - phyper(k - 1, succ, fail, n)  # Hypergeometric probability
    showprob <- format(this.prob, digits = 4)  # formatting probability
    mySubtitle <-
      paste("P(X \u2265 ", k, ") = ", showprob, sep = "")
    plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.25)) +
      geom_bar(  # Setting up bar graph
        stat = "identity",
        col = "black",
        fill = "grey",
        alpha = .2
      ) +
      geom_bar(
        stat = "identity",  # fills in part of bar graph
        data = subset(df, x >= k),
        colour = "black",
        fill = "#3366FF",
        alpha = .7
      ) +
      labs(  # Graph labels
        x = "Number of Successess",
        y = "Probability",
        title = myTitle,
        subtitle = mySubtitle
      ) +
      theme_bw(16, "serif") +  # editing font
      theme(plot.subtitle = element_text(color = "#3366FF"))
    cat("Probability", k, "and above =", this.prob, "\n")
  }
  print(plot1)
}
