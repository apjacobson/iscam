#' iscam_invbinom Function
#'
#' This function calculates the binomial quantile of a specified probability. The integer that achieves at most the stated probability will be returned.
#' @param alpha alpha level
#' @param n sample size
#' @param prob probability of success
#' @param lower.tail upper tail (FALSE) or lower tail (TRUE)
#' @keywords inverse binomial
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_invbinom(.05, 15, .20, lower.tail = FALSE)

iscam_invbinom <- function(alpha, n, prob, lower.tail) {
  x = NULL
  y = NULL
  myTitle <-
    substitute(paste("Binomial (", n == x1, ", ", pi == x2, ")"),
               list(x1 = n, x2 = prob))  # graph's main title

  thisx <- 0:n  # range of data (number of trials)

  if (lower.tail) {
    answer <- qbinom(alpha, n, prob, lower.tail) - 1  # Binomial quantile
    actualprob <-
      format(pbinom(answer, n, prob, lower.tail), digits = 4)  # binomial probability using quantile
    actualprob2 <-
      format(pbinom(answer + 1, n, prob, lower.tail), digits = 4)  # 1 over the rejection region
    mySubtitle <-
      paste("P(X \u2264",
            answer,
            ") =",
            actualprob,
            ", P(X \u2264",
            answer + 1,
            ") =",
            actualprob2)  # creating subtitle
    df <-
      data.frame(x = thisx, y = dbinom(thisx, n, prob))  # putting data into data frame
    plot1 <- ggplot(df, aes(x = x, y = y, width = 0.25)) +
      geom_bar(  # bar graph
        stat = "identity",
        col = "black",
        fill = "grey",
        alpha = .2
      ) +
      geom_bar(
        stat = "identity",  # fills in part of bar graph
        data = subset(df, x <= answer),
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
      theme_bw(16, "serif") +  # editing font of title
      theme(plot.subtitle = element_text(color = "#3366FF"))
    cat("The observation with at most",
        alpha,
        "probability at or below is",
        answer,
        "\n")  # output the binom. prob.
  }
  if (!lower.tail) {
    answer = qbinom(alpha, n, prob, lower.tail) + 1
    actualprob = format(pbinom(answer - 1, n, prob, lower.tail), digits =
                          4)
    actualprob2 <-
      format(pbinom(answer - 2, n, prob, lower.tail), digits = 4) # 1 below the rejection region
    mySubtitle <-
      paste("P(X \u2264",
            answer,
            ") =",
            actualprob,
            ", P(X \u2264",
            answer - 1,
            ") =",
            actualprob2) #creating subtitle
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
        data = subset(df, x >= answer),
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
    cat("The observation with at most",
        alpha,
        "probability at or above is",
        answer,
        "\n")
  }
  print(plot1)
}
