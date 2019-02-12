#' iscam_hypernorm Function
#'
#' This function calculates tail probabilities from the hypergeometric distribution.
#' @param k number of successes of interest (integer) or difference in conditional proportions
#' @param total total number of observations in the study
#' @param succ overall number of successes
#' @param n number of observations in group A
#' @param lower.tail a Boolean for finding the probability above (FALSE) or below (TRUE) the inputted value (inclusive)
#' @keywords hypergeometric normal
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_hypernorm(2, 52, 5, 26, lower.tail = TRUE)

iscam_hypernorm <- function (k, total, succ, n, lower.tail) {
  x <- NULL
  if (k < 1)
    k <- round((k * n * (total - n) + n * succ) / total)
  fail <- total - succ
  thisx <- max(0, n - fail):min(n, succ)
  normseq <- seq(max(0, n - fail), min(n, succ), 0.001)
  normmean <- n * succ / total
  normsd <- sqrt(n * succ / total * (total - n) / total * (total -
                                                            succ) / (total - 1))
  minx <- max(0, normmean - 4 * normsd)
  maxx <- min(n, normmean + 4 * normsd)
  myy <- dhyper(floor(normmean), succ, fail, n) / 2

  df <-
    data.frame(x = thisx, y = dhyper(thisx, succ, fail, n)) #putting data into data frame

  plot <- ggplot(df, aes_string(x = "x", y = "y", width = 0.05)) +
    geom_bar(
      stat = "identity",  # creating bars
      col = "black",
      fill = "dimgrey",
      alpha = .2
    ) +
    stat_function(
      fun = dnorm,
      args = list(mean = normmean, sd = normsd),
      color = "dimgrey"
    )
  if (lower.tail) {
    myTitle <- paste("Pr(X \u2264 ", k, ") = ", sep = "")
    hyperprob <- phyper(k, succ, fail, n)
    showhyperprob <- format(hyperprob, digits = 4)
    normprob1 <- pnorm(k, normmean, normsd)
    shownormprob1 <- format(normprob1, digits = 4)
    normprob2 <- pnorm(k + 0.5, normmean, normsd)
    shownormprob2 <- format(normprob2, digits = 4)

    subtitle1 <- paste("Hypergeometric: ", showhyperprob, "\n")
    subtitle2 <- paste("Normal Approximation:", shownormprob1, "\n")
    subtitle3 <-
      paste("Normal Approximation with Continuity:", shownormprob2)
    subtitle <-
      paste(subtitle1, subtitle2, subtitle3, sep = "") #creating subtitle

    plot1 <- plot +
      stat_function(
        fun = dnorm,  #shade in under normal curve
        args = list(mean = normmean, sd = normsd),
        xlim = c(minx, k),
        geom = "area",
        color = "dimgrey",
        fill = "dimgrey",
        alpha = 0.4
      ) +
      stat_function(
        fun = dnorm,  # shade in continuity correction
        args = list(mean = normmean, sd = normsd),
        xlim = c(k, k + 0.5),
        geom = "area",
        color = "orange",
        fill = "orange",
        alpha = 0.4
      ) +
      geom_bar(
        stat = "identity",  # fills in part of histogram
        data = subset(df, x <= k),
        colour = "black",
        fill = "#3366FF"
      )
    output3 <-
      paste(
        "Pr(X \u2264 ",
        k,
        ") = ",
        "\n",
        "Hypergeometric: ",
        showhyperprob,
        "\n",
        subtitle2,
        subtitle3,
        sep = ""
      )
  } else if (!lower.tail) {
    myTitle <- paste("Pr(X \u2265 ", k, ") = ", sep = "")
    hyperprob <- 1 - phyper(k - 1, succ, fail, n)
    showhyperprob <- format(hyperprob, digits = 4)
    normprob1 <- pnorm(k, normmean, normsd, lower.tail = FALSE)
    shownormprob1 <- format(normprob1, digits = 4)
    normprob2 <- pnorm(k - 0.5, normmean, normsd, lower.tail = FALSE)
    shownormprob2 <- format(normprob2, digits = 4)

    subtitle1 <- paste("Hypergeometric: ", showhyperprob, "\n")
    subtitle2 <- paste("Normal Approximation:", shownormprob1, "\n")
    subtitle3 <-
      paste("Normal Approximation with Continuity:", shownormprob2)
    subtitle <-
      paste(subtitle1, subtitle2, subtitle3, sep = "")  # creating subtitle

    plot1 <- plot +
      stat_function(
        fun = dnorm,  # shade in under normal curve
        args = list(mean = normmean, sd = normsd),
        xlim = c(k, maxx),
        geom = "area",
        color = "dimgrey",
        fill = "dimgrey",
        alpha = 0.4
      ) +
      stat_function(
        fun = dnorm,  # shade in continuity correction
        args = list(mean = normmean, sd = normsd),
        xlim = c(k - 0.5, k),
        geom = "area",
        color = "orange",
        fill = "orange",
        alpha = 0.4
      ) +
      geom_bar(
        stat = "identity",  # fills in part of bar graph
        data = subset(df, x >= k),
        colour = "black",
        fill = "#3366FF"
      )
    output3 <-
      paste(
        "Pr(X \u2265 ",
        k,
        ") = ",
        "\n",
        "Hypergeometric: ",
        showhyperprob,
        "\n",
        subtitle2,
        subtitle3,
        sep = ""
      )
  }
  # this is the plot that will be printed
  finalplot <- plot1 + labs(
    x = "X = Number of Successes",
    y = "Probability",
    title = myTitle,
    subtitle = subtitle
  ) +
    theme_bw(14, "serif") +
    theme(plot.subtitle = element_text(color = "#3366FF"),
          plot.title = element_text(size = 14))
  print(finalplot)
  output1 <-
    paste("Hypergeometric (N = ", total, ", M = ", succ, ", n = ", n, ")", sep = "")
  output2 <-
    paste("Normal(mean = ",
          normmean,
          ", SD = ",
          signif(normsd / n, 4),
          ")",
          sep = "")
  cat(output1, "\n")
  cat(output2, "\n")
  cat(output3)
}
