#' iscam_binompower Function
#'
#' This function determines the rejection region corresponding to the level of
#' significance and the first probability.
#' A second, optional probability can be specified to determine the power of the test.
#' @param LOS level of significance
#' @param n number of trials (zero or more)
#' @param prob1 probability of success on each trial
#' @param alternative allows you to specify whether you want to find the probability
#' "less" or "greater" or a symmetric "two.sided" probability
#' @param prob2 NULL or a second probability
#' @param explain logical, default = FALSE. Set to TRUE to see type I and II error and power on the graph
#' @keywords binomial power rejection region
#' @import stats graphics ggplot2 gridExtra
#' @export
#' @examples
#' iscam_binompower(.05, 30, 0.5, alternative = "greater")
#' iscam_binompower(.10, 55, 0.10, alternative = "less")
#' iscam_binompower(.05, 20, 0.5, "two.sided", 0.6)
#' iscam_binompower(.05, 20, 0.25, alternative = "greater", 0.333, explain = TRUE)

iscam_binompower <-
  function(LOS,
           n,
           prob1,
           alternative,
           prob2 = NULL,
           explain = FALSE) {

    # Calculating x limits for graph
    x = NULL
    minx <-
      max(0, min(n * prob1 - 4 * sqrt(prob1 * (1 - prob1) * n), n * prob2 - 4 *
                   sqrt(prob2 * (1 - prob2) * n)))
    maxx <-
      min(n, max(n * prob1 + 4 * sqrt(prob1 * (1 - prob1) * n), n * prob2 + 4 *
                   sqrt(prob2 * (1 - prob2) * n)))
    thisx = 0:n
    maintitle <-
      substitute(paste("Binomial (", n == x1, ", ", pi == x2, ")"),
                 list(x1 = n, x2 = prob1))


    if (alternative == "less") {
      rr <- qbinom(LOS, n, prob1) - 1  # finding rejection region
      this.prob1 <- pbinom(rr, n, prob1)  # pvalue
      showprob1 <- format(this.prob1, digits = 4)  # formatting pvalue
      subtitle <-
        paste("P(X \u2264 ", rr, ") = ", showprob1, sep = "")  # creating subtitle
      df <- data.frame(x = thisx, y = dbinom(thisx, n, prob1))  # Creating data frame
      if (!isTRUE(explain)) {
        plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15)) +
          geom_bar(  # bar graph
            stat = "identity",
            col = "black",
            fill = "grey",
            alpha = .2
          ) +
          geom_bar(
            stat = "identity",  #fills in part of histogram
            data = subset(df, x <= rr),
            colour = "black",
            fill = "#007f80",
            alpha = .7
          ) +
          guides(fill = FALSE)  # removes default legends
      } else if (isTRUE(explain)) {  # Shading in type I error region
        plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15)) +
          geom_bar(
            stat = "identity",
            col = "black",
            fill = "grey",
            alpha = .2
          ) +
          geom_bar(
            stat = "identity",  # fills in type I part of histogram
            data = subset(df, x <= rr),
            colour = "black",
            aes(fill = "red"),
            alpha = .7
          ) +
          scale_fill_manual(values = "red",
                            name = "",
                            labels = "Type I Error")
      }
      cat(paste("Probability ", rr, " and below = ", showprob1, "\n", sep = ""))  # output in R  console
    } else if (alternative == "greater") {
      rr <- qbinom(LOS, n, prob1, FALSE) + 1  # calculate rejection region
      this.prob1 <- 1 - pbinom(rr - 1, n, prob1)  # calculate binom. prob.
      showprob1 <- format(this.prob1, digits = 4)
      subtitle <-
        paste("P(X \u2265 ", rr, ") = ", showprob1, sep = "")
      df <- data.frame(x = thisx, y = dbinom(thisx, n, prob1))
      if (!isTRUE(explain)) {
        plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15)) +
          geom_bar(
            stat = "identity",
            col = "black",
            fill = "grey",
            alpha = .2
          ) +
          geom_bar(
            stat = "identity",  # fills in part of histogram in rejection region
            data = subset(df, x >= rr),
            colour = "black",
            alpha = .7,
            fill = "#007f80"
          )
      } else if (isTRUE(explain)) {  # shades in type i error
        plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15)) +
          geom_bar(
            stat = "identity",
            col = "black",
            fill = "grey",
            alpha = .2
          ) +
          geom_bar(
            stat = "identity",
            data = subset(df, x >= rr),  # fills in type i error part of histogram
            colour = "black",
            aes(fill = "red"),
            alpha = .7
          ) +
          scale_fill_manual(values = "red",  # editing legend
                            name = "",
                            labels = "Type I Error")
      }
      cat(paste("Probability ", rr, " and above = ", showprob1, "\n", sep = ""))
    }  else if (alternative == "two.sided") {
      lowerrr <- qbinom(LOS / 2, n, prob1) - 1  # lower rejection region
      upperrr <- qbinom(LOS / 2, n, prob1, FALSE) + 1  # upper rejection region
      lowerprob1 <- pbinom(lowerrr, n, prob1)
      upperprob1 <- pbinom(upperrr - 1, n, prob1, FALSE)
      showlowerprob1 <-
        format(lowerprob1, digits = 4)
      showupperprob1 = format(upperprob1, digits = 4)
      showprob1 <- format(lowerprob1 + upperprob1, digits = 4)
      subtitle <-
        paste("P(X \u2264 ",
              lowerrr,
              ") + P(X \u2265 ",
              upperrr,
              ") = ",
              showprob1,
              sep = "")
      df <- data.frame(x = thisx, y = dbinom(thisx, n, prob1))
      if (!isTRUE(explain)) {
        plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15)) +
          geom_bar(
            stat = "identity",
            col = "black",
            fill = "grey",
            alpha = .2
          ) +
          geom_bar(
            stat = "identity",
            data = subset(df, x <= lowerrr | x >= upperrr),  # shading in tails
            colour = "black",
            fill = "#007f80",
            alpha = .7
          )
      } else if (isTRUE(explain)) {  # shading in type i error
        plot1 <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15)) +
          geom_bar(
            stat = "identity",
            col = "black",
            fill = "grey",
            alpha = .2
          ) +
          geom_bar(
            stat = "identity",
            data = subset(df, x <= lowerrr | x >= upperrr),
            colour = "black",
            aes(fill = "red"),
            alpha = .7
          ) +
          scale_fill_manual(values = "red",
                            name = "",
                            labels = "Type I Error")
      }
      cat(paste("Probability in rejection region = ", showprob1, "\n", sep =
                  ""))
    }
    else
      stop("Check input for alternative")

    # when prob2 is specified
    if (!is.null(prob2)) {
      maintitle2 <-
        substitute(paste("Binomial (", n == x1, ", ", pi == x2, ")"),
                   list(x1 = n, x2 = prob2))
      if (alternative == "less") {
        this.prob2 = pbinom(rr, n, prob2)
        showprob2 = format(this.prob2, digits = 4)
        subtitle2 <-
          paste("P(X \u2264 ", rr, ") = ", showprob2, sep = "")
        df <- data.frame(x = thisx, y = dbinom(thisx, n, prob2))
        if (!isTRUE(explain)) {
          plot2 <- ggplot(df, aes_string(
            x = "x",
            y = "y",
            width = 0.15
          )) +
            geom_bar(
              stat = "identity",
              col = "black",
              fill = "grey",
              alpha = .2
            ) +
            geom_bar(
              stat = "identity",
              #fills in power part of histogram
              data = subset(df, x <= rr),
              colour = "black",
              fill = "#007f80",
              alpha = .7
            )
        } else if (isTRUE(explain)) {
          plot2 <- ggplot(df, aes_string(
            x = "x",
            y = "y",
            width = 0.15
          )) +
            geom_bar(
              stat = "identity",
              col = "black",
              fill = "grey",
              alpha = .2
            ) +
            geom_bar(
              stat = "identity",
              #fills in power part of histogram
              data = subset(df, x <= rr),
              colour = "black",
              aes(fill = "limegreen")
            ) +
            geom_bar(
              stat = "identity",
              #fills in type ii error part of histogram
              data = subset(df, x > rr),
              colour = "black",
              aes(fill = "navy")
            ) +
            scale_fill_manual(
              values = c("limegreen", "navy"),
              name = "",
              labels = c("Power", "Type II Error")
            )
        }
        cat("Probability", rr, "and below =", showprob2)
      }
      else if (alternative == "greater") {
        this.prob2 <- 1 - pbinom(rr - 1, n, prob2)
        showprob2 <- format(this.prob2, digits = 4)
        subtitle2 <-
          paste("P(X \u2265 ", rr, ") = ", showprob2, sep = "")
        df <- data.frame(x = thisx, y = dbinom(thisx, n, prob2))
        if (!isTRUE(explain)) {
          plot2 <- ggplot(df, aes_string(
            x = "x",
            y = "y",
            width = 0.15
          )) +
            geom_bar(
              stat = "identity",
              col = "black",
              fill = "grey",
              alpha = .2
            ) +
            geom_bar(
              stat = "identity",
              #fills in power part of histogram
              data = subset(df, x >= rr),
              colour = "black",
              fill = "#007f80",
              alpha = .7
            )
        } else if (isTRUE(explain)) {
          plot2 <- ggplot(df, aes_string(
            x = "x",
            y = "y",
            width = 0.15
          )) +
            geom_bar(
              stat = "identity",
              col = "black",
              fill = "grey",
              alpha = .2
            ) +
            geom_bar(
              stat = "identity",
              #fills in power part of histogram
              data = subset(df, x >= rr),
              colour = "black",
              aes(fill = "limegreen")
            ) +
            geom_bar(
              stat = "identity",
              #fills in type ii error part of histogram
              data = subset(df, x < rr),
              colour = "black",
              aes(fill = "navy")
            ) +
            scale_fill_manual(
              values = c("limegreen", "navy"),
              name = "",
              labels = c("Power", "Type II Error")
            )
        }
        cat("Probability", rr, "and above =", showprob2)
      }
      else if (alternative == "two.sided") {
        this.prob2 = pbinom(lowerrr, n, prob2) + pbinom(upperrr - 1, n, prob2, FALSE)
        showprob2 = format(this.prob2, digits = 4)
        subtitle2 <-
          paste("P(X \u2264 ",
                lowerrr,
                ") + P(X \u2265 ",
                upperrr,
                ") = ",
                showprob2,
                sep = "")
        df <- data.frame(x = thisx, y = dbinom(thisx, n, prob2))
        if (!isTRUE(explain)) {
          plot2 <- ggplot(df, aes_string(
            x = "x",
            y = "y",
            width = 0.15
          )) +
            geom_bar(
              stat = "identity",
              col = "black",
              fill = "grey",
              alpha = .2
            ) +
            geom_bar(
              stat = "identity",
              data = subset(df, x <= lowerrr | x >= upperrr),
              colour = "black",
              fill = "#007f80",
              alpha = .7
            )
        } else if (isTRUE(explain)) {
          plot2 <- ggplot(df, aes_string(
            x = "x",
            y = "y",
            width = 0.15
          )) +
            geom_bar(
              stat = "identity",
              col = "black",
              fill = "grey",
              alpha = .2
            ) +
            geom_bar(
              stat = "identity",
              data = subset(df, x <= lowerrr | x >= upperrr),
              colour = "black",
              aes(fill = "limegreen")
            ) +
            geom_bar(
              stat = "identity",
              data = subset(df, x > lowerrr & x < upperrr),
              colour = "black",
              aes(fill = "navy")
            ) +
            scale_fill_manual(
              values = c("limegreen", "navy"),
              name = "",
              labels = c("Power", "Type II Error")
            )
        }
        cat("Probability in rejection region =", showprob2, "\n")
      } else
        stop("Check input for alternative")
      plot1 <- plot1 + xlim(minx, maxx) +
        labs(
          x = "Number of Successess",
          y = "Probability",
          title = maintitle,
          subtitle = subtitle
        ) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#007f80"))
      plot2 <- plot2 + xlim(minx, maxx) +
        labs(
          x = "Number of Successess",
          y = "Probability",
          title = maintitle2,
          subtitle = subtitle2
        ) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#007f80"))
      gridExtra::grid.arrange(plot1, plot2, nrow = 2)
    }
    if (is.null(prob2)) {
      plot1 <- plot1 + xlim(minx, maxx) +
        labs(
          x = "Number of Successess",
          y = "Probability",
          title = maintitle,
          subtitle = subtitle
        ) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#007f80"))
      print(plot1)
    }
  }
