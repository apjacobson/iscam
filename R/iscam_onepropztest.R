#' iscam_onepropztest Function
#'
#' This function calculates a one proportion z-test and/or interval.
#' @param observed number of success or sample proportion (assumed if value is less than 1)
#' @param n sample size
#' @param hypothesized hypothesized probability of success
#' @param alternative form of alternative hypothesis "less", "greater", or "two.sided"
#' @param conf.level confidence level(s) for a two-sided confidence interval
#' @keywords one proportion z test
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_onepropztest(12, 15, .70, "less")
#' iscam_onepropztest(12, 15, .6, "greater")
#' iscam_onepropztest(12, 15, .5, "two.sided", 0.95)
#' iscam_onepropztest(12, 15, conf.level = c(.9, .95))

iscam_onepropztest <-
  function(observed,
           n,
           hypothesized = NULL,
           alternative = "two.sided",
           conf.level = NULL) {
    if (observed < 1) {  # converting sample proportion to number of successes
      observed <-
        round(n * observed)
    }
    # R's built in one prop z test
    proptest <-
      prop.test(observed, n, hypothesized, alternative, correct = FALSE)

    cat("\n", "One Proportion z test\n", sep = "", "\n")  # output
    statistic <- signif(observed / n, 4)   # proportion of successes
    cat(
      paste(
        "Data: observed successes = ",
        observed,
        ", sample size = ",
        n,
        ", sample proportion = ",
        statistic,
        "\n\n",
        sep = ""
      )
    )
    zvalue <- NULL
    pvalue <- NULL

    # When a hypothesized value is specified
    if (!is.null(hypothesized)) {
      zvalue <-
        (statistic - hypothesized) / sqrt(hypothesized * (1 - hypothesized) / n)
      cat("z-statistic:", signif(zvalue, 4), "\n")
      subtitle1 <- paste("z-statistic:", signif(zvalue, 4), "\n")
      pvalue <- signif(proptest$p.value, 4)
      subtitle2 <- paste("p-value:", pvalue)
      cat("p-value:", pvalue, "\n")

      SD <- sqrt(hypothesized * (1 - hypothesized) / n) # std error

      # finding x limits
      min <- min(hypothesized - 4 * SD, hypothesized - abs(zvalue) * SD -
                   .001)
      max <- max(hypothesized + 4 * SD, hypothesized + abs(zvalue) * SD +
                   .001)
      x <- seq(min, max, .001)

      my_seq <- -3:3   # setting up 2nd x axis
      xticks <- hypothesized + my_seq * SD
      perc <- c("z=-3", "z=-2", "z=-1", "z=0", "z=1", "z=2", "z=3")
      l <- paste(round(xticks, 2), perc, sep = "\n")

      data <- data.frame(x = x, y = dnorm(x, hypothesized, SD))
      plot <- ggplot(data, aes_string(x = "x", y = "y"))
      if (alternative == "less") {
        plot1 <- plot +
          stat_function(
            fun = dnorm,  # drawing normal density curve
            args = list(mean = hypothesized, sd = SD),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,  # shading in normal curve
            args = list(mean = hypothesized, sd = SD),
            xlim = c(min, statistic),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          )
      } else if (alternative == "greater") {
        plot1 <- plot +
          stat_function(
            fun = dnorm,  # drawing normal density curve
            args = list(mean = hypothesized, sd = SD),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,  # shading in normal curve
            args = list(mean = hypothesized, sd = SD),
            xlim = c(statistic, max),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          )
      } else if (alternative == "two.sided") {
        subtitle2 <- paste("two-sided p-value:", pvalue)
        if (statistic < hypothesized) {
          lowerstat <- statistic
          upperstat <- hypothesized + (hypothesized - statistic)
        } else {
          lowerstat <- hypothesized - (statistic - hypothesized)
          upperstat <- statistic
        }
        plot1 <- plot +
          stat_function(
            fun = dnorm,  # drawing normal density curve
            args = list(mean = hypothesized, sd = SD),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,  # shading in normal curve
            args = list(mean = hypothesized, sd = SD),
            xlim = c(min, lowerstat),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          ) +
          stat_function(
            fun = dnorm,  # shading in normal curve
            args = list(mean = hypothesized, sd = SD),
            xlim = c(upperstat, max),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          )
      }
      mysubtitle <- paste(subtitle1, subtitle2)

      finalplot <- plot1 +
        geom_segment(aes(
          x = min,
          xend = max,
          y = 0,
          yend = 0
        ), color = "dodgerblue") +
        labs(x = "Sample Proportions",
             subtitle = mysubtitle) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#3366FF")) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        scale_x_continuous(breaks = xticks,
                           labels = l,
                           limits = c(min, max))


      print(finalplot)
    }

    lower = 0
    upper = 0
    if (!is.null(conf.level)) {
      for (k in 1:length(conf.level)) {
        if (conf.level[k] > 1)
          conf.level[k] = conf.level[k] / 100
        #myout=prop.test(observed, n, p=statistic, alternative="two.sided", conf.level[k], correct=FALSE)
        criticalvalue <- qnorm((1 - conf.level[k]) / 2)
        lower[k] <-
          signif(statistic + criticalvalue * sqrt(statistic * (1 - statistic) / n), 4)
        upper[k] <-
          signif(statistic - criticalvalue * sqrt(statistic * (1 - statistic) / n), 4)
        multconflevel <- 100 * conf.level[k]
        cat(multconflevel,
            "% Confidence interval for pi: (",
            lower[k],
            ", ",
            upper[k],
            ") \n",
            sep = "")
      }
    }


    # making plots when hypothesized and alternative are not specified, but conf.level is specified
    if (is.null(hypothesized)) {
      SDphat <- sqrt(statistic * (1 - statistic) / n)
      min <- statistic - 4 * SDphat
      max <- statistic + 4 * SDphat
      CIseq <- seq(min, max, .001)

      if (length(conf.level) == 1) {
        data <- data.frame(x = CIseq, y = dnorm(CIseq, lower[1], SDphat))
        nicelower <- round(lower, 4)
        niceupper <- round(upper, 4)
        title2.1 <-
          substitute(paste("Normal (", mean == x1, ", ", SD == x2, ")"),
                     list(x1 = signif(lower[1], 4), x2 = signif(SDphat, 4)))
        plot2.1 <- ggplot(data, aes_string(x = "x", y = "y")) +
          stat_function(
            fun = dnorm,
            #drawing normal density curve
            args = list(mean = lower[1], sd = SDphat),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,
            #shading in normal curve
            args = list(mean = lower[1], sd = SDphat),
            xlim = c(statistic, max),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          ) +
          labs(x = "Sample Proportions",
               title = title2.1) +
          theme(
            axis.title.y = element_blank(),
            #removing y axis
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          ) +
          geom_segment(aes(
            x = min,
            xend = max,
            y = 0,
            yend = 0
          ), color = "dodgerblue")
        title2.2 <-
          substitute(paste("Normal (", mean == x1, ", ", SD == x2, ")"),
                     list(x1 = signif(upper[1], 4), x2 = signif(SDphat, 4)))
        plot2.2 <- ggplot(data, aes_string(x = "x", y = "y")) +
          stat_function(
            fun = dnorm,
            #drawing normal density curve
            args = list(mean = upper[1], sd = SDphat),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,
            #shading in normal curve
            args = list(mean = upper[1], sd = SDphat),
            xlim = c(min, statistic),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          ) +
          labs(x = "Sample Proportions",
               title = title2.2) +
          theme(
            axis.title.y = element_blank(),
            #removing y axis
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          ) +
          geom_segment(aes(
            x = min,
            xend = max,
            y = 0,
            yend = 0
          ), color = "dodgerblue")
        CIsubtitle <- paste(100 * conf.level[1], "% Confidence Interval", sep = "")
        midpoint <- signif((upper[1] - lower[1]) / 2 + lower[1], 4)
        plot2_3 <- ggplot(data, aes_string(x = "x", y = "y")) +
          geom_segment(aes(
            x = upper[1],
            y = 1,
            xend = lower[1],
            yend = 1
          ), data = data) +
          geom_point(data = data,
                     mapping = aes(x = upper[1], y = 1)) +
          geom_point(data = data,
                     mapping = aes(x = lower[1], y = 1)) +
          geom_point(data = data,
                     mapping = aes(x = midpoint, y = 1)) +
          geom_text(x = midpoint, y = 1.2, label = midpoint) +
          geom_text(x = lower + 0.01,
                    y = 1.2,
                    label = nicelower) +
          geom_text(x = upper - 0.01,
                    y = 1.2,
                    label = niceupper) +
          labs(x = "Process Probability",
               title = CIsubtitle) +
          theme(
            axis.title.y = element_blank(),
            #removing y axis
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
        grid.arrange(plot2.1, plot2.2, plot2_3, nrow = 3)
      } else if (length(conf.level) > 1) {
        par(mar = c(4, 2, 1.5, 4),
            mfrow = c(length(conf.level), 1))
        for (k in 1:length(conf.level)) {
          plot(
            c(min, statistic, max),
            c(1, 1, 1),
            pch = c(".", "^", "."),
            ylab = " ",
            xlab = "process probability",
            ylim = c(1, 1)
          )
          abline(v = statistic, col = "gray")
          text(min * 1.1, 1, labels = paste(conf.level[k] * 100, "% CI:"))
          text(statistic, .9, labels = signif(statistic, 4))
          text(lower[k],
               1,
               labels = signif(lower[k], 4),
               pos = 3)
          text(upper[k],
               1,
               labels = signif(upper[k], 4),
               pos = 3)
          points(c(lower[k], upper[k]), c(1, 1), pch = c("[", "]"))
          lines(c(lower[k], upper[k]), c(1, 1))
        }
      }
    }
  }
