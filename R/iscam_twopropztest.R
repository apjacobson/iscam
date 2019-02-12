#' iscam_twopropztest Function
#'
#' This function calculates a two proportion z-test and/or interval.
#' @param observed1 observed number of successes or sample proportions (assumed if value is less than one)
#' @param n1 sample size
#' @param observed2 observed number of successes (assumed if value is less than one)
#' @param n2 sample size
#' @param hypothesized hypothesized difference in probability of success/population proportions (default is zero)
#' @param alternative form of alternative hypothesis ("less", "greater", or "two.sided")
#' @param conf.level Input confidence level(s) for a two-sided confidence interval.
#' @param datatable Instead of entering the four individual numbers, you can pass in the two-way table of counts.  Be sure to label the datatable in this case.
#' @keywords two proportion z test
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_twopropztest(6, 17, 12, 20, alternative = "less")
#' iscam_twopropztest(10, 20, 17, 22, conf.level = .95)
#' iscam_twopropztest(10, 20, 17, 22, conf.level = c(.90, .95))

iscam_twopropztest <-
  function(observed1,
           n1,
           observed2,
           n2,
           hypothesized = 0,
           alternative = NULL,
           conf.level = NULL,
           datatable = NULL) {
    # converting data table
    if (!is.null(datatable)) {
      observed1 = datatable[1]
      n1 = datatable[1] + datatable[2]
      observed2 = datatable[3]
      n2 = datatable[3] + datatable[4]
    }


    pvalue = NULL
    zvalue = NULL
    # converting proportion into number of successes
    if (observed1 < 1) {
      observed1 = round(n1 * observed1)
    }
    if (observed2 < 1) {
      observed2 = round(n2 * observed2)
    }
    cat("\n", "Two Proportion z test\n", sep = "", "\n")  # output
    statistic1 = observed1 / n1
    statistic2 = observed2 / n2
    statistic = statistic1 - statistic2
    cat(  # output
      paste(
        "Group1: observed successes = ",
        observed1,
        ", sample size = ",
        n1,
        ", sample proportion = ",
        signif(statistic1, 4),
        "\n\n",
        sep = ""
      )
    )
    cat(  # more output
      paste(
        "Group2: observed successes = ",
        observed2,
        ", sample size = ",
        n2,
        ", sample proportion = ",
        signif(statistic2, 4),
        "\n\n",
        sep = ""
      )
    )
    if (!is.null(alternative)) {
      cat(paste("Null hypothesis       : pi1-pi2 =", hypothesized, sep = " "),
          "\n")
      altname <-
        switch(
          alternative,
          less = "<",
          greater = ">",
          two.sided = "<>",
          not.equal = "<>"
        )
      cat(paste(
        "Alternative hypothesis: pi1-pi2",
        altname,
        hypothesized,
        sep = " "
      ),
      "\n")
      pooledphat <- (observed1 + observed2) / (n1 + n2)
      zvalue <-
        (statistic1 - statistic2 - hypothesized) / sqrt(pooledphat * (1 - pooledphat) *
                                                          (1 / n1 + 1 / n2))
      subtitle1 <- paste("z-statistic:", signif(zvalue, 4), "\n")
      cat("z-statistic:", signif(zvalue, 4), "\n")
      pvalue = 2
      SD <- sqrt(pooledphat * (1 - pooledphat) * (1 / n1 + 1 / n2))
      min <- min(hypothesized - 4 * SD, hypothesized - abs(zvalue) * SD -
                   .001)
      max <- max(hypothesized + 4 * SD, hypothesized + abs(zvalue) * SD +
                   .001)
      x <- seq(min, max, .001)

      # creating second x axis
      my_seq <- -3:3
      xticks <- hypothesized + my_seq * SD
      perc <- c("z=-3", "z=-2", "z=-1", "z=0", "z=1", "z=2", "z=3")
      l <- paste(round(xticks, 2), perc, sep = "\n")

      data <- data.frame(x = x, y = dnorm(x, hypothesized, SD))
      if (alternative == "less") {
        pvalue <- signif(pnorm(zvalue), 4)
        subtitle2 <- paste("p-value:", pvalue)
        plot1 <- ggplot(data, aes_string(x = "x", y = "y")) +
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
        pvalue <- signif(1 - pnorm(zvalue), 4)
        subtitle2 <- paste("p-value:", pvalue)
        plot1 <- ggplot(data, aes_string(x = "x", y = "y")) +
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
      } else if (alternative == "two.sided" ||
                 alternative == "not.equal") {
        pvalue <- signif(2 * pnorm(-1 * abs(zvalue)), 4)
        subtitle2 <- paste("two-sided p-value:", pvalue)
        if (statistic < hypothesized) {
          lowerstat <- statistic
          upperstat <- hypothesized + (hypothesized - statistic)
        } else {
          lowerstat <- hypothesized - (statistic - hypothesized)
          upperstat <- statistic
        }
        plot1 <- ggplot(data, aes_string(x = "x", y = "y")) +
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
    }
    lower = NULL
    upper = NULL

    if (!is.null(conf.level)) {
      if (length(conf.level) > 1)
        par(mar = c(4, 2, 1.5, 4), mfrow = c(length(conf.level), 1))
      for (k in 1:length(conf.level)) {
        if (conf.level[k] > 1)
          conf.level[k] = conf.level[k] / 100
        criticalvalue <- qnorm((1 - conf.level[k]) / 2)
        sephat <-
          sqrt(statistic1 * (1 - statistic1) / n1 + statistic2 * (1 - statistic2) /
                 n2)
        lower[k] <- statistic + criticalvalue * sephat
        upper[k] <- statistic - criticalvalue * sephat
        multconflevel <- 100 * conf.level[k]
        cat(multconflevel,
            "% Confidence interval for pi1-pi2: (",
            lower[k],
            ", ",
            upper[k],
            ") \n")
      }
      if (is.null(alternative)) {
        min <- statistic - 4 * sephat
        max <- statistic + 4 * sephat
        CIseq <- seq(min, max, .001)
        data <-
          data.frame(x = CIseq, y = dnorm(CIseq, lower[1], sephat))
        nicelower <- round(lower, 4)
        niceupper <- round(upper, 4)
        if (length(conf.level) == 1) {
          title2.1 <-
            substitute(paste("Normal (", mean == x1, ", ", SD == x2, ")"),
                       list(x1 = signif(lower[1], 4), x2 = signif(sephat, 4)))
          plot2.1 <- ggplot(data, aes_string(x = "x", y = "y")) +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = lower[1], sd = sephat),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = lower[1], sd = sephat),
              xlim = c(statistic, max),
              geom = "area",
              color = "dodgerblue4",
              fill = "dodgerblue4"
            ) +
            labs(x = "Difference in Sample Proportions",
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
                       list(x1 = signif(upper[1], 4), x2 = signif(sephat, 4)))
          plot2.2 <- ggplot(data, aes_string(x = "x", y = "y")) +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = upper[1], sd = sephat),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = upper[1], sd = sephat),
              xlim = c(min, statistic),
              geom = "area",
              color = "dodgerblue4",
              fill = "dodgerblue4"
            ) +
            labs(x = "Difference in Sample Proportions",
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
            geom_text(x = midpoint,
                      y = 1.2,
                      label = midpoint) +
            geom_text(x = lower + 0.01,
                      y = 1.2,
                      label = nicelower) +
            geom_text(x = upper - 0.01,
                      y = 1.2,
                      label = niceupper) +
            labs(x = "Difference in Process Probabilities") +
            theme(
              axis.title.y = element_blank(),
              # removing y axis
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
            )
          grid.arrange(plot2.1, plot2.2, plot2_3, nrow = 3)
        } else if (length(conf.level) > 1) {
          for (k in 1:length(conf.level)) {
            plot(
              c(min, statistic, max),
              c(1, 1, 1),
              pch = c(".", "^", "."),
              ylab = " ",
              xlab = "difference in process probabilities",
              ylim = c(1, 1)
            )
            abline(v = statistic, col = "gray")
            text(min * .95, 1, labels = paste(100 * conf.level[k], "% CI:"))
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
    if (!is.null(alternative)) {
      mysubtitle <- paste(subtitle1, subtitle2)
      maintitle <-
        paste("Normal (mean = ",
              hypothesized,
              ", SD = ",
              signif(SD, 3),
              ")",
              sep = "")
      finalplot <- plot1 +
        geom_segment(aes(
          x = min,
          xend = max,
          y = 0,
          yend = 0
        ), color = "dodgerblue") +
        labs(x = "Difference in Sample Proportions",
             subtitle = mysubtitle,
             title = maintitle) +
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
      cat("p-value: ", pvalue)
      print(finalplot)
    }
  }
