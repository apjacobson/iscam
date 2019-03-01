#' iscam_twosamplet Function
#'
#' This function calculates a two sample t-test and/or interval from summary data.
#' @param x1 observed mean
#' @param sd1 observed standard deviation
#' @param n1 sample size
#' @param x2 observed mean
#' @param sd2 observed standard deviation
#' @param n2 sample size
#' @param hypothesized hypothesized difference in population means (default = 0)
#' @param alternative form of alternative ("less", "greater", or "two.sided")
#' @param conf.level a confidence level for a two-sided confidence interval
#' @keywords two sample t test
#' @export
#' @import graphics ggplot2
#' @examples
#' iscam_twosamplet(97.25, 3.65, 8, 87.25, 9.60, 12, alternative = "less")
#' iscam_twosamplet(97.25, 3.65, 8, 87.25, 9.60, 12, conf.level = .95)

iscam_twosamplet <-
  function(x1,
           sd1,
           n1,
           x2,
           sd2,
           n2,
           hypothesized = 0,
           alternative = NULL,
           conf.level = 0) {
    y <- NULL
    cat("\n", "Two Sample t test\n", sep = "", "\n")  # output
    statistic1 = x1
    statistic2 = x2
    statistic = statistic1 - statistic2
    df = signif((sd1 * sd1 / n1 + sd2 * sd2 / n2) * (sd1 * sd1 / n1 + sd2 *
                                                       sd2 / n2) / ((sd1 * sd1 / n1) ** 2 / (n1 - 1) + (sd2 ** 2 / n2) ** 2 / (n2 -
                                                                                                                                 1)),
                4)
    unpooledsd = sqrt(sd1 * sd1 / n1 + sd2 * sd2 / n2)

    cat(paste("Group1: mean = ", x1, ", sd = ", sd1, ",  sample size = ", n1, "\n", sep =
                ""))  # output
    cat(paste("Group2: mean = ", x2, ", sd = ", sd2, ",  sample size = ", n2, "\n", sep =
                ""))  # output
    cat(paste("diff: ", x1 - x2, "\n\n", sep = ""))  # output
    maintitle <- paste("t (df = ", df, ")", sep = "")  # title of plot

    if (!is.null(alternative)) {  # when alternative is specified
      cat(paste("Null hypothesis       : mu1-mu2 =", hypothesized, sep = " "),
          "\n")  # output
      altname = switch(
        alternative,
        less = "<",
        greater = ">",
        two.sided = "<>",
        not.equal = "<>"
      )
      cat(paste(
        "Alternative hypothesis: mu1-mu2",
        altname,
        hypothesized,
        sep = " "
      ),
      "\n")

      tvalue = (statistic1 - statistic2 - hypothesized) / unpooledsd
      subtitle1 <- paste("t-statistic:", signif(tvalue, 4), "\n")
      cat("t-statistic:", signif(tvalue, 4), "\n")
      cat("df:", signif(df, 4), "\n")
      min = min(-4, tvalue - .001)
      diffmin = min(
        hypothesized - 4 * unpooledsd,
        min(
          hypothesized - 4 * unpooledsd,
          hypothesized - abs(hypothesized - statistic) - .001
        )
      )
      max = max(4, tvalue + .001)
      diffmax = max(hypothesized + 4 * unpooledsd,
                    hypothesized + abs(hypothesized - statistic) + .001)

      x = seq(min, max, .001)
      diffx = x * unpooledsd + hypothesized
      # creating second x axis
      my_seq <- -3:3
      xticks <- hypothesized + my_seq * unpooledsd
      perc <-
        c("t = -3", "t = -2", "t = -1", "t = 0", "t = 1", "t = 2", "t = 3")
      l <- paste(round(xticks, 2), perc, sep = "\n")

      data <- data.frame(x = diffx, y = dt(x, df))
      plot <-
        ggplot(data, aes_string(x = "x", y = "y")) + geom_line(color = "dodgerblue")  # creating density curve

      if (alternative == "less") {
        pvalue <- signif(pt(tvalue, df), 4)  # t p-value
        subtitle2 <- paste("p-value: ", pvalue)
        plot1 <- plot +
          geom_ribbon(  # shading in density curve
            data = subset(data, x < statistic),
            aes(ymin = 0, ymax = y),
            fill = "dodgerblue4",
            alpha = "0.5"
          )
        subtitle2 <- paste("p-value:", round(pvalue, 4))
      } else if (alternative == "greater") {
        pvalue <- signif(1 - pt(tvalue, df), 4)
        subtitle2 <- paste("p-value: ", pvalue)
        plot1 <- plot +
          geom_ribbon(  # shading in density curve
            data = subset(data, x > statistic),
            aes(ymin = 0, ymax = y),
            fill = "dodgerblue4",
            alpha = "0.5"
          )
        subtitle2 <- paste("p-value:", round(pvalue, 4))
      } else if (alternative == "two.sided" ||
                 alternative == "not.equal") {
        pvalue <- signif(2 * pt(-1 * abs(tvalue), df), 4)

        subtitle2 <- paste("two-sided p-value: ", pvalue)
        plot1 <- plot +
          geom_ribbon(
            data = subset(data, x < hypothesized - abs(hypothesized - statistic)),
            aes(ymin = 0, ymax = y),
            fill = "dodgerblue4",
            alpha = "0.5"
          ) +
          geom_ribbon(
            data = subset(data, x > hypothesized + abs(hypothesized - statistic)),
            aes(ymin = 0, ymax = y),
            fill = "dodgerblue4",
            alpha = "0.5"
          )
        subtitle2 <- paste("two-sided p-value:", round(pvalue, 4))
      }
      finalplot <-
        plot1 + geom_segment(aes(
          x = diffmin,
          xend = diffmax,
          y = 0,
          yend = 0
        ), color = "dodgerblue") +
        labs(
          title = maintitle,
          subtitle = paste(subtitle1, subtitle2),
          x = "Sample Means"
        ) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "dodgerblue")) +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        ) +
        scale_x_continuous(  # setting 2nd x axis
          breaks = xticks,
          labels = l,
          limits = c(diffmin, diffmax)
        )
      print(finalplot)
      cat("p-value: ", pvalue, "\n")
    }
    lower = NULL
    upper = NULL


    if (conf.level != 0) {
      if (conf.level > 1)
        conf.level = conf.level / 100
      criticalvalue = qt((1 - conf.level) / 2, df)
      lower = statistic + criticalvalue * unpooledsd
      upper = statistic - criticalvalue * unpooledsd
      multconflevel = 100 * conf.level
      cat(multconflevel,
          "% Confidence interval for mu1-mu2: (",
          lower,
          ", ",
          upper,
          ") \n")
      if (is.null(alternative)) {
        min = statistic - 4 * unpooledsd
        max = statistic + 4 * unpooledsd
        CIseq = seq(min, max, .001)
        par(mar = c(4, .5, 1.5, .5), mfrow = c(3, 1))
        myxlab = substitute(paste(mean == x1), list(x1 = signif(lower, 4)))
        plot(CIseq,
             dnorm(CIseq, lower, unpooledsd),
             type = "l",
             xlab = " ")
        mtext(
          "difference in sample means",
          side = 1,
          line = 1.75,
          adj = .5,
          cex = .75
        )
        topseq = seq(statistic, max, .001)
        polygon(c(statistic, topseq,  max), c(0, dnorm(topseq, lower, unpooledsd), 0), col =
                  "red")
        myxlab = substitute(paste("difference in population mean", s == x1),
                            list(x1 = signif(lower, 4)))
        title(myxlab)
        plot(
          seq(min, max, .001),
          dnorm(seq(min, max, .001), upper, unpooledsd),
          type = "l",
          xlab = " "
        )
        mtext(
          "difference in sample means",
          side = 1,
          line = 1.75,
          adj = .5,
          cex = .75
        )
        bottomseq = seq(min, statistic, .001)
        polygon(c(min, bottomseq, statistic, statistic),
                c(
                  0,
                  dnorm(bottomseq, upper, unpooledsd),
                  dnorm(statistic, upper, unpooledsd),
                  0
                ),
                col = "red")
        newtitle = substitute(paste("difference in population mean", s == x1),
                              list(x1 = signif(upper, 4)))
        title(newtitle)
        #newtitle=substitute(paste("t (", df==x1, ")", ), list(x1=signif(upper,4), x2=signif(sephat, 4)));   title(newtitle)

        plot(
          c(min, statistic, max),
          c(1, 1, 1),
          pch = c(".", "^", "."),
          ylab = " ",
          xlab = "difference in process means",
          ylim = c(1, 1)
        )
        abline(v = statistic, col = "gray")
        text(min * 1.01, 1, labels = paste(multconflevel, "% CI:"))
        text(statistic, .9, labels = signif(statistic, 4))
        text(lower, 1, labels = signif(lower, 4), pos = 3)
        text(upper, 1, labels = signif(upper, 4), pos = 3)
        points(c(lower, upper), c(1, 1), pch = c("[", "]"))
        lines(c(lower, upper), c(1, 1))
      }
    }
    if (!is.null(alternative)) {
      invisible(list (
        "tvalue" = tvalue,
        "df" = df,
        "pvalue" = pvalue,
        "lower" = lower,
        "upper" = upper
      ))
    }
  }
