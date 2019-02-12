#' iscam_normpower Function
#'
#' This function determines the rejection region corresponding to the level of significance and the first probability and shows the second distribution shading its corresponding region.
#' @param LOS level of significance
#' @param n sample size
#' @param prob1 probability of success
#' @param alternative can be "less", "greater", or "two.sided"
#' @param prob2 second probability of success
#' @param explain defaults to FALSE. If TRUE, shows a shaded graph with the Type 2 error.
#' @keywords normal power
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_normpower(.05, 20, .5, "less", .7)

iscam_normpower <-
  function(LOS,
           n,
           prob1,
           alternative,
           prob2 = NULL,
           explain = FALSE) {

    # Calculating x limits for graph
    minx <-
      max(0, min(prob1 - 4 * sqrt(prob1 * (1 - prob1) / n), prob2 - 4 * sqrt(prob2 *
                                                                               (1 - prob2) / n)))
    maxx <-
      min(n, max(prob1 + 4 * sqrt(prob1 * (1 - prob1) / n), prob2 + 4 * sqrt(prob2 *
                                                                               (1 - prob2) / n)))
    mean <- prob1  # normal mean
    std <- round(sqrt(prob1 * (1 - prob1) / n), digits = 4)  # standard deviation
    thisx <- 0:n  # x axis
    maintitle <-
      substitute(paste("Normal (", mu == x1, ", ", sigma == x2, ")"),
                 list(x1 = mean, x2 = std))

    if (alternative == "less") {
      rr <- qnorm(LOS, mean, std)  # finding rejection region
      this.prob1 <- pnorm(rr, mean, std)  # p-value
      showprob1 <- format(this.prob1, digits = 4)  # formatting p-value
      rr <- format(rr, digits = 4)  # formatting rejection region
      subtitle <-
        paste("P(p-hat \u2264 ", rr, ") = ", showprob1, sep = "")  # creating subtitle
      df <- data.frame(x = thisx, y = dnorm(thisx, mean, std))
      plot <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15))
      if (!isTRUE(explain)) {  # when explain is set to false
        plot1 <- plot +
          stat_function(
            fun = dnorm,  # drawing normal density curve
            args = list(mean = mean, sd = std),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,  #shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(minx, rr),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          ) +
          guides(fill = FALSE)  # removing default legends
      } else if (isTRUE(explain)) {
        plot1 <- plot +
          stat_function(
            fun = dnorm,  #drawing normal density curve
            args = list(mean = mean, sd = std),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,  #shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(minx, rr),
            geom = "area",
            color = "dodgerblue4",
            aes(fill = "red"),
            alpha = .7
          ) +
          scale_fill_manual(values = "red",  # editing legend manually
                            name = "",
                            labels = "Type I Error")
      }
      cat(paste("Probability ", rr, " and below = ", showprob1, sep = ""))
    } else if (alternative == "greater") {
      rr <- round(qnorm(LOS, mean, std, FALSE), digits = 4)  # rejection region
      this.prob1 <- 1 - pnorm(rr, mean, std)  # normal probability
      showprob1 <- round(this.prob1, digits = 4)  # formatting probability
      subtitle <-
        paste("P(p-hat \u2265 ", rr, ") = ", showprob1, sep = "")
      df <- data.frame(x = thisx, y = dnorm(thisx, mean, std))
      plot <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15))
      if (!isTRUE(explain)) {
        plot1 <- plot +
          stat_function(
            fun = dnorm,  # drawing normal density curve
            args = list(mean = mean, sd = std),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,  # shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(rr, maxx),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          ) +
          guides(fill = FALSE)  # hiding default legend
      } else if (isTRUE(explain)) {
        plot1 <- plot +
          stat_function(
            fun = dnorm,  # drawing normal density curve
            args = list(mean = mean, sd = std),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,  # shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(rr, maxx),
            geom = "area",
            color = "dodgerblue4",
            aes(fill = "red"),
            alpha = .7
          ) +
          scale_fill_manual(values = "red",
                            name = "",
                            labels = "Type I Error")
      }
      cat(paste("Probability ", rr, " and above = ", showprob1, sep = ""))
    }  else if (alternative == "two.sided") {
      lowerrr <- round(qnorm(LOS / 2, mean, std), 4)  # lower tail rejection region
      upperrr <- round(qnorm(LOS / 2, mean, std, FALSE), 4)  # upper tail rejection region
      lowerprob1 <- pnorm(lowerrr, mean, std)
      upperprob1 <- pnorm(upperrr, mean, std, FALSE)
      showlowerprob1 <-
        format(lowerprob1, digits = 4)
      showupperprob1 = format(upperprob1, digits = 4)
      showprob1 <- format(lowerprob1 + upperprob1, digits = 4)

      subtitle <-
        paste("P(p-hat \u2264 ",
              lowerrr,
              ") + P(p-hat \u2265 ",
              upperrr,
              ") = ",
              showprob1,
              sep = "")  # for subtitle
      df <- data.frame(x = thisx, y = dnorm(thisx, mean, std))
      plot <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15))
      if (!isTRUE(explain)) {
        plot1 <- plot +
          stat_function(
            fun = dnorm,
            #drawing normal density curve
            args = list(mean = mean, sd = std),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,
            #shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(minx, lowerrr),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          ) +
          stat_function(
            fun = dnorm,
            #shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(upperrr, maxx),
            geom = "area",
            color = "dodgerblue4",
            fill = "dodgerblue4"
          ) +
          guides(fill = FALSE)
      } else if (isTRUE(explain)) {
        plot1 <- plot +
          stat_function(
            fun = dnorm,
            #drawing normal density curve
            args = list(mean = mean, sd = std),
            color = "dodgerblue"
          ) +
          stat_function(
            fun = dnorm,
            #shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(minx, lowerrr),
            geom = "area",
            color = "dodgerblue4",
            aes(fill = "red"),
            alpha = .7
          ) +
          stat_function(
            fun = dnorm,
            #shading in normal curve
            args = list(mean = mean, sd = std),
            xlim = c(upperrr, maxx),
            geom = "area",
            color = "dodgerblue4",
            aes(fill = "red"),
            alpha = .7
          ) +
          scale_fill_manual(values = "red",
                            name = "",
                            labels = "Type I Error")
      }
      cat(paste("Probability in rejection region = ", showprob1, sep = ""))
    }
    else
      stop("Check input for alternative")
    if (!is.null(prob2)) {
      mean2 <- prob2
      std2 <- round(sqrt(prob2 * (1 - prob2) / n), 4)
      maintitle2 <-
        substitute(paste("Normal (", mu == x1, ", ", sigma == x2, ")",),
                   list(x1 = mean2, x2 = std2))
      if (alternative == "less") {
        rr <- round(qnorm(LOS, mean, std), 4)
        this.prob2 <- pnorm(rr, mean2, std2)
        showprob2 <- round(this.prob2, digits = 4)
        subtitle2 <-
          paste("P(p-hat \u2264 ", rr, ") = ", showprob2, sep = "")
        df <- data.frame(x = thisx, y = dnorm(thisx, mean2, std2))
        plot <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15))
        if (!isTRUE(explain)) {
          plot2 <- plot +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = mean2, sd = std2),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(minx, rr),
              geom = "area",
              color = "dodgerblue4",
              fill = "#007f80"
            ) +
            guides(fill = FALSE)
        } else if (isTRUE(explain)) {
          plot2 <- plot +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = mean2, sd = std2),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(minx, rr),
              geom = "area",
              color = "dodgerblue4",
              aes(fill = "limegreen"),
              alpha = .7
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(rr, maxx),
              geom = "area",
              color = "dodgerblue4",
              aes(fill = "navy"),
              alpha = .7
            ) +
            scale_fill_manual(
              values = c("limegreen", "navy"),
              name = "",
              labels = c("Power", "Type II Error")
            )
        }
        cat("\n Probability", rr, "and below =", this.prob2)
      }
      else if (alternative == "greater") {
        rr <- round(qnorm(LOS, mean, std, FALSE), 4)
        this.prob2 <- 1 - pnorm(rr, mean2, std2)
        showprob2 <- format(this.prob2, digits = 4)
        subtitle2 <-
          paste("P(p-hat \u2265 ", rr, ") = ", showprob2, sep = "")
        df <- data.frame(x = thisx, y = dnorm(thisx, mean2, std2))
        plot <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15))
        cat("\n", "Probability", rr, "and above =", showprob2)
        if (!isTRUE(explain)) {
          plot2 <- plot +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = mean2, sd = std2),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(rr, maxx),
              geom = "area",
              color = "dodgerblue4",
              fill = "#007f80"
            ) +
            guides(fill = FALSE)
        } else if (isTRUE(explain)) {
          plot2 <- plot +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = mean2, sd = std2),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(rr, maxx),
              geom = "area",
              color = "dodgerblue4",
              aes(fill = "limegreen"),
              alpha = .7
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(minx, rr),
              geom = "area",
              color = "dodgerblue4",
              aes(fill = "navy"),
              alpha = .7
            ) +
            scale_fill_manual(
              values = c("limegreen", "navy"),
              name = "",
              labels = c("Power", "Type II Error")
            )
        }
        cat("\n Probability", rr, "and above =", this.prob2)
      }
      else if (alternative == "two.sided") {
        lowerrr <- qnorm(LOS / 2, mean, std)
        upperrr <- qnorm(LOS / 2, mean, std, FALSE)
        lowerprob2 <- pnorm(lowerrr, mean2, std2)
        upperprob2 <- pnorm(upperrr, mean2, std2, FALSE)
        showlowerprob2 <-
          round(lowerprob2, digits = 4)
        showupperprob2 = round(upperprob2, digits = 4)
        showprob2 <- round(lowerprob2 + upperprob2, digits = 4)
        showupperrr <-
          round(upperrr, digits = 4)
        showlowerrr = round(lowerrr, digits = 4)
        subtitle2 <-
          paste(
            "P(p-hat \u2264",
            showlowerrr,
            ")+P(p-hat \u2265",
            showupperrr,
            ") =",
            showlowerprob2,
            "+",
            showupperprob2,
            "\n =",
            showprob2
          )
        df <- data.frame(x = thisx, y = dnorm(thisx, mean2, std2))
        plot <- ggplot(df, aes_string(x = "x", y = "y", width = 0.15))
        if (!isTRUE(explain)) {
          plot2 <- plot +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = mean2, sd = std2),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(minx, lowerrr),
              geom = "area",
              color = "dodgerblue4",
              fill = "#007f80"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(upperrr, maxx),
              geom = "area",
              color = "dodgerblue4",
              fill = "#007f80"
            ) +
            guides(fill = FALSE)
        } else if (isTRUE(explain)) {
          plot2 <- plot +
            stat_function(
              fun = dnorm,
              #drawing normal density curve
              args = list(mean = mean2, sd = std2),
              color = "dodgerblue"
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(minx, lowerrr),
              geom = "area",
              color = "dodgerblue4",
              aes(fill = "limegreen"),
              alpha = .7
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(upperrr, maxx),
              geom = "area",
              color = "dodgerblue4",
              aes(fill = "limegreen"),
              alpha = .7
            ) +
            stat_function(
              fun = dnorm,
              #shading in normal curve
              args = list(mean = mean2, sd = std2),
              xlim = c(lowerrr, upperrr),
              geom = "area",
              color = "dodgerblue4",
              aes(fill = "navy"),
              alpha = .7
            ) +
            scale_fill_manual(
              values = c("limegreen", "navy"),
              name = "",
              labels = c("Power", "Type II Error")
            )
        }
        cat("\n Probability in rejection region = ", showprob2, "\n")
      } else
        stop("Check input for alternative")
      plot1 <- plot1 + xlim(minx, maxx) +
        labs(x = "Probability of Success",
             title = maintitle,
             subtitle = subtitle) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#007f80")) +
        geom_segment(aes(
          x = minx,
          xend = maxx,
          y = 0,
          yend = 0
        ), color = "dodgerblue")
      plot2 <- plot2 + xlim(minx, maxx) +
        labs(x = "Probability of Success",
             title = maintitle2,
             subtitle = subtitle2) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#007f80")) +
        geom_segment(aes(
          x = minx,
          xend = maxx,
          y = 0,
          yend = 0
        ), color = "dodgerblue")
      grid.arrange(plot1, plot2, nrow = 2)
    }
    if (is.null(prob2)) {
      plot1 <- plot1 + xlim(minx, maxx) +
        labs(x = "Probability of Success",
             title = maintitle,
             subtitle = subtitle) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#007f80")) +
        geom_segment(aes(
          x = minx,
          xend = maxx,
          y = 0,
          yend = 0
        ), color = "dodgerblue")
      print(plot1)
    }
  }
