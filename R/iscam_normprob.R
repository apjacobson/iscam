#' iscam_normprob Function
#'
#' This function calculates tail probability for the normal distribution.
#' @param xval x value
#' @param mean default mean = 0
#' @param sd default standard deviation = 1
#' @param direction a String for finding the probability above "above" or below "below" the inputted value. If "outside" or "between" are specified, a second larger observation needs to be given at the end.
#' @param label It is highly recommended that you indicate a label for the horizontal axis, with the quotation marks e.g., "sample proportions"
#' @param xval2 used if direction is specified as "outside" or "between"
#' @param digits number of tick marks on x axis
#' @keywords normal tail probability
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_normprob(2.2, direction = "above", label = "sample proportions")
#' iscam_normprob(2, 5, 1.5, direction = "outside", label = "sample proportions", xval2 = 3)

iscam_normprob <-
  function(xval,
           mean = 0,
           sd = 1,
           direction,
           label = NULL,
           xval2 = NULL,
           digits = 4) {
    if (is.null(xval2))
      xval2 <- abs(xval)
    # determining which xval is larger
    if (xval2 < xval) {
      temp <- xval
      xval <- xval2
      xval2 <- temp
    }
    xvallabel <- format(xval, digits = digits)
    xval2label <- format(xval2, digits = digits)
    minx <- min(mean - 4 * sd, xval - .15 * abs(xval))
    maxx <- max(mean + 4 * sd, xval2 + .15 * xval2)
    thisx <- seq(minx, maxx, .001)
    xlabel <- "x-variable"
    if (!is.null(label))
      xlabel <- label

    data <- data.frame(x = thisx, y = dnorm(xval, mean, sd))
    myTitle <-
      paste("Normal(mean = ", mean, ", SD = ", sd, ")", sep = "")
    if (direction == "below") {
      normprob <- pnorm(xval, mean, sd, lower.tail = T)
      showprob <- format(normprob, digits = 4)
      mySubtitle <-
        paste("Pr(X \u2264 ", xvallabel, ") = ", showprob, sep = "")
      plot1 <- ggplot(data, aes_string(x = "x", y = "y", width = 0.25)) +
        stat_function(fun = dnorm,
                      #drawing normal density curve
                      args = list(mean = mean, sd = sd),
                      color = "dodgerblue") +
        stat_function(
          fun = dnorm,  # shading in normal curve
          args = list(mean = mean, sd = sd),
          xlim = c(minx, xval),
          geom = "area",
          color = "dodgerblue4",
          fill = "dodgerblue4"
        ) +
        labs(  # graph labels
          x = xlabel,
          y = NULL,
          title = myTitle,
          subtitle = mySubtitle
        ) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#3366FF"))
    } else if (direction == "above") {
      normprob <- pnorm(xval, mean, sd, lower.tail = T)
      showprob <- format(normprob, digits = 4)
      mySubtitle <-
        paste("Pr(X \u2265 ", xvallabel, ") = ", showprob, sep = "")
      plot1 <- ggplot(data, aes_string(x = "x", y = "y", width = 0.25)) +
        stat_function(fun = dnorm,  # drawing normal density curve
                      args = list(mean = mean, sd = sd),
                      color = "dodgerblue") +
        stat_function(
          fun = dnorm,  # shading in normal curve
          args = list(mean = mean, sd = sd),
          xlim = c(xval, maxx),
          geom = "area",
          color = "dodgerblue4",
          fill = "dodgerblue4"
        ) +
        labs(  # graph labels
          x = xlabel,
          y = NULL,
          title = myTitle,
          subtitle = mySubtitle
        ) +
        theme_bw(16, "serif") +  # editing fonts
        theme(plot.subtitle = element_text(color = "#3366FF"))
    } else if (direction == "between") {
      if (is.null(xval2))  # specifying error message if a 2nd xvalue is not inputted
        stop("You need to specify a second observation value.")
      normprob <- pnorm(xval2, mean, sd) - pnorm(xval, mean, sd)
      showprob <- format(normprob, digits = digits)
      mySubtitle <-
        paste("Pr(",
              xvallabel,
              " \u2264 X \u2264 ",
              xval2label,
              ") = ",
              showprob,
              sep = "")
      plot1 <- ggplot(data, aes_string(x = "x", y = "y", width = 0.25)) +
        stat_function(fun = dnorm,  # drawing normal density curve
                      args = list(mean = mean, sd = sd),
                      color = "dodgerblue") +
        stat_function(
          fun = dnorm,  # shading in normal curve
          args = list(mean = mean, sd = sd),
          xlim = c(xval, xval2),
          geom = "area",
          color = "dodgerblue4",
          fill = "dodgerblue4"
        ) +
        labs(  # graph labels
          x = xlabel,
          y = NULL,
          title = myTitle,
          subtitle = mySubtitle
        ) +
        theme_bw(16, "serif") +  # editing fonts
        theme(plot.subtitle = element_text(color = "#3366FF"))
    } else if (direction == "outside") {
      maxx <- max(maxx, xval2)
      thisx <- seq(minx, maxx, .001)
      if (is.null(xval2))
        stop("You need to specify a second observation value.")
      # determining which xval is larger
      if (xval2 < xval) {
        temp <- xval
        xval <- xval2
        xval2 <- temp
      }
      normprob <- 1 - (pnorm(xval2, mean, sd) - pnorm(xval, mean, sd))  # normal probability
      showprob <- format(normprob, digits = digits)
      mySubtitle <-
        paste(
          "Pr(X ",
          "\u2264 ",
          xvallabel,
          ") and Pr(X ",
          "\u2265 ",
          xval2label,
          ") = ",
          showprob,
          sep = ""
        )  # subtitle for plot
      plot1 <- ggplot(data, aes_string(x = "x", y = "y", width = 0.25)) +
        stat_function(fun = dnorm,  # drawing normal density curve
                      args = list(mean = mean, sd = sd),
                      color = "dodgerblue") +
        stat_function(
          fun = dnorm,  # shading in normal curve
          args = list(mean = mean, sd = sd),
          xlim = c(minx, xval),
          geom = "area",
          color = "dodgerblue4",
          fill = "dodgerblue4"
        ) +
        stat_function(
          fun = dnorm,  # shading in normal curve
          args = list(mean = mean, sd = sd),
          xlim = c(xval2, maxx),
          geom = "area",
          color = "dodgerblue4",
          fill = "dodgerblue4"
        ) +
        labs(
          x = xlabel,
          y = NULL,
          title = myTitle,
          subtitle = mySubtitle
        ) +
        theme_bw(16, "serif") +
        theme(plot.subtitle = element_text(color = "#3366FF"))
    } else
      stop("Use \"above\", \"below\", \"between\", or \"outside\" as the direction.")
    finalplot <-
      plot1 + geom_segment(aes(
        x = minx,
        xend = maxx,
        y = 0,
        yend = 0
      ), color = "dodgerblue")
    print(finalplot)
    cat(paste("Probability:", showprob))
  }
