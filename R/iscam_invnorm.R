#' iscam_invnorm Function
#'
#' This function calculates the normal quantile of a specified probability. 
#' @param prob1 desired probability
#' @param mean default = 0
#' @param sd default = 1
#' @param direction Specify whether you want this area to be "above", "below", or "outside" or "between"
#' @keywords inverse normal probability
#' @export
#' @examples
#' iscam_invnorm(prob1 = 0.25, direction = "above")
#' iscam_invnorm(.55, 15, 2, direction = "below")
#' iscam_invnorm(.40, direction = "outside")
#' iscam_invnorm(0.5, direction = "between")

iscam_invnorm <- function(prob1,
                          mean = 0,
                          sd = 1,
                          direction) {
  min <- mean - 4 * sd  # min for x axis
  max <- mean + 4 * sd  # max for x axis
  mydf <- data.frame(x = c(min, max))  # Data frame from min to max
  upper <- qnorm(prob1, mean, sd, lower.tail = F)  # when direction == above
  lower <- qnorm(prob1, mean, sd)  # when direction == below
  upside <- signif(qnorm((1 - prob1) / 2, mean, sd, lower.tail = F), 4)  # when direction == between
  downside <- signif(qnorm((1 - prob1) / 2, mean, sd, lower.tail = T), 4)  # when direction == between
  outupper <- signif(qnorm(prob1 / 2, mean, sd, lower.tail = F), 4)  # when direction == outside
  outlower <- signif(qnorm(prob1 / 2, mean, sd, lower.tail = T), 4)  # when direction == outside
  mytitle <-
    paste("Normal (mean = ", mean, ", SD = ", sd, ")", sep = "")  # graph main title
  
  
  if (direction == "above") {
    answer <- signif(upper, 4)  # normal quantile
    mysubtitle <- paste("Pr(X >", answer, ") =", prob1)
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm,  # normal density curve
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(  # horizontal segment for the bottom of density curve
        x = min,
        xend = max,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "X = variable",  # graph labels
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") +
      theme(  # removing y axis
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(color = "#3366FF")
      ) +
      stat_function(  # shading the upper tail
        fun = dnorm,
        args = list(mean = mean, sd = sd),
        xlim = c(upper, max),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      )
    cat("The observation with",  # output
        prob1,
        "probability above is",
        answer,
        "\n")
  }
  else if (direction == "below") {
    answer <- signif(lower, 4)  # normal quantile
    mysubtitle <- paste("Pr(X < ", answer, ") = ", prob1, sep = "")
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm,  # normal density curve
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(  # horizontal segment for bottom of the curve
        x = min,
        xend = max,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "X = variable",  # graph labels
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") +  # editing font
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(color = "#3366FF")
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = mean, sd = sd),
        xlim = c(min, lower),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4")
      cat("the observation with",
          prob1,
          "probability below is",
          answer,
          "\n")
  }
  else if (direction == "between") {
    mysubtitle <-
      paste("Pr(", downside, " < X < ", upside, ") = ", prob1, sep = "")
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(
        x = min,
        xend = max,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "X = variable",
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(color = "#3366FF")
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = mean, sd = sd),
        xlim = c(downside, upside),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      ) +
      #annotate(geom="text", x=-2.5, y=0.3, label=mysubtitle,
      #        color="dodgerblue4", fontface =2)
      cat("There is",
          prob1,
          "probability between",
          downside,
          "and",
          upside,
          "\n")
  }
  else if (direction == "outside") {
    subtitle1 <-
      paste("Pr(X > ", outupper, ") = ", prob1 / 2, "\n", sep = "")
    subtitle2 <- paste("Pr(X < ", outlower, ") = ", prob1 / 2, sep = "")
    mysubtitle <- paste(subtitle1, subtitle2)
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dnorm,
                    args = list(mean = mean, sd = sd),
                    color = "dodgerblue") +
      geom_segment(aes(
        x = min,
        xend = max,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "X = variable",
           title = mytitle,
           subtitle = mysubtitle) +
      theme_bw(12, "serif") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(color = "#3366FF")
      ) +
      stat_function(
        fun = dnorm,
        args = list(mean = mean, sd = sd),
        xlim = c(min, outlower),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      ) +
      stat_function(
        fun = dnorm,
        #Is there an easier way to combine these?
        args = list(mean = mean, sd = sd),
        xlim = c(outupper, max),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4")
      cat("There is",
          prob1,
          "probability outside",
          outlower,
          "and",
          outupper,
          "\n")
  }
  print(p)
}
