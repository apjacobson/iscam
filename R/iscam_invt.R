#' iscam_invt Function
#'
#' This function calculates the t quantile of a specified probability.
#' @param prob1 desired probability
#' @param df degrees of freedom
#' @param direction Specify whether you want this area to be "above", "below", or "outside" or "between"
#' @keywords inverse t probability
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_invt(.35, 2, direction = "above")
#' iscam_invt(.4, 3, direction = "below")
#' iscam_invt(.45, 4, direction = "between")
#' iscam_invt(.55, 5, direction = "outside")

iscam_invt <- function(prob1, df, direction) {
  mydf <- data.frame(x = c(-4, 4))  # Data frame with x from -4 to 4
  upper <- qt(prob1, df, lower.tail = F)  # when direction == above
  lower <- qt(prob1, df)  # when direction == below
  upside <- -signif(qt((1 - prob1) / 2, df, lower.tail = TRUE), 4)  # when direction == between
  downside <- -upside  # when direction == between
  outupper <- -signif(qt(prob1 / 2, df, lower.tail = TRUE), 4)  # when direction == outside
  outlower <- -outupper  # # when direction == outside
  mytitle <- paste("t distribution with", df, "degrees of freedom")  # graph's main title
  
  if (direction == "above") {
    answer <- signif(upper, 4)  # t quantile
    mysubtitle <- paste("Pr(T > ", answer, ") = ", prob1, sep = "")
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dt,  # creating t density curve
                    args = list(df = df),
                    color = "dodgerblue") +
      geom_segment(aes(  # horizontal line segment for bottom of curve
        x = -4,
        xend = 4,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "t values",  # graph labels
           y = "density",
           title = mytitle) +
      theme_bw(12, "serif") +
      theme(  # remove y axis
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      stat_function(  # shade in t density curve
        fun = dt,
        args = list(df = df),
        xlim = c(upper, 4),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      ) +
      annotate(  # annotating graph
        geom = "text",
        x = 3,
        y = 0.1,
        label = mysubtitle,
        color = "dodgerblue4",
        fontface = 2
      )
    cat("The observation with",  # output
        prob1,
        "probability above is",
        answer,
        "\n")
  }
  else if (direction == "below") {
    answer <- signif(lower, 4)  # t quantile
    mysubtitle <- paste("Pr(T < ", answer, ") = ", prob1, sep = "")  # for annotation purposes
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dt,  # t density curve
                    args = list(df = df),
                    color = "dodgerblue") +
      geom_segment(aes(  # horizontal line segment for bottom of curve
        x = -4,
        xend = 4,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "t values",
           y = "density",
           title = mytitle) +
      theme_bw(12, "serif") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      stat_function(
        fun = dt,
        args = list(df = df),
        xlim = c(-4, lower),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      ) +
      annotate(
        geom = "text",
        x = -3,
        y = 0.1,
        label = mysubtitle,
        color = "dodgerblue4",
        fontface = 2
      )
    cat("the observation with",
        prob1,
        "probability below is",
        answer,
        "\n")
  }
  else if (direction == "between") {
    mysubtitle <-
      paste("Pr(", downside, "< T < ", upside, ") = ", prob1, sep = "")
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dt,
                    args = list(df = df),
                    color = "dodgerblue") +
      geom_segment(aes(
        x = -4,
        xend = 4,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "t values",
           y = "density",
           title = mytitle) +
      theme_bw(12, "serif") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      stat_function(
        fun = dt,
        args = list(df = df),
        xlim = c(downside, upside),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      ) +
      annotate(
        geom = "text",
        x = -2.5,
        y = 0.3,
        label = mysubtitle,
        color = "dodgerblue4",
        fontface = 2
      )
    cat("There is",
        prob1,
        "probability between",
        downside,
        "and",
        upside,
        "\n")
  }
  else if (direction == "outside") {
    subtitle1 <- paste("Pr(T > ", outupper, ") = ", prob1 / 2, sep = "")
    subtitle2 <-
      paste("Pr(T < ", outlower, ") = ", prob1 / 2, sep = "")
    p <- ggplot(mydf, aes(x = x)) +
      stat_function(fun = dt,
                    args = list(df = df),
                    color = "dodgerblue") +
      geom_segment(aes(
        x = -4,
        xend = 4,
        y = 0,
        yend = 0
      ), color = "dodgerblue") +
      labs(x = "t values",
           y = "density",
           title = mytitle) +
      theme_bw(12, "serif") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      stat_function(
        fun = dt,
        args = list(df = df),
        xlim = c(-4, outlower),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      ) +
      stat_function(
        fun = dt,
        #Is there an easier way to combine these?
        args = list(df = df),
        xlim = c(outupper, 4),
        geom = "area",
        color = "dodgerblue4",
        fill = "dodgerblue4"
      ) +
      annotate(
        geom = "text",
        x = 3,
        y = 0.09,
        label = subtitle1,
        color = "dodgerblue4",
        fontface = 2
      ) +
      annotate(
        geom = "text",
        x = -3,
        y = 0.09,
        label = subtitle2,
        color = "dodgerblue4",
        fontface = 2
      )
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
