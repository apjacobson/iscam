#' iscam_dotplot Function
#'
#' This function displays horizontal dotplot(s). 
#' Optional: A second, categorical variable can also be specified and values will be calculated separately for each group. 
#' @param response a quantitative response variable.
#' @param explanatory a categorical explanatory variable.
#' @param names used to specify the horizontal and vertical axis labels, respectively.
#' @keywords dotplot
#' @export
#' @import stats graphics ggplot2
#' @examples
#' iscam_dotplot(chickwts$weight)
#' iscam_dotplot(chickwts$weight, names = "weight")
#' iscam_dotplot(chickwts$weight, chickwts$feed, names = c("weight", "feed"))

iscam_dotplot <-
  function(response,
           explanatory = NULL,
           names = NULL) {
    df <- data.frame(response)  # putting data into a data frame
    if (is.null(explanatory)) {
      ggplot(df, aes(x = response)) +
        geom_dotplot(binwidth = 1) +
        ylim(0, 2) +
        labs(x = ifelse(is.null(names), deparse(substitute(response)), names))
    }
    else{
      # if explanatory grouping variable is specified
      ggplot(df, aes(x = factor(explanatory), y = response)) +
        geom_dotplot(binaxis = 'y', binwidth = 1) +
        coord_flip() +  # flip boxplot
        labs(
          y = ifelse(is.null(names), deparse(substitute(x)), names[1]),
          x = ifelse(is.null(names), deparse(substitute(explanatory)), names[2])
        )
    }
  }