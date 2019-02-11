#' iscam_boxplot Function
#'
#' This function displays horizontal boxplot(s). 
#' Optional: A second, categorical variable can also be specified and values will be calculated separately for each group. 
#' @param x a quantitative response variable.
#' @param explanatory a categorical explanatory variable.
#' @param names used to specify the horizontal and vertical axis labels, respectively.
#' @keywords boxplot
#' @import stats graphics ggplot2
#' @export
#' @examples
#' iscam_boxplot(chickwts$weight)
#' iscam_boxplot(chickwts$weight, names = "weight")
#' iscam_boxplot(chickwts$weight, chickwts$feed, names = c("weight", "feed"))


iscam_boxplot <- function(x,
                          explanatory = NULL,
                          names = NULL) {
  df <- data.frame(x)
  if (is.null(explanatory)) {
    ggplot(df, aes(x = "", y = x)) +
      geom_boxplot(  
        outlier.colour = "red",  # editing look of outliers
        outlier.shape = 8,
        outlier.size = 2
      ) +
      coord_flip() +
      # if names are not specified, use the x input as the y label
      labs(y = ifelse(is.null(names), deparse(substitute(x)), names),  
           x = "")
  }
  else{
    # if explanatory grouping variable is specified
    ggplot(df,
           aes(
             x = as.factor(explanatory),
             y = x,
             group = as.factor(explanatory),
             color = as.factor(explanatory)
           )) +
      geom_boxplot(
        outlier.colour = "red",
        outlier.shape = 8,
        outlier.size = 2
      ) +
      coord_flip() +
      labs(
        y = ifelse(is.null(names), deparse(substitute(x)), names[1]),
        x = ifelse(is.null(names), deparse(substitute(explanatory)), names[2]),
        col = ifelse(is.null(names), deparse(substitute(explanatory)), names[2])
      )
  }
}