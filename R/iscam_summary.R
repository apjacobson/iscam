#' iscam_summary Function
#'
#' This function calculates the five number summary, mean, and standard deviation of the quantitative variable x.
#' @param x quantitative variable
#' @param explanatory optional, categorical grouping variable
#' @param digits number of tick marks on x axis
#' @keywords summary
#' @export
#' @import stats graphics ggplot2 skimr
#' @examples
#' iscam_summary(chickwts$weight, chickwts$feed, digits = 4)



iscam_summary <- function(x,
                          explanatory = NULL,
                          digits = 3) {
  # setting number of digits to be displayed
  skim_format(numeric = list(digits = digits))
  # selecting what is to be shown in the output
  skim_with(numeric = list(
    p50 = NULL,
    p100 = NULL,
    p0 = NULL,
    median = median,
    min = min,
    max = max
  ))
  if (is.null(explanatory)) {
    skim(x)
  } else if (!is.null(explanatory)) {  # When explanatory grouping variable is specified
    data <-
      data.frame(x = x, explanatory = explanatory)  # deparse substitute to get variable name
    data %>%
      dplyr::group_by(explanatory) %>%
      skim()
  }
}
