#' iscam_binom Function
#'
#' This function calculates either a tail probability or the inverse cumulative
#' probability depending on whether k or prob is passed a question mark.
#' @param k number of successes
#' @param prob probability of interest
#' @param n number of trials (zero or more)
#' @param pi probability of success on each trial
#' @param lower.tail logical; if TRUE, probabilities are  P[X <= k], otherwise, P[X > k]
#' @keywords binomial
#' @import graphics ggplot2
#' @export
#' @examples
#' iscam_binom("?", 0.05, 20, 0.3, lower.tail = TRUE)
#' iscam_binom(10, "?", 20, 0.3, TRUE)

iscam_binom <- function(k, prob, n, pi, lower.tail) {
  Description = "iscambinomprob(k, prob, n, pi, lower.tail) \n This function calculates either a tail probability or the inverse cumulative probability depending on whether k or prob is passed a question mark."

  if (as.character(prob) == "?")  # Calculating binomial probability
    iscam_binomprob(k, n, pi, lower.tail)
  if (as.character(k) == "?")  # Calculating inverse cumulative probability
    iscam_invbinom(prob, n, pi, lower.tail)

}
