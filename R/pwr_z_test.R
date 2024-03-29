#' @title Power of a Z-Test for Population Mean
#' @description Computes the Power of a Z-test for the Population Mean
#' @param m0 The value of the population mean from the null hypothesis
#' @param m1 An alternative value of the population mean
#' @param sd Population standard deviation
#' @param n Sample size
#' @param sig.level Significance level, default is 0.05
#' @param alternative type of alternative, with values
#' "two.sided","less" or "greater", default is "two.sided"
#' @return Power of the test
#' @examples
#' pwr.z.test(m0 = 3, m1 = 2.5, sd = 1, n = 15,
#' sig.level = 0.01, alternative = "two.sided")
#' pwr.z.test(m0 = 3, m1 = 3.5, sd = 1, n = 20,
#' sig.level = 0.10, alternative = "greater")
#' @export
pwr.z.test <- function(m0, m1, sd, n,
                       sig.level = 0.05,
                       alternative = "two.sided") {
  if (is.numeric(m0) == FALSE) stop("The mean m0 from the null should be a number.")
  if (is.numeric(m1) == FALSE) stop("The mean m1, the alternative value of the mean, should be a number.")
  if (is.numeric(sd) == FALSE) {
    stop("Standard deviation should be positive real number")
  } else if (sd <= 0) {
    stop("Standard deviation should be a positive number.")
  }
  if (round(n) != n) {
    stop("Sample size should be a positive integer.")
  }
  if (is.numeric(sig.level) == FALSE) {
    stop("Significance level should be a decimal between 0 and 1")
  } else if (sig.level <= 0 | sig.level >= 1) {
    stop("Significance level should be a decimal between 0 and 1")
  }
  if (alternative != "two.sided" & alternative != "less" &
      alternative != "greater") {
    stop("Alternative can only be \"two.sided\", \"less\"
         or \"greater\".")
  }
  a <- ifelse(alternative == "two.sided", sig.level/2, sig.level);

  #finding z-alpha or z-alpha/2
  z_crit <- qnorm(1 - a);
  z1 <- z_crit + (m0 - m1)/(sd/sqrt(n))
  z2 <- -z_crit + (m0 - m1)/(sd/sqrt(n))
  p1 <- pnorm(z1)
  p2 <- pnorm(z2)
  beta <- ifelse(alternative == "two.sided", p1-p2,
                 ifelse(alternative == "less", 1-p2, p1))
  1 - beta
}
