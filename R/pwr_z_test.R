#' @title Power of a Z-Test for Population Mean
#' @description Computes the Power of a Z-test for the Population Mean
#' @param m0 The value of the population mean from the null hypothesis
#' @param m1 An alternative value of the population mean
#' @param sd Population standard deviation
#' @param n Sample size
#' @param sig.level Significance level, default is 0.05
#' @param alternative type of alternative, with values
#' "two.sided","less" or "greater"
#' @return Power of the test
#' @examples
#' pwr.z.test(m0 = 3, m1 = 2.5, sd = 1, n = 15,
#' sig.level = 0.01, alternative = "two.sided")
#' pwr.z.test(m0 = 3, m1 = 3.5, sd = 1, n = 20,
#' sig.level = 0.10, alternative = "greater")
#' @export
pwr.z.test <- function(m0, m1, sd, n,
                       sig.level = 0.05,
                       alternative = c("two.sided","less","greater")) {
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
