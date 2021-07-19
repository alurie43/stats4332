#' @title Power of a Z-Test for Population Proportion
#' @description Computes the Power of a Z-test for the Population Proportion
#' @param p0 The value of the population proportion
#' (in decimals) from the null hypothesis
#' @param p1 An alternative value of the population proportion
#' @param n Sample size
#' @param sig.level Significance level, default is 0.05
#' @param alternative type of alternative, with values
#' "two.sided","less" or "greater", default is "two.sided"
#' @return Power of the test
#' @examples
#' pwr.p.test(p0 = 0.10, p1 = 0.15, n = 100,
#' sig.level = 0.01, alternative = "two.sided")
#' pwr.p.test(p0 = 0.2, p1 = 0.25, n = 50,
#' sig.level = 0.10, alternative = "greater")
#' @export
pwr.p.test <- function(p0, p1, n, sig.level = 0.05, alternative = "two.sided") {
  if (is.numeric(p0) == FALSE) {
    stop("Proportion from the null should be a decimal between 0 and 1")
  } else if (p0 <= 0 | p0 >= 1) {
    stop("Proportion from the null should be a decimal between 0 and 1")
  }
  if (is.numeric(p1) == FALSE) {
    stop("Proportion from the alternative should be a decimal between 0 and 1")
  } else if (p1 <= 0 | p1 >= 1) {
    stop("Proportion from the alternative should be a decimal between 0 and 1")
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
  if (alternative == "greater") {
    a <- sig.level;
    za <- qnorm(a, mean = 0, sd = 1, lower.tail = FALSE);
    se1 <- sqrt(p1*(1-p1)/n);
    se0 <- sqrt(p0*(1-p0)/n);
    z1 <- (p0 - p1 + za*se0)/se1;
    beta <- pnorm(z1, mean = 0, sd = 1);
    pwr <- 1 - beta;
  }
  if (alternative == "less") {
    a <- sig.level;
    za <- qnorm(a, mean = 0, sd = 1, lower.tail = FALSE);
    se1 <- sqrt(p1*(1-p1)/n);
    se0 <- sqrt(p0*(1-p0)/n);
    z1 <- (p0 - p1 - za*se0)/se1;
    beta <- 1 - pnorm(z1, mean = 0, sd = 1);
    pwr <- 1 - beta;
  }
  if (alternative == "two.sided") {
    a <- sig.level/2;
    za <- qnorm(a, mean = 0, sd = 1, lower.tail = FALSE);
    se1 <- sqrt(p1*(1-p1)/n);
    se0 <- sqrt(p0*(1-p0)/n);
    z1 <- (p0 - p1 - za*se0)/se1;
    z2 <- (p0 - p1 + za*se0)/se1;
    beta <- pnorm(z2, mean = 0, sd = 1) - pnorm(z1, mean = 0, sd = 1);
    pwr <- 1 - beta;
  }
  return(pwr);
}
