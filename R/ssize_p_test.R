#' @title Sample Size of a Z-Test for Population Proportion
#' @description Computes the Sample Size of a Z-test for the
#' Population Proportion
#' @param p0 The value of the population proportion from the
#' null hypothesis (in decimal form)
#' @param p1 An alternative value of the population proportion
#' @param power Power of the test
#' @param sig.level Significance level, default is 0.05
#' @param alternative type of alternative, with values
#' "two.sided","less" or "greater"
#' @return Sample size required to achieve the given power
#' @examples
#' ssize.p.test(p0 = 0.10, p1 = 0.08, power = 0.9,
#' sig.level = 0.01, alternative = "two.sided")
#' ssize.p.test(p0 = 0.30, p1 = 0.25, power = 0.92,
#' sig.level = 0.10, alternative = "less")
#' @export
ssize.p.test <- function(p0, p1, power = 0.9, sig.level = 0.05, alternative = "two.sided") {
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
  if (is.numeric(power) == FALSE) {
    stop("Power of the test should be a decimal between 0 and 1")
  } else if (power <= 0 | power >= 1) {
    stop("Power of the test should be a decimal between 0 and 1")
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
  if (alternative == "two.sided") {
    a <- sig.level/2;
    s1 <- sqrt(p1*(1-p1));
    s0 <- sqrt(p0*(1-p0));
    b <- 1 - power;
    za <- qnorm(a, mean = 0, sd = 1, lower.tail = FALSE);
    zb <- qnorm(b, mean = 0, sd = 1, lower.tail = FALSE);
    n1 <- ceiling(((za*s0 + zb*s1)/(p1-p0))^2);
  } else {
    a <- sig.level;
    s1 <- sqrt(p1*(1-p1));
    s0 <- sqrt(p0*(1-p0));
    b <- 1 - power;
    za <- qnorm(a, mean = 0, sd = 1, lower.tail = FALSE);
    zb <- qnorm(b, mean = 0, sd = 1, lower.tail = FALSE);
    n1 <- ceiling(((za*s0 + zb*s1)/(p1-p0))^2);
  }
  return(n1);
}
