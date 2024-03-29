#' @title Sample size of a Z-Test for Population Mean
#' @description Computes the Power of a Z-test for the Population Mean
#' @param m0 The value of the population mean from the null hypothesis
#' @param m1 An alternative value of the population mean
#' @param sd Population standard deviation
#' @param power Power of the test
#' @param sig.level Significance level, default is 0.05
#' @param alternative type of alternative, with values
#' "two.sided","less" or "greater"
#' @return Sample size required to achieve the given power
#' @examples
#' ssize.z.test(m0 = 3.25, m1 = 3.5, sd = 1, power = 0.9,
#' sig.level = 0.01, alternative = "two.sided")
#' ssize.z.test(m0 = 3.25, m1 = 3, sd = 1, power = 0.92,
#' sig.level = 0.10, alternative = "less")
#' @export
ssize.z.test <- function(m0, m1, sd, power = 0.9, sig.level = 0.05, alternative = "two.sided") {
  if (is.numeric(m0) == FALSE) stop("The mean m0 from the null should be a number.")
  if (is.numeric(m1) == FALSE) stop("The mean m1, the alternative value of the mean, should be a number.")
  if (is.numeric(sd) == FALSE) {
    stop("Standard deviation should be positive real number")
  } else if (sd <= 0) {
    stop("Standard deviation should be a positive number.")
  }
  if (is.numeric(power) == FALSE) {
    stop("Power should be a number between 0 and 1.")
  } else if (power <= 0 | power >= 1) {
    stop("Power should be a number between 0 and 1.")
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
    tside = 2;
  } else {
    tside = 1;
  }

  if (tside == 2) {
    a <- sig.level/2;
    b <- 1 - power;
    za <- qnorm(a, mean = 0, sd = 1, lower.tail = FALSE);
    zb <- qnorm(b, mean = 0, sd = 1, lower.tail = FALSE);
    n1 <- ceiling((sd *(za + zb)/(m0 - m1))^2);
  }
  if (tside != 2) {
    a <- sig.level;
    b <- 1 - power;
    za <- qnorm(a, mean = 0, sd = 1, lower.tail = FALSE);
    zb <- qnorm(b, mean = 0, sd = 1, lower.tail = FALSE);
    n1 <- ceiling((sd *(za + zb)/(m0 - m1))^2);
  }
  return(n1);
}
