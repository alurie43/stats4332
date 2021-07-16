#' @title Degrees of freedom for a two-sample t-test
#' @description Computes degrees of freedom for a two-sample t-test
#' @param s1 Standard deviation of the first sample
#' @param s2 Standard deviation of the second sample
#' @param n1 Sample size of the first sample
#' @param n2 Sample size of the second sample
#' @return Degrees of freedom
#' @examples df.t2.test(1.5, 1.2, 30, 20)
#' @export
#the function below computes the degrees of freedom for a
#two-sample t-test
df.t2.test <- function(s1, s2, n1, n2) {
  if (s1 <= 0) stop("First standard deviation must be a positive number.")
  if (s2 <= 0) stop("Second standard deviation must be a positive number")
  if (n1 != round(n1) | n1 <= 0) stop("First sample size must be a positive integer.")
  if (n2 != round(n2) | n2 <= 0) stop("Second sample size must be a positive integer.")
  se1 = s1^2/n1;
  se2 = s2^2/n2;
  n11 = n1 - 1;
  n21 = n2 - 1;
  df = (se1 + se2)^2/(se1^2/n11 + se2^2/n21);
  return(df);
}
