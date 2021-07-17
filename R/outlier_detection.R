#' @title Helper for outlier detection
#' @description Computes 5-number summary by using
#'    exclusive method of quartile calculation.
#'    Also outputs interquartile range and lower
#'    and upper fences to detect outliers.
#'    Prints outliers, if there are any.
#' @param x A numeric vector
#' @import stats
#' @return 5-number summary, iqr, lower fence and upper fence
#' @examples outlierdetection(mtcars$wt)
#' @export
outlierdetection <- function(x) {
  if (is.numeric(x) == FALSE) stop("This function works on numeric vectors only.")
  sorted_x <- sort(x);
  n <- length(x);
  n1 <- n %/% 2;
  if (n %% 2 == 0) {
    n2 = n1 + 1;
  } else {
    n2 = n1 + 2;
  }
  min <- min(x);
  max <- max(x);
  q1 <- median(sorted_x[1:n1]);
  q2 <- median(x);
  q3 <- median(sorted_x[n2:n]);
  iqr <- q3 - q1;
  lf <- q1 - 1.5*iqr;
  uf <- q3 + 1.5*iqr;
  outliers <- x[x < lf | x > uf];
  outl <- toString(outliers);
  if (identical(outl,"") == FALSE) {
    print("The outliers are: ");
    print(outl);
  } else {
    print("There are no outliers.")
  }
  data.frame(min, q1, q2, q3, max, iqr, lf, uf)
}
