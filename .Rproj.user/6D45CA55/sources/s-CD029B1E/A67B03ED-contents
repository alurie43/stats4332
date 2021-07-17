#' @title Normality Check Utility Function
#' @description Does Checks for Normality Using Graphical
#'     Methods and Shapiro-Wilk test
#' @param x A numeric vector
#' @return Normal Probability Plot, Boxplot and Shapiro-
#'     Wilk output
#' @examples normality_check(mtcars$wt)
#' @importFrom car qqPlot
#' @export
normality_check <- function(x) {
  if (is.numeric(x) == FALSE) stop("This function works on numeric vectors only.")
  #this command will print two graphs in one row
  par(mfrow = c(1, 2))
  s <- deparse(substitute(x))
  #performing normal probability plot
  qqPlot(x,
         main = paste("Normal Probability\nPlot of",s),
         ylab = s, col="blue")
  #checking boxplot for potential outliers
  boxplot(x,ylab = s,
          col=rainbow(10),main=paste("Boxplot of\n",s))
  #performing Shapiro-Wilk Test for Normality
  print(shapiro.test(x))
  #go back to printing one graph
  par(mfrow= c(1, 1))
}

