% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pwr_p_test.R
\name{pwr.p.test}
\alias{pwr.p.test}
\title{Power of a Z-Test for Population Proportion}
\usage{
pwr.p.test(p0, p1, n, sig.level = 0.05, alternative = "two.sided")
}
\arguments{
\item{p0}{The value of the population proportion
(in decimals) from the null hypothesis}

\item{p1}{An alternative value of the population proportion}

\item{n}{Sample size}

\item{sig.level}{Significance level, default is 0.05}

\item{alternative}{type of alternative, with values
"two.sided","less" or "greater", default is "two.sided"}
}
\value{
Power of the test
}
\description{
Computes the Power of a Z-test for the Population Proportion
}
\examples{
pwr.p.test(p0 = 0.10, p1 = 0.15, n = 100,
sig.level = 0.01, alternative = "two.sided")
pwr.p.test(p0 = 0.2, p1 = 0.25, n = 50,
sig.level = 0.10, alternative = "greater")
}
