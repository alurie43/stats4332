% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.R
\name{df.t2.test}
\alias{df.t2.test}
\title{Degrees of freedom for a two-sample t-test}
\usage{
df.t2.test(s1, s2, n1, n2)
}
\arguments{
\item{s1}{Standard deviation of the first sample}

\item{s2}{Standard deviation of the second sample}

\item{n1}{Sample size of the first sample}

\item{n2}{Sample size of the second sample}
}
\value{
Degrees of freedom
}
\description{
Computes degrees of freedom for a two-sample t-test
}
\examples{
df.t2.test(1.5, 1.2, 30, 20)
}
