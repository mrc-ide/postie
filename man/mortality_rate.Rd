% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epi.R
\name{mortality_rate}
\alias{mortality_rate}
\title{Add mortality rate}
\usage{
mortality_rate(x, scaler)
}
\arguments{
\item{x}{output}

\item{scaler}{Scaler for severe cases to deaths. The
original fitted scaler for P. falciparum is 0.215. See
\href{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(15)00423-5/fulltext}{Griffin et al (2016)}.}
}
\value{
output with mortality rates
}
\description{
Add mortality rate
}
