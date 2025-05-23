% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clm.R
\name{S3_clm_clmm}
\alias{S3_clm_clmm}
\alias{coef_.clm}
\alias{coef_.clmm}
\alias{confint.clmm}
\alias{.pval.summary.clm}
\alias{.pval.summary.clmm}
\alias{getCanonicalLink.clm}
\alias{getCanonicalLink.clmm}
\alias{model.matrix.clm}
\alias{desc_.clm}
\alias{desc_.clmm}
\alias{nobsText.clmm}
\alias{terms.clmm}
\title{S3 methods for \link[ordinal]{clm} and \link[ordinal]{clmm} Objects}
\usage{
coef_.clm(x)

coef_.clmm(x)

\method{confint}{clmm}(object, ...)

.pval.summary.clm(x)

.pval.summary.clmm(x)

getCanonicalLink.clm(x)

getCanonicalLink.clmm(x)

\method{model.matrix}{clm}(object, ...)

desc_.clm(x)

desc_.clmm(x)

nobsText.clmm(x)

\method{terms}{clmm}(x, type = c("formula", "scale", "nominal", "all"), ...)
}
\arguments{
\item{x, object}{\link[ordinal]{clm} or \link[ordinal]{clmm} object}

\item{type, ...}{additional parameters for S3 method dispatches}
}
\description{
S3 methods for \link[ordinal]{clm} and \link[ordinal]{clmm} Objects
}
\examples{
library(ordinal)
?ordinal::clm
?ordinal::clmm
m1 = clm(rating ~ temp + contact, data = wine)

m2 = clmm(rating ~ temp + contact + (1|judge), data = wine)
m2 |> coef_.clmm()
m2 |> summary() |> .pval.summary.clm()
m2 |> confint()
m2 |> nobsText.clmm()

m3 = clm(Sat ~ Infl + Type + Cont, weights = Freq, data = MASS::housing)

}
