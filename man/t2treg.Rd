% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t2treg.R
\name{t2treg}
\alias{t2treg}
\title{Mapping original time points to their registered position}
\usage{
t2treg(argvals, hinv)
}
\arguments{
\item{argvals}{A vector of time points}

\item{hinv}{A \code{fda::fd} object containing only one curve}
}
\value{
A vector of the same length of \code{argvals}
}
\description{
An auxiliary thin wrapper to \code{fda::eval.fd()}.
Given an already computed inverse time warping function \code{hinv},
map original time points \code{argvals} to their registered position \code{hinv}(\code{argvals}).
Values outside the range of \code{hinv} are mapped to \code{NA}s.
}
