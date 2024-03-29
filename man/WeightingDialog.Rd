% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weightingdialog.R
\name{WeightingDialog}
\alias{WeightingDialog}
\title{\code{WeightsDialog}}
\usage{
WeightingDialog(
  categorical.variables = NULL,
  categorical.targets = NULL,
  numeric.variables = NULL,
  numeric.targets = NULL,
  lower = "",
  upper = "",
  calfun = c("Raking", "Linear", "Logit")[1],
  force.to.n = TRUE,
  input.weight = NULL
)
}
\arguments{
\item{categorical.variables}{An optional list or data frame of categorical adjustment variables.}

\item{categorical.targets}{The target probabilities for each category listed in \code{categorical.variables}}

\item{numeric.variables}{An optional list or data frame of categorical adjustment variables.}

\item{numeric.targets}{the target mean for each numeric variable in numeric.variables}

\item{lower}{A lower bound weight value (not guaranteed to be achieved).}

\item{upper}{An upper bound weight value (not guaranteed to be achieved).}

\item{calfun}{The calibration function: \code{"Raking"} (Default), \code{"Linear"}, or \code{"Logit"}.}

\item{force.to.n}{Force the sum of weights to equal the sample size.}

\item{input.weight}{An optional weight variable; if supplied, the created weight is created to be as close
to this input.weight as possible}
}
\value{
numeric A vector of weights
}
\description{
Code for the Displayr Weighting dialog box.
}
