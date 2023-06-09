% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantile_selection.R
\name{quantile_selection}
\alias{quantile_selection}
\title{Quantile selection}
\usage{
quantile_selection(
  train = Data0[sel_a, ],
  cv = Data0$Date[sel_b],
  quantile = seq(0.3, 0.5, 0.05)
)
}
\arguments{
\item{train}{the dataset on which the model is fitted}

\item{cv}{the cross-validation set}

\item{quantile}{a vector of quantile levels to compare}
}
\value{
the different forecasts obtained with the quantile levels
}
\description{
Fit multiple smooth additive quantile regression models in order to determine the best quantile level
}
\author{
halvard.bariller@universite-paris-saclay.fr>
}
