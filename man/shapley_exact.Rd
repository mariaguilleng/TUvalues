% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapley_exact.R
\name{shapley_exact}
\alias{shapley_exact}
\title{Shapley value (exact)}
\usage{
shapley_exact(characteristic_func, n_players)
}
\arguments{
\item{characteristic_func}{The valued function defined on the subsets of the number
of players}

\item{n_players}{The number of players}
}
\value{
The Shapley value for each player
}
\description{
Calculate the exact Shapley value
}
