% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equal_surplus_division.R
\name{equal_surplus_division}
\alias{equal_surplus_division}
\title{Equal Surplus Division value}
\usage{
equal_surplus_division(characteristic_func, n_players = 0)
}
\arguments{
\item{characteristic_func}{The valued function defined on the subsets of the number
of players}

\item{n_players}{Only used if \code{characteristic_func} is a \code{function}.
The number of players in the game.}
}
\value{
The equal surplus division value for each player
}
\description{
Calculate the equal surplus division value
}
\examples{
n <- 10
v <- function(coalition) {
  if (length(coalition) > n/2) {
    return(1)
  } else {
    return(0)
  }
}
equal_surplus_division(v,n)

}
