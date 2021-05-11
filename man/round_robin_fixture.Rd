\name{round_robin_fixture}
\alias{round_robin_fixture}
\title{Displays fixture for the tournament for a given number of team using "staircase" or "cyclic" method }
\usage{
round_robin_fixture(number_of_teams,method)
}
\arguments{
  \item{number_of_teams}{numeric}
  \item{method}{method for setting fixture "staircase" for staircase method and "cyclic" for cyclic method}
     }
\value{
    \item{text}{Displays fixture for the tournament}
  }
\description{
Displays fixture for the tournament for a given number of team using "staircase" or "cyclic" method
}
\examples{
number_of_teams=4
round_robin_fixture(number_of_teams,method="staircase")
}
