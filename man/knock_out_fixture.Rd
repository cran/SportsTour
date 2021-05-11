\name{knock_out_fixture}
\alias{knock_out_fixture}
\title{Displays fixture for the tournament when number of teams is not exactly power of two using the "Bye" method}
\usage{
knock_out_fixture(number_of_teams)
}
\arguments{
  \item{number_of_teams}{input numeric}
     }
\value{
    \item{text}{Displays fixture for the tournament along with other necessary details such as number of matches, number of rounds, number of byes etc.}
  }
\description{
Displays fixture for the tournament when number of teams is  not exactly power of two using the "Bye" method
}
\examples{
number_of_teams=4
knock_out_fixture(number_of_teams)
}
