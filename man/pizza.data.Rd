% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pizza.data.R
\docType{data}
\name{pizza.data}
\alias{pizza.data}
\title{Pizza and Survival}
\format{A data frame with 150 rows and 7 variables:
\describe{
  \item{id}{Study ID.}
  \item{likes_pizza}{Does this person like pizza?}
  \item{time_to_fu}{Time to last follow-up (months).}
  \item{status_fu}{Status at follow-up: 0 = Alive, 1 = Dead, as the Survival package expects.}
  \item{fav_flavor}{Favorite flavor of pizza.}
  \item{sex}{Sex.}
  \item{height}{Height (inches).}
}}
\source{
Ethan made up this data. Turns out people who like pizza live longer. Who'da thunk?
}
\usage{
pizza.data
}
\description{
A dataset containing containing time to follow-up, status at follow-up, whether someone likes pizza, their favorite flavor of pizza, their
 sex, their height, and their id.
}
\keyword{datasets}
