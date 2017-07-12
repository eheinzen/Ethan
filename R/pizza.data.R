###########################################################################################
###      Filename: pizza.data.R
### Creation Date: Thursday, 19 November 2015 03:15 PM CST
### Last Modified: Thursday, 19 November 2015 03:15 PM CST
###########################################################################################

#' Pizza and Survival
#'
#' A dataset containing containing time to follow-up, status at follow-up, whether someone likes pizza, their favorite flavor of pizza, their
#'  sex, their height, and their id.
#'
#' @format A data frame with 150 rows and 7 variables:
#' \describe{
#'   \item{id}{Study ID.}
#'   \item{likes_pizza}{Does this person like pizza?}
#'   \item{time_to_fu}{Time to last follow-up (months).}
#'   \item{status_fu}{Status at follow-up: 0 = Alive, 1 = Dead, as the Survival package expects.}
#'   \item{fav_flavor}{Favorite flavor of pizza.}
#'   \item{sex}{Sex.}
#'   \item{height}{Height (inches).}
#' }
#' @source Ethan made up this data. Turns out people who like pizza live longer. Who'da thunk?
"pizza.data"
