###########################################################################################
###      Filename: is.nontrivial.vector.R
### Creation Date: Tuesday, 28 July 2015 5:00 PM CDT
### Last Modified: Wednesday, 12 July 2017 10:00 AM CDT
###########################################################################################

#' is.nontrivial.vector
#'
#' Determine if an object is a vector of length > 1.
#'
#' @param x An R object.
#' @return TRUE or FALSE
#' @export
#' @examples
#' ## TRUE
#' is.vector("Hello")
#' ## FALSE
#' is.nontrivial.vector("Hello")
is.nontrivial.vector <- function(x)
{
  if(missing(x)){stop("Parameter x is missing, with no default.")}
  if(!is.vector(x)){return(FALSE)}
  # otherwise it's a vector!
  return(length(x) > 1)
}
