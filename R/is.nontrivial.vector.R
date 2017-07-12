###########################################################################################
###      Filename: is.nontrivial.vector.R
### Creation Date: Tuesday, 28 July 2015 5:00 PM CDT
### Last Modified: Monday, 16 November 2015 10:30 AM CST
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
  if(length(x) > 1){return(TRUE)} else{return(FALSE)}
}
