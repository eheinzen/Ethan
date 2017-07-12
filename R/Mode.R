###########################################################################################
###      Filename: Mode.R
### Creation Date: Wednesday, 27 April 2016 11:00 AM CDT
### Last Modified: Wednesday, 27 April 2016 11:45 AM CDT
###########################################################################################

#' Mode
#'
#' Calculate the mode of a vector.
#'
#' @param x A vector.
#' @param na.rm A single logical value; should NAs be removed before proceeding?
#' @return The mode of x. If there are several modes, it returns the smallest one.
#' @details
#' This was adapted from the web.
#' @examples
#' Mode(c(1,1,2,2)) # will find the smallest
#' Mode(c("1", "1", "2", "3"))
#' Mode(c(TRUE, TRUE, FALSE, FALSE, FALSE, NA))
#' Mode(factor(c("M", "F", "M")))
#' @export
Mode <- function(x, na.rm = FALSE)
{
  if(!is.logical(na.rm) || length(na.rm) != 1L){stop("'na.rm' must be a single logical value.")}
  if(na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


