###########################################################################################
###      Filename: ggcolor.R
### Creation Date: Creation Date: Thursday, 03 December 2015 02:30 PM CST
### Last Modified: Thursday, 11 February 2016 1:00 PM CST (moved it from packages/Ethan/R/)
###########################################################################################

#' ggcolors
#'
#' Get the colors that ggplot2 uses by default. This code is not totally written by
#'  the author; he adapted it from someplace on the web.
#'
#' @param n The number of colors you want (equally spaced along the color wheel)
#' @return A vector of colors is returned.
#' @details
#' I made this into a function because I was tired of copying it into all my code.
#' @examples
#' ggcolors(3)
#' ggcolors(6) # overlaps the ggcolors(3) call
#' @export
ggcolors <- function(n)
{
  if(missing(n)){stop("'n' is missing with no default.")}
  if (!kinda.integer(n)){stop("'n' must be coercible to integer.")}
  n <- as.integer(n)
  return(hcl(seq(15, 375, length = n + 1), l = 65, c = 100)[1:n])
}
