###########################################################################################
###      Filename: rowMedians.R
### Creation Date: Tuesday, 18 August 2015 11:30 AM CDT
### Last Modified: Tuesday, 18 August 2015 11:30 AM CDT
###########################################################################################

#' rowMedians
#'
#' Find the medians of rows; analagous to rowMeans.
#'
#' @param x a matrix or data frame, containing numeric, integer, or logical values.
#' @param na.rm logical. Should missing values be omitted from calculations?
#' @return A vector with length \code{nrow(x)} containing the median of each row of x.
#' @examples
#' myMatrix <- rbind(c(1,2,3,4,5), c(TRUE, FALSE, TRUE, TRUE, FALSE), c(0.3, 0.2, NA, 0.1, 0.1))
#' rownames(myMatrix) <- c("a", "b", "c")
#' rowMedians(myMatrix, na.rm = TRUE)
#' @export
rowMedians <- function (x, na.rm = FALSE)
{
  z <- apply(x, 1, median, na.rm = na.rm)
  names(z) <- rownames(x)
  return(z)
}
