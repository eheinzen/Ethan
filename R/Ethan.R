###########################################################################################
###      Filename: Ethan.R
### Creation Date: Wednesday, 05 August 2015 11:30 AM CDT
### Last Modified: Thursday, 16 June 2016 10:00 AM CDT
###########################################################################################

#' Ethan's Functions
#'
#' A collection of functions that Ethan has found helpful for his work in R.
#'
#' @section Functions:
#'
#' \code{\link{coxtable}}: Make a pretty cox model table.
#'
#' \code{\link{auc}}: Calculate the area under the curve for longitudinal data, just like \%auc.sas.
#'
#' \code{\link{is.nontrivial.vector}}: Determine if an object is a vector of length > 1.
#'
#' \code{\link{rowMedians}}: Find the medians of rows; analagous to rowMeans.
#'
#' \code{\link{rowSDs}}: Find the standard deviation of rows; analagous to rowMeans.
#'
#' \code{\link{multiplot}}: Plot more than one ggplot object in a grid fashion.
#'
#' \code{\link{marginal.boxplot}}: Plot boxplots on the margins of a scatterplot in ggplot2.
#'
#' \code{\link{kinda.type}}: Returns whether 'x' can be coerced without error or warning to <type> using as.<type>(x).
#'
#' \code{\link{ggcolors}}: Get the colors that ggplot2 uses by default.
#'
#' \code{\link{Mode}}: Calculate the mode of a vector.
#'
#' \code{\link{oodle}}: Replace all vowels (aeiou) in a name with 'oodle'.
#'
#' \code{\link{declare.affection}}: Declare affection.
#'
#' @section Data:
#' \code{\link{pizza.data}}: A data set containing time to follow-up, status at follow-up, whether someone likes pizza, their favorite flavor of pizza, their
#'  sex, their height, and their id.
#'
#' @examples
#' library(Ethan)
#'
#' @docType package
#' @name Ethan
#'
NULL

## devtools::check_man();
## devtools::check()
## devtools::install(dependencies = FALSE)
## < restart R >
## library(Ethan)
##
