###########################################################################################
###      Filename: kinda.type.R
### Creation Date: Thursday, 19 November 2015 03:30 PM CST
### Last Modified: Thursday, 19 November 2015 03:30 PM CST
###########################################################################################

#' Kinda functions
#'
#' kinda.<type>(x) returns whether 'x' can be coerced without error or warning
#'   to <type> using as.<type>(x).
#'
#' @details
#' Be careful using these! Make sure they're doing exactly what you expect, as some functions are pretty lenient in their coercion.
#'
#' @param x An R object.
#' @param FUN A function of the form as.<type> (e.g. as.logical, as.matrix) that tries to coerce x to <type>.
#' @name kinda
NULL
#> NULL

#' @rdname kinda
#' @export
kinda.type <- function(x, FUN)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  if(missing(FUN)){stop("Argument 'FUN' is missing.")}
  return(tryCatch({trash <- FUN(x); TRUE}, warning = function(w) {FALSE}, error = function(e) {FALSE}))
}

#' @rdname kinda
#' @export
kinda.logical <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.logical))
}

#' @rdname kinda
#' @export
kinda.integer <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.integer))
}

#' @rdname kinda
#' @export
kinda.numeric <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.numeric))
}

#' @rdname kinda
#' @export
kinda.character <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.character))
}

#' @rdname kinda
#' @export
kinda.vector <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.vector))
}

#' @rdname kinda
#' @export
kinda.factor <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.factor))
}

#' @rdname kinda
#' @export
kinda.matrix <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.matrix))
}

#' @rdname kinda
#' @export
kinda.array <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.array))
}

#' @rdname kinda
#' @export
kinda.data.frame <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.data.frame))
}

#' @rdname kinda
#' @export
kinda.formula <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.formula))
}

#' @rdname kinda
#' @export
kinda.list <- function(x)
{
  if(missing(x)){stop("Argument 'x' is missing.")}
  return(kinda.type(x, as.list))
}
