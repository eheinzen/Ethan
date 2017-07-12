###########################################################################################
###      Filename: declare.affection.R
### Creation Date: Tuesday, 31 May 2016 11:45 AM CDT
### Last Modified: Tuesday, 31 May 2016 12:00 PM CDT
###########################################################################################

#' declare.affection
#'
#' Declare affection for \code{name}.
#'
#' @param name A single character string.
#' @param oodle A single logical value; should \code{name} be oodle-ized?
#' @param ... Other arguments to be passed to other methods
#' @return Invisibly returns \code{name}.
#' @examples
#' declare.affection("Alese")
#' declare.affection("Jeremy", oodle = TRUE, replace.y = TRUE)
#' @seealso \code{\link{oodle}}
#' @export
declare.affection <- function(name, oodle = FALSE, ...)
{
  if(missing(name)){stop("'name' argument is missing with no default.")}
  if(!is.character(name) || length(name) != 1){stop("'name' must be a single character string.")}
  if(!is.logical(oodle) || length(oodle) != 1){stop("'oodle' must be a single logical value.")}

  name2 <- if(oodle) oodle(name, ...) else name

  cat(name2, ', you\'re awesome.\n',
      'No, but really, you\'re awesome.\n',
      'You are the coolest person ever.\n',
      name2, ', I love you.\n',
      '\n',
      'Roses are red\n',
      'Violets are blue\n',
      'No one I know\n',
      'Is quite as cool as you.',
      sep = '', file = '')

  invisible(name)
}



