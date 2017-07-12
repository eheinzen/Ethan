###########################################################################################
###      Filename: oodle.R
### Creation Date: Tuesday, 31 May 2016 11:30 AM CDT
### Last Modified: Tuesday, 31 May 2016 11:45 AM CDT
###########################################################################################

#' oodle
#'
#' Replace all vowels (aeiou) in a name with 'oodle'.
#'
#' @param name A single character string.
#' @param replace.y A single logical value; should y's be replaced?
#' @param ... Other arguments
#' @return A copy of \code{name} with all vowels replaced with 'oodle'.
#' @examples
#' oodle("Brendan Broderick")
#' oodle("Peter Martin")
#' oodle("Jeremy Syrjanen", TRUE)
#' @export
oodle <- function(name, replace.y = FALSE, ...)
{
  if(missing(name)){stop("'name' argument is missing with no default.")}
  if(!is.character(name) || length(name) != 1){stop("'name' must be a single character string.")}
  if(!is.logical(replace.y) || length(replace.y) != 1){stop("'replace.y' must be a single logical value.")}

  # see ?toupper
  cap <- function(x)
  {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse=" ")
  }
  replace <- if(replace.y) '[aeiouy]' else '[aeiou]'
  return(cap( gsub(replace, 'oodle', name, ignore.case = TRUE)))
}



