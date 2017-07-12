###########################################################################################
###      Filename: auc.R
### Creation Date: Tuesday, 28 July 2015 5:00 PM CDT
### Last Modified: Wednesday, 12 July 2017 10:00 AM CDT
###########################################################################################

#' auc
#'
#' Calculate the area under the curve for longitudinal data.
#'
#' @param y A vector of y-values of any length, or else a character string denoting which
#'   column in data the y-values should be taken from.
#' @param t A vector of t- (x-) values of the same length as y, or else a character string
#'   denoting which column in data the t-values should be taken from. If t is missing,
#'   the y-values are assumed to come in regular intervals (i.e. 1:length(y)).
#' @param data An optional data.frame from which y and t come.
#' @param baseadj Logical, denoting whether the AUC should be centered at the baseline value
#'   (baseadj = TRUE) or not (baseadj = FALSE) (DEFAULT).
#' @param missadj A flag denoting how to deal with missing values at the end of y.
#'   \itemize{
#'     \item{'NA': Set the AUC to NA (DEFAULT)}
#'     \item{'zero': Set the AUC to 0. Can also be referred to as '0'}
#'     \item{'none': No adjustment; compute what AUC is available}
#'     \item{'last.known': Replace missing values with the last known value}
#'     \item{'lowest': Replace missing values with the lowest known value. Can also be referred to as 'smallest'}
#'     \item{'highest': Replace missing values with the highest known value. Can also be referred to as 'biggest'}
#'     \item{'first': Replace missing values with the baseline (first) value. Can also be referred to as 'baseline'}
#'     \item{'average': Replace missing values with the average of the known values. Can also be referred to as 'mean'}
#'  }
#' @param onlybase A flag denoting how to deal with cases where there is only one
#'   non-missing value. onlybase = 'zero' (or '0') will set the AUC to 0, while
#'   onlybase = 'NA' will set it to NA (DEFAULT).
#' @param nobase A flag denoting how to deal with a missing baseline value.
#'   \itemize{
#'     \item{'NA': Set the AUC to NA (DEFAULT)}
#'     \item{'zero': Set the AUC to 0. Can also be referred to as '0'}
#'     \item{'none': No adjustment; compute what AUC is available (the first known value will be considered
#'       as the baseline, but no longer at t[1].)}
#'   }
#' @param rescale (Optional) A number denoting how to scale the AUC; if not missing,
#'   the average AUC per time unit will be calculated and multiplied by this value.
#'   (DEFAULT NULL means the AUC will not be scaled)
#' @return
#' \code{auc} returns the area under the curve.
#'
#' \code{auc.args} returns a list giving the five values. Used mostly as default arguments to other functions.
#' @details
#' \code{auc} won't work if you specify the data parameter but also specify vectors for y or t.
#'   It also won't work if there are any missing (non-finite) t-values. Any non-finite y-values are ignored.
#'
#' \code{auc.args} creates a list giving the five "adjustment" variables in \code{auc}.
#'   The defaults are NOT the same as the defaults in \code{auc}, however.
#'   No error checking is done here! I'm counting on \code{auc} to catch all of those.
#' @examples
#' auc(c(1,0,2,1,1,NA), missadj = 'none')
#' auc(c(1,0,2,1,1,NA), missadj = 'last.known')
#' auc(c(1,0,2,1,1,NA), missadj = 'lowest')
#' auc(c(1,0,2,1,1,NA), missadj = 'highest') # and so on
#'
#' data <- data.frame(time = c(0,2,3,8), fold.change = c(1, 2, 3, 3))
#' auc("fold.change", "time", baseadj = TRUE, data = data)
#' @name auc
NULL
#> NULL

#' @rdname auc
#' @export
auc.args <- function(baseadj = TRUE, missadj = 'last.known', onlybase = 'NA', nobase = 'NA', rescale = NULL)
{
  return(list(baseadj = baseadj, missadj = missadj, onlybase = onlybase, nobase = nobase, rescale = rescale))
}

#' @rdname auc
#' @export
auc <- function(y, t, data, baseadj = FALSE,
                missadj = c('NA', 'zero', '0', 'none', 'last.known', 'lowest', 'smallest', 'highest', 'biggest', 'first', 'baseline', 'average', 'mean'),
                onlybase = c('NA', 'zero', '0'), nobase = c('NA', 'zero', '0', 'none'), rescale = NULL)
{
  ########## Check to make sure stuff was entered right ##########

  if(missing(y)){stop("Parameter y is missing with no default.")}
  if(missing(data)) ## y had better be a numeric vector
  {
    if(!(is.vector(y) && is.numeric(y))){stop("Parameter y is not a numeric vector, and data is missing.")}
    if(missing(t))
    {
      t <- 1:length(y)
    } else if(!(is.vector(t) && is.numeric(t))){stop("Parameter t is not a numeric vector, and data is missing.")}
  } else
  {
    if(!is.data.frame(data)){stop("Parameter data is not a data.frame.")}
    if(!is.character(y) || length(y) != 1L){stop("Parameter y is not a character, but data is not missing.")}
    if(y %nin% colnames(data)){stop("Parameter y is a character, but not in the colnames of data.")}
    y <- data[[y]]
    if(!is.numeric(y)){stop("The column in data corresponding to y is not numeric.")}

    if(missing(t))
    {
      t <- 1:length(y)
    } else
    {
      if(!is.character(t) || length(t) != 1L){"Parameter t is not a character, but data is not missing."}
      if(t %nin% colnames(data)){stop("Parameter t is a character, but not in the colnames of data.")}
      t <- data[[t]]
      if(!is.numeric(t)){stop("The column in data corresponding to t is not numeric.")}
    }
  }

  if(any(!is.finite(t))){stop("There are non-finite values in t!")}

  #### double check we did everything right ####
  if(!is.vector(y) || !is.vector(t) || any(!is.finite(t))){stop("Something went wrong. Contact the person who maintains this function.")}

  # we'll consider NaN and Inf and -Inf here
  if(!any(is.finite(y))){warning("No finite values in y. Returning NA."); return(NA_real_)}
  if(length(y) != length(t)){stop("Lengths of y and t are not equal.")}
  validate.logical(baseadj)
  missadj <- match.arg(missadj, several.ok = FALSE)
  onlybase <- match.arg(onlybase, several.ok = FALSE)
  nobase <- match.arg(nobase, several.ok = FALSE)
  if(!is.null(rescale) && !(is.numeric(rescale) && length(rescale) != 1)){stop("Parameter rescale needs to be a numeric constant.")}

  #### sort if we need to ####
  y <- y[order(t)] # this shouldn't fail, because we already checked no t-values are missing
  t <- t[order(t)]


  ########## Now calculate the AUC ##########
  #### If the baseline is missing ####
  if(!is.finite(y[1]))
  {
    if(nobase %in% c('zero', '0')){return(0)}
    if(nobase == 'NA'){return(NA_real_)}
  }
  #### If there is only one non-missing value ####
  if(sum(is.finite(y)) == 1)
  {
    if(onlybase %in% c('zero', '0')){return(0)}
    if(onlybase == 'NA'){return(NA_real_)}
  }
  #### If the last (few) entry(ies) of y are missing ####
  n <- length(y)
  i_firstfinite <- min(which(is.finite(y)))
  i_lastfinite <- max(which(is.finite(y)))

  if(!is.finite(y[n]))
  {
    if(missadj %in% c('zero', '0')){return(0)}
    if(missadj == 'NA'){return(NA)}
    i_n <- (i_lastfinite + 1):n
    if(missadj == 'last.known'){y[i_n] <- y[i_lastfinite]}
    if(missadj %in% c('lowest', 'smallest')){y[i_n] <- min(y[is.finite(y)])} # We don't want -Inf to be the smallest value
    if(missadj %in% c('highest', 'biggest')){y[i_n] <- max(y[is.finite(y)])} # We don't want Inf to be the biggest value
    if(missadj %in% c('first', 'baseline')){y[i_n] <- y[i_firstfinite]}
    if(missadj %in% c('average', 'mean')){y[i_n] <- mean(y[is.finite(y)])}
  }
  #### baseadj ####
  y <- y - baseadj*y[i_firstfinite] # indicator variable for the win!

  #### Now that we've imputed any values we might want we're ready to go ####
  y_compl <- y[is.finite(y)]
  t_compl <- t[is.finite(y)]
  n_compl <- length(y_compl)

  #print(data.frame(y_compl, t_compl))

  #### Okay but now we're ACTUALLY calculating the AUC ####
  auc <- 0
  for (i in 1:(n_compl-1))
  {
    auc <- auc + 0.5*(y_compl[i] + y_compl[i+1])*(t_compl[i+1] - t_compl[i])
  }

  #### rescale ####
  if(!is.null(rescale)){auc <- rescale * auc/(t_compl[n_compl] - t_compl[1])}

  return(auc)
}




