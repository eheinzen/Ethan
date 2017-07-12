###########################################################################################
###      Filename: coxtable.R
### Creation Date: Tuesday, 28 July 2015 5:00 PM CDT
### Last Modified: Tuesday, 31 May 2016 01:30 PM CDT
###########################################################################################

#' coxtable
#'
#' Make a pretty cox model table.
#'
#' @param cox.model A cox object
#' @param rownames (optional) The row names to put on the table; defaults to what summary() does. Must be a character vector
#' @param caption.prefix (optional) A string to put in front of the caption.
#'   By default, the caption contains N, N_events, and N_missing.
#'
#' @param print.xtable If TRUE, prints the cox table created as an xtable object. If FALSE (the default), it simply returns the table.
#' @param digits If print.xtable is TRUE, how many digits should each column have?
#'   Note that the p-value is automatically converted to scientific notation where appropriate.
#' @return If print.xtable is FALSE, a table of cox model estimates, SEs, Z-scores, p-values, hazard ratios, and confidence intervals.
#' @examples
#' library(survival)
#' model <- coxph(Surv(time_to_fu, status_fu) ~ likes_pizza, data = pizza.data)
#' coxtable(model, "Likes Pizza", "Unadjusted Cox Model.", TRUE, 3)
#' model <- coxph(Surv(time_to_fu, status_fu) ~ likes_pizza + fav_flavor, data = pizza.data)
#' coxtable(model, c("Likes Pizza", "Pepperoni", "Sausage"), "Adjusted Cox Model.", TRUE, 3)
#' @export
coxtable <- function(cox.model, rownames, caption.prefix = "", print.xtable = FALSE, digits = 2)
{
  if(missing(cox.model) || !(class(cox.model) == "coxph")) {stop("Not a valid cox.model object.")}
  if(!missing(rownames) && !is.character(rownames))
  {stop("Parameter rownames not valid: make sure it's a character vector.")}

  nrows <- nrow(coef(summary(cox.model)))

  if(nrows == 1)
  {
    modeltable <- cbind(t(as.matrix(coef(summary(cox.model))[,-2])), t(as.matrix(summary(cox.model)$conf[,-2])))
    # Have to t(as.matrix) because subscripting changes it to a vector
    # and matrix() strips the names off, so that's no good
  } else
  {
    modeltable <- cbind(coef(summary(cox.model))[,-2], summary(cox.model)$conf[,-2])
  }

  colnames(modeltable)[5] <- "HR"

  if(!missing(rownames) && length(rownames) == nrows)
  {
    rownames(modeltable) <- rownames
  } else if(!missing(rownames))
  {
    warning("\nrownames parameter not the right length; returning table with default row names.\n")
  }

  if(!print.xtable)
  {
    if(caption.prefix != "" || digits != 2)
    {
      message("\nCaption and digits arguments are being ignored.\n")
    }
    return(modeltable)
  } else
  {

    caption <- paste0(caption.prefix, " N = ", cox.model$n,
                      ", N events = ", cox.model$nevent, ", N missing = ", length(cox.model$na.action))
    model.xtable <- xtable::xtable(modeltable, caption = caption, digits = digits)
    xtable::display(model.xtable)[5] <- "g" # adjusts the p-values so that they display in scientific notation if appropriate.
    print(model.xtable, comment = FALSE)
  }
}






