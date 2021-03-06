% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coxtable.R
\name{coxtable}
\alias{coxtable}
\title{coxtable}
\usage{
coxtable(cox.model, rownames, caption.prefix = "", print.xtable = FALSE,
  digits = 2)
}
\arguments{
\item{cox.model}{A cox object}

\item{rownames}{(optional) The row names to put on the table; defaults to what summary() does. Must be a character vector}

\item{caption.prefix}{(optional) A string to put in front of the caption.
By default, the caption contains N, N_events, and N_missing.}

\item{print.xtable}{If TRUE, prints the cox table created as an xtable object. If FALSE (the default), it simply returns the table.}

\item{digits}{If print.xtable is TRUE, how many digits should each column have?
Note that the p-value is automatically converted to scientific notation where appropriate.}
}
\value{
If print.xtable is FALSE, a table of cox model estimates, SEs, Z-scores, p-values, hazard ratios, and confidence intervals.
}
\description{
Make a pretty cox model table.
}
\examples{
library(survival)
model <- coxph(Surv(time_to_fu, status_fu) ~ likes_pizza, data = pizza.data)
coxtable(model, "Likes Pizza", "Unadjusted Cox Model.", TRUE, 3)
model <- coxph(Surv(time_to_fu, status_fu) ~ likes_pizza + fav_flavor, data = pizza.data)
coxtable(model, c("Likes Pizza", "Pepperoni", "Sausage"), "Adjusted Cox Model.", TRUE, 3)
}
