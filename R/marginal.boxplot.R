###########################################################################################
###      Filename: marginal.boxplot.R
### Creation Date: Friday, 13 November 2015 01:00 PM CST
### Last Modified: Thursday, 03 December 2015 08:30 AM CST
###########################################################################################

#' marginal.boxplot
#'
#' Plot boxplots on the margins of a scatterplot in ggplot2. This code is not totally written by
#'  Ethan; he adapted it from \url{http://www.r-bloggers.com/scatterplot-with-marginal-boxplots/}.
#'
#' @param data The data.frame to feed into ggplot2
#' @param x A character string of the x-values to use in 'data'. Will be coerced to numeric if it isn't already.
#' @param y A character string of the y-values to use in 'data'. Will be coerced to numeric if it isn't already.
#' @param by A character string of the factor to use in 'data' for color/fill. If missing, defaults to no grouping. Will be coerced to factor if it isn't already.
#' @param xlim The x-limits. If missing, defaults to a good view.
#' @param ylim The y-limits. If missing, defaults to a good view.
#' @param xlab The x-label. Defaults to 'x'. See details.
#' @param ylab The y-label. Defaults to 'y'. See details.
#' @param bylab The label for the by-variable. Defaults to "".
#' @param width The width of jitter. Default 0.1. A warning is issued if > 0.3.
#' @param counts Display the counts of each 'by' group in the top right corner? If 'by' is missing, displays nrow(data).
#' @param countsize If the counts are displayed, what size should they be? Default = 5 - floor(# levels / 4).
#' @return Nothing is returned; however, the ggplot objects are printed.
#' @details
#' I considered using arguments that don't require character strings, but I think this way might just be safer.
#'   The only downside is that I need to fill the xlab and ylab so they're not so dang ugly. That's why the
#'   defaults are to 'x' and 'y'. The plus-side is that I have fewer if-else's when specifying '+xlab()+ylab()'
#'   to 'ggplot()'. It's also easier (in my opinion) to catch when the variable names aren't in 'data'.
#' @examples
#' marginal.boxplot(pizza.data, x = "height", y = "time_to_fu")
#' marginal.boxplot(pizza.data, x = "height", y = "time_to_fu", by = "sex",
#'  xlab = "Height", ylab = "Time to Follow-Up", bylab = "Sex", counts = TRUE)
#' @export
#' @seealso
#'  \url{http://www.r-bloggers.com/scatterplot-with-marginal-boxplots/}
#' @import ggplot2
marginal.boxplot <- function(data, x, y, by, xlim, ylim, xlab = "x", ylab = "y", bylab = "", width = 0.5, counts = FALSE, countsize = 5)
{
  if(missing(data) || !is.data.frame(data)) {stop("'data' argument must be a data.frame.")}
  if(missing(x)) {stop("'x' argument must be non-missing.")}
  if(missing(y)) {stop("'y' argument must be non-missing.")}
  if(!is.character(x) || length(x) != 1) {stop("'x' argument must be character string.")}
  if(!is.character(y) || length(y) != 1) {stop("'y' argument must be character string.")}
  if(!(x %in% colnames(data))) {stop("'x' argument must be in colnames(data).")}
  if(!(y %in% colnames(data))) {stop("'y' argument must be in colnames(data).")}
#  if(!kinda.numeric(data[,x]) || !kinda.numeric(data[,y])) {stop("data[ , x] and data[ , y] must be coercible to numeric.")}
  if(!is.numeric(data[,x])) {data[,x] <- as.numeric(data[,x])}
  if(!is.numeric(data[,y])) {data[,y] <- as.numeric(data[,y])}
  if(!missing(by) && (!is.character(by) || length(by) != 1)) {stop("'by' argument must be character string.")}
  if(!missing(by) && !(by %in% colnames(data))) {stop("'by' argument must be in colnames(data).")}
 # if(!missing(by) && !kinda.factor(data[,by])) {stop("data[ , by] argument must coercible to a factor.")}
  if(!missing(by) && !is.factor(data[,by])) {data[,by] <- as.factor(data[,by])}
  if(!missing(xlim) && (!is.vector(xlim) || !is.numeric(xlim) || length(xlim) != 2)) {stop("'xlim' argument must be a vector of length 2.")}
  if(!missing(ylim) && (!is.vector(ylim) || !is.numeric(ylim) || length(ylim) != 2)) {stop("'ylim' argument must be a vector of length 2.")}
  if(!is.character(xlab) || length(xlab) != 1) {stop("'xlab' argument must be character string.")}
  if(!is.character(ylab) || length(ylab) != 1) {stop("'ylab' argument must be character string.")}
  if(!is.character(bylab) || length(bylab) != 1) {stop("'bylab' argument must be character string.")}
  if(!is.numeric(width) || length(width) != 1 || width < 0) {stop("'width' argument must be numeric constant >= 0.")}
  if(!is.logical(counts)){stop("'counts' argument must be logical.")}
  if(!is.numeric(countsize) || length(countsize) != 1 || countsize < 0){stop("'size' argument must be a numeric constant >= 0.")}

  if(width > 1) {warning("'width' argument is > 1. Results may not be as expected.")}

  if(missing(by))
  {
    p1 <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = if(missing(ylim)) c(min(data[,y]) - 0.1 * diff(range(data[,y])), max(data[,y]) + 0.1 * diff(range(data[,y]))) else ylim) +
      expand_limits(x = if(missing(xlim)) c(min(data[,x]) - 0.1 * diff(range(data[,x])), max(data[,x]) + 0.1 * diff(range(data[,x]))) else xlim) +
      theme(plot.margin = grid::unit(c(0.9, 0.9, 0.5, 0.5), "lines"), legend.position = "none") +
      xlab(xlab) +
      ylab(ylab)

    p2 <- ggplot(data, aes_string(x = "factor(1)", y = x)) +
      geom_boxplot(outlier.colour = NA) +
      geom_jitter(position = position_jitter(width = width)) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = if(missing(xlim)) c(min(data[,x]) - 0.1 * diff(range(data[,x])), max(data[,x]) + 0.1 * diff(range(data[,x]))) else xlim) +
      theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
                     plot.margin = grid::unit(c(1, 0.9, -0.5, 0.5), "lines"), legend.position = "none") +
      coord_flip()
    p3 <- ggplot(data, aes_string(x = "factor(1)", y = y)) +
      geom_boxplot(outlier.colour = NA) +
      geom_jitter(position = position_jitter(width = width)) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = if(missing(ylim)) c(min(data[,y]) - 0.1 * diff(range(data[,y])), max(data[,y]) + 0.1 * diff(range(data[,y]))) else ylim) +
      theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
                     plot.margin = grid::unit(c(0.9, 1, 0.5, -0.5), "lines"), legend.position = "none")

    tmp <- data.frame(x = factor(1), y = factor(1), n = paste0("n=",nrow(data)))
    p4 <- ggplot(tmp, aes_string(x = "x", y = "y", label = "n")) +
      geom_text(size = countsize) +
      theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
                     plot.margin = grid::unit(c(1, 1, -0.5, -0.5), "lines"), legend.position = "none")
  } else
  {
    p1 <- ggplot(data, aes_string(x = x, y = y, color = by)) +
      geom_point() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = if(missing(ylim)) c(min(data[,y]) - 0.1 * diff(range(data[,y])), max(data[,y]) + 0.1 * diff(range(data[,y]))) else ylim) +
      expand_limits(x = if(missing(xlim)) c(min(data[,x]) - 0.1 * diff(range(data[,x])), max(data[,x]) + 0.1 * diff(range(data[,x]))) else xlim) +
      theme(plot.margin = grid::unit(c(0.9, 0.9, 0.5, 0.5), "lines"), legend.position = "none") +
      xlab(xlab) +
      ylab(ylab)
    print(p1)

    p2 <- ggplot(data, aes_string(x = by, y = x)) +
      geom_boxplot(outlier.colour = NA) +
      geom_jitter(position = position_jitter(width = width), aes_string(color = by)) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = if(missing(xlim)) c(min(data[,x]) - 0.1 * diff(range(data[,x])), max(data[,x]) + 0.1 * diff(range(data[,x]))) else xlim) +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(),
                     plot.margin = grid::unit(c(1, 0.9, -0.5, 0.5), "lines"), legend.position = "none") +
      coord_flip() +
      xlab(bylab)
    p3 <- ggplot(data, aes_string(x = by, y = y)) +
      geom_boxplot(outlier.colour = NA) +
      geom_jitter(position = position_jitter(width = width), aes_string(color = by)) +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = if(missing(ylim)) c(min(data[,y]) - 0.1 * diff(range(data[,y])), max(data[,y]) + 0.1 * diff(range(data[,y]))) else ylim) +
      theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
                     plot.margin = grid::unit(c(0.9, 1, 0.5, -0.5), "lines"), legend.position = "none")

    tmp <- data.frame(x = levels(data[,by]), y = levels(data[,by]), n = sapply(levels(data[,by]),
                                                                               function(lvl, data_){paste0("n=",sum(data_ == lvl))}, data_ = data[,by]))
    tmp$x <- factor(tmp$x, levels = levels(data[,by])) # need these to line up the right levels with the right levels
    tmp$y <- factor(tmp$y, levels = levels(data[,by])) # need these to line up the right levels with the right levels

    p4 <- ggplot(tmp, aes_string(x = "x", y = "y", label = "n", color = "x")) +
      geom_text(size = countsize) +
      theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
                     plot.margin = grid::unit(c(1, 1, -0.5, -0.5), "lines"), legend.position = "none")
  }

  gt1 <- ggplot_gtable(ggplot_build(p1))
  gt2 <- ggplot_gtable(ggplot_build(p2))
  gt3 <- ggplot_gtable(ggplot_build(p3))
  gt4 <- ggplot_gtable(ggplot_build(p4))

  # Get maximum widths and heights for x-axis and y-axis title and text
  maxWidth <- grid::unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
  maxHeight <- grid::unit.pmax(gt1$heights[4:5], gt3$heights[4:5])

  # Set the maximums in the gtables for gt1, gt2 and gt3
  gt1$widths[2:3] <- as.list(maxWidth)
  gt2$widths[2:3] <- as.list(maxWidth)

  gt1$heights[4:5] <- as.list(maxHeight)
  gt3$heights[4:5] <- as.list(maxHeight)

  # Combine the scatterplot with the two marginal boxplots
  # Create a new gtable
  gt <- gtable::gtable(widths = grid::unit(c(7, 2), "null"), height = grid::unit(c(2, 7), "null"))

  # Instert gt1, gt2 and gt3 into the new gtable
  gt <- gtable::gtable_add_grob(gt, gt1, 2, 1)
  gt <- gtable::gtable_add_grob(gt, gt2, 1, 1)
  gt <- gtable::gtable_add_grob(gt, gt3, 2, 2)
  if(counts)
  {
    gt <- gtable::gtable_add_grob(gt, gt4, 1, 2)
  }

  # And render the plot
  grid::grid.newpage()
  grid::grid.draw(gt)


}
