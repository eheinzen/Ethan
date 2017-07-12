###########################################################################################
###      Filename: multiplot.R
### Creation Date: Friday, 21 August 2015 11:15 AM CDT
### Last Modified: Monday, 09 January 2017 01:15 PM CST
###########################################################################################

#' multiplot
#'
#' Plot more than one ggplot object in a grid fashion, similar to \code{par()}. This code is not totally written by
#'  Ethan; he adapted it from \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}.
#'
#' @param ... ggplot objects.
#' @param plotlist A list of ggplot objects.
#' @param ncol How many columns in the grid of plots? Ignored if 'layout' is specified.
#' @param byrow Should plots be printed by rows or by columns? Ignored if 'layout' is specified.
#' @param layout A matrix of indices of plots specifying where in the grid the plots should be printed. See details.
#' @param title A string denoting the title.
#' @return Nothing is returned; however, the ggplot objects are printed.
#' @details
#' If 'layout' is specified, 'ncol' and 'byrow' will be ignored. Each plot will then be
#'   printed in the grid location corresponding to the entry/entries in 'layout' which list(s) its
#'   index in the function call. E.g. the first plot will be plotted in the grid corresponding to where
#'   '1' shows up in 'layout'. If any entry is '0', then nothing will be plotted there.
#' It is possible to have a plot span more than one place in the grid; see the examples.
#' @examples
#' library(ggplot2)
#' data(pizza.data)
#' p1 <- ggplot(pizza.data, aes(x = fav_flavor, y = time_to_fu)) + geom_boxplot()
#' p2 <- ggplot(pizza.data, aes(x = likes_pizza, y = time_to_fu)) + geom_boxplot()
#' p3 <- ggplot(pizza.data, aes(x = time_to_fu, fill = likes_pizza)) + geom_histogram()
#' multiplot(p1, p2, p3, layout = matrix(c(1,2,3,3), ncol = 2, byrow = TRUE))
#' ## Will give a warning
#' \dontrun{
#' multiplot(plotlist = list(p1, p2, p3), layout = matrix(c(1,2,1,2), ncol = 2, byrow = TRUE))
#' }
#' @export
#' @seealso
#'  \url{http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/}
#'  \url{http://stackoverflow.com/questions/10776139/r-grid-layout-title}
multiplot <- function(..., plotlist = NULL, ncol = 1, byrow = FALSE, layout = NULL, title)
{
  #require(ggplot2)
  ## because the ggplot2::print.ggplot isn't available
  ## but apparently it still works even without this because of the DESCRIPTION file

  if(!is.null(plotlist) && !is.list(plotlist)){stop("Parameter 'plotlist' must be NULL or a list.")}
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  if(any(sapply(plots, function(x) !inherits(x, "ggplot")))){stop("One or more of the objects supplied is not a ggplot object.")}

  numPlots <- length(plots)

  # If layout is NULL, then use 'ncol' to determine layout
  if(is.null(layout))
  {
    if(!is.numeric(ncol)){stop("Parameter 'ncol' must be numeric.")}
    if(!is.logical(byrow)){stop("Parameter 'ncol' must be logical.")}
    ncol <- as.integer(ncol)
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of ncol
    layout <- matrix(seq(1, ncol * ceiling(numPlots/ncol)),
                     ncol = ncol, nrow = ceiling(numPlots/ncol), byrow = byrow)
  } else
  {
    if(!is.matrix(layout) || !is.numeric(layout)){stop("Parameter 'layout' must be a numeric matrix.")}
    mode(layout) <- "integer"
    if(any(layout < 0)){stop("Parameter 'layout' must have all non-negative entries.")}
    if(max(layout) < numPlots)
    {
      warning(paste0("The maximum value in 'layout' is ", max(layout), ", but ", numPlots, " plots are specified.\n  Ignoring the remaining plots."))
      numPlots <- max(layout)
    }
  }

  if(numPlots==1)
  {
    print(plots[[1]])

  } else
  {
    if(missing(title) || !is.character(title))
    {
      if(!missing(title) && !is.character(title)){warning("'title' parameter is not a character. Ignoring.")}

      # Set up the page
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

      # Make each plot, in the correct location
      for (i in 1:numPlots)
      {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                              layout.pos.col = matchidx$col))
      }
    } else
    {
      title_layout <- matrix(0, ncol = ncol(layout), nrow = 1)
      layout <- rbind(title_layout, layout)
      # Set up the page
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout),
                                                                   heights = grid::unit(c(0.75, rep(5, times = nrow(layout) - 1)), "null"))))
      grid::grid.text(title, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)))


      # Make each plot, in the correct location
      for (i in 1:numPlots)
      {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                              layout.pos.col = matchidx$col))
      }
    }
  }
}
