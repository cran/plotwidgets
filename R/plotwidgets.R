#' Plot widgets
#' 
#' A collection of plotting widgets (pie charts, spider plots etc.)
#'
#' \if{html}{\figure{overview.svg}{options: width=800 alt="Overview of all widgets"}}
#' \if{latex}{\figure{overview.pdf}{options: width=5in}}
#'
#' A widget here is a mini-plot which can be placed anywhere on any other
#' plot, at an area specified by the center point coordinates x and y, and
#' width and height. The widgets do not influence the layout of the plot, or change any
#' parameters with \code{par()}; it only uses basic graphic primitives for
#' maximal compatibility.
#'
#' The \code{plotWidget()} function is a generic to call the available
#' widgets from one place. Any widget-specific parameters are passed on
#' through the ellipsis (...). See the documentation for specific widgets
#' for more information. Use the functions \code{\link{listPlotWidgets}}
#' and \code{\link{plotwidgetGallery}} to see available widgets.
#'
#' The pie function draws a simple pie chart at specified coordinates with
#' specified width, height and color. The rug function draws a
#' corresponding rug plot, while boxpie creates a "rectangular pie
#' chart" that is considered to be better legible than the regular pie.
#' 
#' @param type what kind of plot. To list all available plot types, use
#'      \code{listPlotWidgets}. Instead of the canonical name such as "wgSpider", you
#'      can simply use "spider"
#' @param x,y coordinates at which to draw the plot
#' @param w,h width and height of the plot
#' @param v sizes of the slices
#' @param col character vector with colors (palette). A default palette is chosen if the argument is NULL
#' @param aspect the actual ratio between screen width and screen height.
#'        Parameters w and h will be scaled accordingly.
#' @param new  Whether to call plot.new() before drawing the widget
#' @param border color of the border. Use NA (default) for no border, and
#'        NULL for a border in the par("fg") color. No effect in spider plots.
#' @param adj for spider plots, burst plots, planet plots and pies: adjust the start by
#'        "adj" degrees. For example, to turn the plot clockwise by 90 degrees, use
#'        adj=90
#' @param max For burst plots -- a maximum value used to scale other values
#'            to make different plots comparable. For spider plots: the
#'            maximum of the scale on spider plot spikes.
#' @param min For spider plots: the minimum of the scale on spider plot spikes
#' @param horizontal for rug plots: whether the stacked bar should be
#'        horizontal (default), or vertical
#' @param start For ring plots, the inner radius of the ring
#' @param rev logical, for rug plots: right to left or top to bottom rather
#'        than the default left to right or bottom to top
#' @param res for pies, rings, planets and bursts: resolution (number of polygon edges in a full circle)
#' @param grid boxpie only: the grid over which the areas are distributed.
#'             Should be roughly equal to the number of areas shown.
#' @param fill for spider plots and roc curves: whether to draw filled polygons rather
#'        than lines
#' @param lwd Line width to use
#' @param tick.labels For spider plots: labels for the scale ticks
#' @param labels character vector
#' @param label.params a named list with arguments passed to text() for
#'        drawing labels (e.g. cex, col etc.)
#' @param ... Any further arguments passed to plotting functions
#' @import graphics grDevices methods
#' @seealso \code{\link{wgPie}}, \code{\link{wgBoxpie}}, \code{\link{wgSpider}}, \code{\link{wgRug}}, \code{\link{wgBurst}},
#'          \code{\link{wgRing}}, \code{\link{wgBarplot}}, \code{\link{wgPlanets}}, \code{\link{wgRoccurve}}

#' @examples
#' # demonstration of the three widgets
#' plot.new()
#' par(usr=c(0,3,0,3))
#' v <- c(7, 5, 11)
#' col <- plotPals("safe")
#' b <- "black"
#' wgRug(0.5, 1.5, 0.8, 0.8, v=v, col=col, border=b)
#' wgPie(1.5, 1.5, 0.8, 0.8, v=v, col=col, border=b)
#' wgBoxpie(2.5, 1.5, 0.8, 0.8, v=v, col=col, border=b)
#'
#' # using pie as plotting symbol
#' plot(NULL, xlim=1:2, ylim=1:2, xlab="", ylab="")
#' col <- c("#cc000099", "#0000cc99")
#' for(i in 1:125) { 
#'   x <- runif(1) + 1 
#'   y <- runif(1) + 1
#'   wgPie( x, y, 0.05, 0.05, c(x,y), col)
#' }
#'
#' # square filled with box pies
#' n <- 10 
#' w <- h <- 1/(n+1)
#' plot.new()
#' for(i in 1:n) for(j in 1:n) 
#'  wgBoxpie(x=1/n*(i-1/2), y=1/n*(j-1/2), w, h, 
#'  v=runif(3), col=plotPals("zeileis"))
#' @docType package
#' @name plotwidgets
NULL

#' @rdname plotwidgets
#' @export
plotWidget <- function(type="pie", x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, new=FALSE, aspect=1, ...) {

  type     <- .getWidgetName(type)
  widgets  <- .widget_list()
  plotfunc <- widgets[[type]]

  .widgetWrapper(plotfunc, x, y, w, h, v, col=col, border=border, new=new, aspect=aspect, ...)

}

## converts "spider" to "wgSpider" etc.
.getWidgetName <- function(type) {

  widgets <- .widget_list()
  wgnames <- names(widgets)
  type <- match.arg(type, c(wgnames, tolower(gsub("^wg", "", wgnames))))
  if(!grepl("^wg[A-Z]", type)) {
    type <- .convName(type)
  }

  return(type)

}


## checking common parameters, sanitize values, calling the actual function
.widgetWrapper <- function(func, x, y, w, h, v, col=NULL, border=NA, new=FALSE, aspect=NULL, ...) {

  if(new) {
    plot.new()
  }

  if(is.null(border)) border <- par("fg")

  dev.hold()
  on.exit(dev.flush())

  if(.isnn(col)) col <- .plotWidgetDefaultPalette()

  if(!is.null(aspect)) {
    pp <- par("pin")
    pu <- par("usr")
    pu <- c(pu[2]-pu[1], pu[4]-pu[3])
    ppu <- pp/pu

    wi <- w * ppu[1] # box width in inches
    hi <- h * ppu[2] # box height in inches
    
    if(wi > hi * aspect) {
      wi <- hi * aspect
    } else {
      hi <- wi / aspect
    }

    w <- wi / ppu[1]
    h <- hi / ppu[2]
  }
 
  func(x=x, y=y, w=w, h=h, v=v, col=col, border=border, ...)
}


.widget_list <- function() {
  names <- list(
    wgPie=.pieWidget,
    wgBoxpie=.boxpieWidget,
    wgSpider=.spiderWidget,
    wgRug=.rugWidget,
    wgBurst=.burstWidget,
    wgRing=.ringWidget,
    wgBarplot=.barplotWidget,
    wgPlanets=.planetsWidget,
    wgRoccurve=.roccurvewidget)

  names
}


.isnn <- function(x) any(is.null(x) | is.na(x))

.upcaseFist <- function(name) paste0(toupper(substring(name, 1, 1)), substring(name, 2))

.convName <- function(name) paste0("wg", .upcaseFist(name))




