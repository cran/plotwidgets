#' A showcase of all plot types available in plotwidgets
#'
#' A showcase of all plot types available in plotwidgets
#'
#' plotwidgetGallery() simply draws all available plot widgets on a single
#' plot.
#'
#' \if{html}{\figure{gallery.svg}{options: width=400 alt="Gallery example"}}
#' \if{latex}{\figure{gallery.pdf}{options: width=5in}}
#' @param ... all subsequent parameters will be passed to the plotWidget() function.
#' @param theme sets both the palette and a suitable bakckground (e.g. dark for "neon")
#' @param pal color palette to use (a character vector with colors)
#' @return Invisibly returns the example data used to generate the plots
#' @examples
#' plotwidgetGallery()
#' ## automatically set black bg
#' plotwidgetGallery(theme="neon") 
#' ## yuck, ugly:
#' plotwidgetGallery(pal=c("red", "#FF9900", "blue", "green", "cyan", "yellow"))
#' ## much better:
#' plotwidgetGallery(pal=plotPals("pastel", alpha=0.8))
#' @export
plotwidgetGallery <- function(theme="default", pal=NULL, ...) {

  widgets <- .widget_list()
  theme <- match.arg(theme, names(.pals()))

  cols <- switch(theme,
    neon=list(bg="black", fg="white"),
    list(bg="white", fg="black"))


  old.par <- par(c(cols, list(mfrow=c(3,3), mar=c(1.5, 1, 3, 1))))
  on.exit(par(old.par))

  if(is.null(pal)) pal <- .plotWidgetDefaultPalette(theme=theme)

  example.data <- list(
    wgSpider=matrix(
      c(10, 7, 4, 19, 6, 8, 20,
        11, 10, 10, 16, 8, 10, 11 ,
        12, 6, 4, 8, 22, 6, 17), byrow=F, ncol=3),
    wgRoccurve=list(a=c(T, T, T, F, T, F, F, T, F, F, F),
                    b=c(T, F, T, F, F, T, F, F, T, F, F))
    )

  fracplots <- c("wgBurst", "wgRing" , "wgBarplot" , "wgPlanets" , "wgRug" , "wgBoxpie" , "wgPie" )
  for(i in fracplots) example.data[[i]] <- c(10, 9, 19, 9, 15, 5)

  user.params    <- list(...)
  default.params <- list(w=1, h=1, x=0.5, y=0.5, new=T, col=pal, aspect=1)

  example.params <- list(
    wgSpider=list(lwd=4),
    wgPie=list(),
    wgRoccurve=list(fill=TRUE),
    wgRug=list(),
    wgRing=list(),
    wgBurst=list(adj=10),
    wgBoxpie=list())

  for(n in names(example.data)) {
    params <- c(list(type=n, v=example.data[[n]]), user.params, example.params[[n]], default.params)
    params <- params[!duplicated(names(params))]
    do.call(plotWidget, params)
    title(n, col.main=cols[[2]])
  }

  return(invisible(example.data))
}


#' List available plot widgets
#'
#' List available plot widgets
#'
#' Simply print out all available wigdets. A list containing the widget
#' function is returned invisibly.
#'
#' @return Invisibly returns a named list of widget functions
#' @examples
#' listPlotWidgets()
#' @export
listPlotWidgets <- function() {

  widgets <- .widget_list()
  print(names(widgets))
  return(invisible(widgets))
}


