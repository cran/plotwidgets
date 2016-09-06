## --------------------  pie widget --------------------

#' @describeIn plotwidgets \code{pie()} draws a pie chart with width w and height h at coordinates
#' (x,y). The angle width of the slices is taken from the numeric vector v, and
#' their color from the character vector col. Note that one of the main goals of the
#' plotwidget package is to give sufficient alternatives to pie charts, hoping
#' to help eradicate pie charts from the surface of this planet.
#' @export
wgPie <- function(x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, new=FALSE, res=100, aspect=1, adj=0,
  labels=NULL, label.params=NULL) {
  .widgetWrapper(.pieWidget, x, y, w, h, v, 
    col=col, res=res, border=border, new=new, aspect=aspect, adj=adj,
    labels=labels, label.params=label.params)
}

.pieWidget <- function(x, y, w, h, v, col=NULL, border=NA, res=100, adj=0,
  labels=NULL, label.params=NULL) {

  v <- c(0, cumsum(v)/sum(v))
  dv <- diff(v)
  nv <- length(dv)

  nn <- res
  w <- w / 2
  h <- h / 2

  # commodity function converting polar to cartesian
  a2xy <- function(a, r=1) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  for(i in 1:nv) {
    ni <- max(3, floor(nn * dv[i])) 
    po <- a2xy(seq.int(v[i], v[i+1], length.out=ni))
    polygon(c(x, po$x), c(y, po$y), col=col[i], border=NA)
  }

  if(!is.na(border)) {
    po <- a2xy(seq.int(0, 1, length.out=res))
    lines(c(po$x, po$x[1]), c(po$y, po$y[1]), col=border)
  }

  # labels in the middle of each slice
  if(!is.null(labels)) {
    angle <- (v[1:nv] + v[2:(nv+1)])/2
    lp <- a2xy(angle, r=2/3)
    params <- c(list(x=lp$x, y=lp$y, labels=labels), label.params)
    do.call(text, params)
  }


  invisible(NULL)
}

## --------------------  ring widget --------------------

## wrapper for the actual function

#' @describeIn plotwidgets A pie with the center removed.
#' @export
wgRing <- function(x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, new=FALSE, res=100, 
  aspect=1, adj=0, start=0.5,
  labels=NULL, label.params=NULL) {

  .widgetWrapper(.ringWidget, x, y, w, h, v, 
    col=col, res=res, border=border, 
    new=new, aspect=aspect, adj=adj, start=start,
    labels=labels, label.params=NULL)
}

## the actual ring plot function
.ringWidget <- function(x, y, w, h, v, 
  col=NULL, border=NA, res=100, adj=0, start=0.5,
  labels=NULL, label.params=NULL) {

  v <- c(0, cumsum(v)/sum(v))
  dv <- diff(v)
  n <- length(dv)

  nn <- res
  w <- w / 2
  h <- h / 2

  # commodity function for converting polar to cartesian
  a2xy <- function(a, r=1) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  # draw all the slices
  for(i in 1:n) {
    ni <- max(3, floor(nn * dv[i])) 
    po  <- a2xy(seq.int(v[i], v[i+1], length.out=ni))
    po2 <- a2xy(seq.int(v[i+1], v[i], length.out=ni), r=start)
    polygon(c(po$x, po2$x), c(po$y, po2$y), col=col[i], border=NA)
  }

  # draw a border if necessary
  if(!is.na(border)) {
    po <- a2xy(seq.int(0, 1, length.out=res))
    lines(c(po$x, po$x[1]), c(po$y, po$y[1]), col=border)
    po <- a2xy(seq.int(0, 1, length.out=res), r=start)
    lines(c(po$x, po$x[1]), c(po$y, po$y[1]), col=border)
  }

  # labels in the middle of the ring
  if(!is.null(labels)) {
    angle <- (v[1:n] + v[2:(n+1)])/2
    lp <- a2xy(angle, (start + 1)/2)
    params <- c(list(x=lp$x, y=lp$y, labels=labels), label.params)
    do.call(text, params)
  }


  invisible(NULL)
}



## --------------------  planets widget --------------------

#' @describeIn plotwidgets Produces a circular arrangement of circles, which
#'            vary in their size proportionally to the values in \code{v}.
#' @export
wgPlanets <- function(x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, 
  new=FALSE, res=100, aspect=1, adj=0, 
  labels=NULL, label.params=NULL) {
  .widgetWrapper(.planetsWidget, x, y, w, h, v, 
    col=col, res=res, border=border, new=new, aspect=aspect, adj=adj, 
    labels=labels, label.params=label.params)
}

## actual function for drawing planet plots
.planetsWidget <- function(x, y, w, h, v, 
  col=NULL, border=NA, res=100, adj=0, 
  labels=NULL, label.params=NULL) {

  v <- sqrt(v)
  dv <- v/sum(v) 
  v <- c(0, cumsum(v)/sum(v))
  nv <- length(dv)
  vmax <- max(dv)

  nn <- res
  w <- w / 2
  h <- h / 2

  # commodity function for converting polar to cartesian
  a2xy <- function(a, r=0.5) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=r * w * cos(t), y=r * h * sin(t) )
  }

  # draw a circle at the specified position
  circle <- function(xx, yy, w, h, r, col=NA, border=NA) {
    po <- a2xy(seq.int(0, 1, length.out=res), r=r)
    polygon(xx + po$x, yy + po$y, col=col, border=border)
  }

  circle(x, y, w, h, 0.5, border="grey")

  # draw each "planet"
  for(i in 1:nv) {
    po <- a2xy(seq.int(v[i], v[i+1], length.out=3), r=0.5)
    r2 <- 0.5 * dv[i] / vmax
    #circle(x + po$x[2], y + po$y[2], r=r2, col=col[i], border=border)
    r2 <- tan(pi * dv[i]) * 0.5
    circle(x + po$x[2], y + po$y[2], r=r2, col=col[i], border=border)

  }

  # labels in the middle of each planet
  if(!is.null(labels)) {
    angle <- (v[1:nv] + v[2:(nv+1)])/2
    lp <- a2xy(angle, r=0.5)
    params <- c(list(x=x + lp$x, y=y + lp$y, labels=labels), label.params)
    do.call(text, params)
  }

  invisible(NULL)
}



## --------------------  burst widget --------------------

## wrapper for the actual function

#' @describeIn plotwidgets Burst plots are similar to pies. However, instead of approximating
#' numbers by arc length, wgBurst approximates by area, making the pie slices stand
#' out from the plot in the process.
#' @export
wgBurst <- function(x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, new=FALSE, 
  res=100, aspect=1, adj=0, max=NULL, 
  labels=NULL, label.params=NULL) {

  .widgetWrapper(.burstWidget, x, y, w, h, v, 
    col=col, res=res, border=border, 
    new=new, aspect=aspect, adj=adj, max=max, 
    labels=labels, label.params=label.params)
}

## the actual burst plot function
.burstWidget <- function(x, y, w, h, v, 
  col=NULL, border=NA, res=100, adj=0, max=NULL, 
  labels=NULL, label.params=NULL) {

  # plot areas, so sqrt
  v <- sqrt(v)
  if(is.null(max)) max <- max(v)
  v <- v / max / 2
  n <- length(v)
  nn <- res
  
  # commodity function for turning polar into cartesian
  a2xy <- function(a, r = 0.5) {
    t <- pi * (0.5  - 2 * a) - pi * adj/180
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  # positions of the pie fragments
  rad <- seq.int(0, 1, length.out=n+1)

  for(i in 1:n) {
    ni <- max(3, floor(nn * 1/n)) 
    po <- a2xy(seq.int(rad[i], rad[i+1], length.out=ni), r=v[i])
    polygon(c(x, po$x), c(y, po$y), col=col[i], border=border)
  }

  if(!is.null(labels)) {
    angle <- seq.int(1/n/2, 1 - 1/n/2, length.out=n)
    lp <- a2xy(angle, v/2)
    params <- c(list(x=lp$x, y=lp$y, labels=labels), label.params)
    do.call(text, params)
  }

  invisible(NULL)
}

## --------------------  rug widget --------------------

#' @describeIn plotwidgets A stacked, horizontal or vertical bar plot.
#' @export
wgRug <- function(x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, 
  labels=NULL, label.params=NULL,
  new=FALSE, aspect=NULL,
  horizontal=TRUE, rev=FALSE) {

  .widgetWrapper(.rugWidget, x, y, w, h, v, 
    col=col, border=border, 
    labels=labels, label.params=label.params,
    new=new, aspect=aspect, horizontal=horizontal, rev=rev)
}

## the actual function for plotting rug plots
.rugWidget <- function(x, y, w, h, v, col=NULL, border=NULL,
  labels=NULL, label.params=NULL, horizontal=TRUE, rev=FALSE) {

  x <- x - w/2
  y <- y - h/2

  if(horizontal) {
    v <- c(0, cumsum(v)/sum(v)) * w
    if(rev) v <- w - v
    nv <- length(v)
    rect( x + v[-nv], y, x + v[-1],  y+h, col=col, border=NA)
    rect(x, y, x + w, y + h, col=NA, border=border)
  } else {
    v <- c(0, cumsum(v)/sum(v)) * h
    if(rev) v <- h - v
    nv <- length(v)
    rect(x, y + v[-nv], x + w, y + v[-1], col=col, border=NA)
    rect(x, y, x + w, y + h, col=NA, border=border)
  }

  # labels in the middle of each bar
  if(!is.null(labels)) {
    xx <- (v[-nv] + v[-1])/2
    yy <- y + h/2
    params <- c(list(x=xx, y=yy, labels=labels), label.params)
    do.call(text, params)
  }


}


## --------------------  barplot widget --------------------

#' @describeIn plotwidgets A minimalistic bar plot. Use the \code{max}
#' paramter to scale the bars on several different widgets to the same value.
#' @export
wgBarplot <- function(x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, 
  labels=NULL, label.params=NULL,
  new=FALSE, aspect=NULL,
  max=NULL
  ) {
  .widgetWrapper(.barplotWidget, x, y, w, h, v, 
    col=col, border=border, new=new, aspect=aspect, max=max,
    labels=labels, label.params=label.params)
}

## the actual function for plotting bar plots
.barplotWidget <- function(x, y, w, h, v, col=NULL, border=NULL, max=NULL,
  labels=NULL, label.params=NULL) {

  x <- x - w/2
  y <- y - h/2
  n <- length(v)
  xx <- seq.int(x, x + w, length.out=n+1)

  if(is.null(max)) max <- max(v)
  yy <- y + v/max

  for(i in 1:n) {
    rect(xx[i], y, xx[i+1], yy[i], col=col[i], border=border) 
  }

  # labels in the middle of each bar
  if(!is.null(labels)) {
    xx <- (xx[1:n] + xx[2:(n+1)])/2
    yy <- (y + yy)/2
    params <- c(list(x=xx, y=yy, labels=labels), label.params)
    do.call(text, params)
  }


  return(invisible(NULL))
}


## --------------------  boxpie widget --------------------

#' @describeIn plotwidgets Rectangular pies are thought to represent
#' information better than pies. Here, the values in v correspond to areas
#' rather than angles, which makes it easier to interpret it visually.
#' @export
wgBoxpie <- function(x=0.5, y=0.5, w=1, h=1, v, 
  col=NULL, border=NA, 
  labels=NULL, label.params=NULL,
  new=FALSE, aspect=1,
  grid=3) {

  .widgetWrapper(.boxpieWidget, x, y, w, h, v, 
    col=col, grid=3, border=border, 
    labels=labels, label.params=label.params,
    new=new, aspect=aspect)

}

.boxpieWidget <- function(x, y, w, h, v, col=NULL, border=NA, grid=3,
  labels=NULL, label.params=NULL) {
  x <- x - w/2
  y <- y - h/2

  v <- grid^2 * v / sum(v)
  cv <- c(0, cumsum(v))

  left <- TRUE

  # for labels
  xt <- c()
  yt <- c()

  for(i in 1:length(v)) {
    ret <- .boxpie.calculate.polygon(cv[i], cv[i+1], grid=grid, left=left)
    xp <- ret$vec[,1] * w / grid + x
    yp <- ret$vec[,2] * h / grid + y

    polygon(xp, yp, col=col[i], border=NA)
    xt[i] <- mean(xp)
    yt[i] <- mean(yp)
      
    left <- ret$left
  }

  rect(x, y, x + w, y + h, border=border)

  # labels in the middle of each bar
  if(!is.null(labels)) {
    params <- c(list(x=xt, y=yt, labels=labels), label.params)
    do.call(text, params)
  }

  invisible(NULL)
}


## joins a rectangle (defined by x1, x2, y1, y2) with a polygon (defined by vec)
## vec is a two-column matrix with coordinates of the polygon
.boxpie.addlines <- function(vec, x, y) {
  t <- cbind(rep(x, each=2), c(y, rev(y)))
  return(rbind(t[1:2,], vec, t[3:4,]))
}

## calculates the coordinates of a polygon corresponding to the area of
## (end-start) on a surface grid x grid. The drawing happens in three
## parts:
##                                +------------+                                      
##              starting block -> |    ########|                
##                     /->        |############|                
##         full rows -+---->      |############|                
##                     \->        |############|                
##              ending block ->   |        ####|              
##                                |            |                
##                                +------------+                
##                                                                       
## the "left" parameter decides whether the polygon should be sticking to
## the left, or to the right side of the grid.
## The function returns a list with two elements:
##
## vec  -- a matrix with two columns encoding the coordinates of the
##         resulting polygon
## left -- a T/F value indicating whether the *next* polygon in row should
##         stick to the left, or to the right side of the grid
##
.boxpie.calculate.polygon <- function(start, end, left=TRUE, grid=3) {

  dx <- end - start

  # start drawing from top to bottom (first row to draw is the top-most
  # row)
  row0     <- grid - start %/% grid 
  leftover <- start %% grid 

  # vec will hold the polygon coordinates
  vec <- NULL

  ret.left <- !left

  # starting block
  if(leftover > 0) {
    startbloclen <- min(grid - leftover, dx)

    xx <- c(0, startbloclen) + leftover
    if(left) xx <- grid - rev(xx) 

    vec <- .boxpie.addlines(vec, xx, c(row0 - 1, row0))

    # if the whole polygon fits in that remainder, and if there is still
    # some place left, the empty place will be on the same side as before,
    # so the next polygon should stick to the same side as that one
    if(grid - leftover > dx) ret.left <- left

    row0 <- row0 - 1
    dx <- dx - startbloclen
  } 

  # rows completly filled
  fullrows <- dx %/% grid

  if(fullrows > 0) {
    vec <- .boxpie.addlines(vec, c(0, grid), c(row0 - fullrows, row0))
    row0 <- row0 - fullrows
  }

  # remaining bit to be filled out
  endrest  <- dx %% grid

  if(endrest > 0) {
    xx <- c(0, endrest)
    if(!left) xx <- grid - rev(xx)
    vec <- .boxpie.addlines(vec, xx, c(row0 - 1, row0))
  }

  return(list(vec=vec, left=ret.left))
}



## --------------------  ROC curve widget --------------------
#' @describeIn plotwidgets This is one of two wigets that take use a
#' different data type. \code{wgRoccurve} takes either a vector of T/F
#' values, or a list of such vectors, and draws a ROC curve for each of these
#' vectors.
#' @export
wgRoccurve <- function(x=0.5, y=0.5, w=1, h=1, v, col=NULL, border=NA, new=FALSE, fill=FALSE, lwd=1, aspect=1) {
  .widgetWrapper(.roccurvewidget, x, y, w, h, v, col=col, border=border, new=new, fill=fill, lwd=lwd, aspect=aspect)
}

.roccurvewidget <- function(x, y, w, h, v, col=NULL, border=NA, fill=FALSE, lwd=1) {

  if(!is.list(v)) {
    if(!is.vector(v)) stop("v must be a vector or a list of vectors")
    v <- list(v)
  }

  x <- x - w/2
  y <- y - h/2

  for(i in 1:length(v)) {
    r <- v[[i]]
    n <- length(r)
    cx <- cumsum(!r) / sum(!r)
    cy <- cumsum(r) / sum(r)
    if(fill) {
      xx <- c(0, cx, 1, 1) * w + x
      yy <- c(0, cy, 1, 0) * h + y
      polygon(xx, yy, col=col[i], border=col[i], lwd=lwd)
    } 

    xx <- c(0, cx, 1) * w + x
    yy <- c(0, cy, 1) * h + y
    lines(xx, yy, col=col[i], lwd=lwd)
  }

  if(!.isnn(border)) 
    rect(x, y, x + w, y + h, col=NA, border=border)

}



## --------------------  spiderplot widget --------------------

#' @describeIn plotwidgets Spider plots can illustrate multivariate data.
#' Different spikes may illustrate different variables, while different
#' lines correspond to different samples -- or vice versa. Consequently,
#' wgSpider accepts either a vector (for a single line) or a matrix (in which
#' each column will correspond to a single line on the plot). The length of
#' the vector (or the number of rows in the matrix) corresponds to the number
#' of spikes on the spider plot.
#' @export
wgSpider <- function(x=0.5, y=0.5, w=1, h=1, v, col=NULL, border=NA, new=FALSE, min=NA, max=NA,
  tick.labels=FALSE, fill=FALSE, lwd=1, aspect=1) {
  .widgetWrapper(.spiderWidget, x, y, w, h, v, 
    col=col, border=border, min=NA, max=NA,
      tick.labels=tick.labels, fill=fill, new=new, lwd=lwd, aspect=aspect)
}

## the actual spider plot
.spiderWidget <- function(x, y, w, h, v, col=NULL, border=NA, min=NA, max=NA,
  tick.labels=FALSE, fill=FALSE, lwd=1) {


  if(is.matrix(v)) {
    n <- nrow(v) 
  } else if(is.vector(v)) {
    n <- length(v)
    v <- matrix(v, ncol=1)
  } else {
    stop("Argument v must be a matrix or a numeric vector")
  }


  if(n < 3) 
    stop("for spider plots, at least 3 data points are necessary")

  if(.isnn(min)) min <- min(v, na.rm=TRUE)
  if(.isnn(max)) max <- max(v, na.rm=TRUE)

  ticks <- axisTicks(c(min, max), log=FALSE)
  min <- min(min, ticks)
  max <- max(max, ticks)

  # standardize values
  vs      <- (v - min)/(max - min)/2
  ticks.s <- (ticks - min)/(max - min)/2

  # converting 0..1 to coordinates on a circle
  a2xy <- function(a, r=1, full=FALSE) {
    t <- pi * (0.5  - 2 * a)
    list( x=x + r * w * cos(t), y=y + r * h * sin(t) )
  }

  # spokes
  spokes <- seq.int(0, 1, length.out=n+1)[-(n+1)]

  po <- a2xy(spokes, r=0.5)
  segments(x, y, po$x, po$y, col="grey")

  # draw the concentric polygons
  for(t in ticks.s) {
    po <- a2xy(spokes, t, TRUE)
    polygon(po$x, po$y, col=NA, border="grey")
  }

  for(c in 1:ncol(v)) {
    po <- a2xy(spokes, vs[,c], TRUE)
    filc <- NA
    if(fill==TRUE) filc <- col[c]
    polygon(po$x, po$y, col=filc, border=col[c], lwd=lwd)
  }

  if(tick.labels) text(x, y + ticks.s * h, as.character(ticks))

}


