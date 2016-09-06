.plotWidgetDefaultPalette <- function (n = NULL, theme="default", transparent = NULL) {

    pal <- .pals()[theme] 

    if(is.null(transparent)) transparent <- switch(theme, light="99", neon="DD", haze="", "CC")

    pal <- unlist(strsplit(pal, "  ?"))
    pal <- paste("#", pal, transparent, sep = "")
    if (!is.null(n)) {
        if (n > length(pal)) {
            pal <- rep(pal, ceiling(n/length(pal)))
        } else {
            pal <- pal[1:n]
        }
    }
    return(pal)
}


## return a gradient palette
.gradientpal <- function(n=NULL, set="bluewhitered", transparent="99") {
  if(is.null(n)) n <- 3

  col <- switch(set,
    bwr=c("blue", "white", "red"),
    rwb=c("red", "white", "blue"),
    ckp=c("cyan", "black", "purple"),
    rwb=c("purple", "black", "cyan")
    )

  pal <- colorRampPalette(col)(n)
  paste0(pal, transparent)
}


## convert a string-encoded palette to a vector of strings
.palfromstring <- function(pal, transparent="") {
  pal <- unlist(strsplit(pal, "  ?"))
  paste("#", pal, transparent, sep = "")
}


.pals <- function() c(
      default="E69F00 56B4E9 009E73 F0E442 0072B2 D55E00 CC79A7 660099 996600 990066 666633 aa3366 5B4E85 FF6A5C ADAEA3 A0A376 FF8040 A2D6DA DA9CA5",
      # safe is based on
      # http://www.nature.com/nmeth/journal/v8/n6/full/nmeth.1618.html?WT.ec_id=NMETH-201106
      safe="000000 E69F00 56B4E9 009E73 F0E442 0072B2 D55E00 CC79A7",
      neon="A020F0 0FFFFF FFFF0F FE6A24 00FF58 FC0001 00FF99 F0FE2D FF5C34",
      pastel="87CEEB 32CD32 BA55D3 F08080 4682B4 9ACD32 40E0D0 FF69B4 F0E68C D2B48C 8FBC8B 6495ED DDA0DD 5F9EA0 FFDAB9 FFA07A",
      haze="E6E6FA E0FFFF FFF0F5 FFDAB9 FFFACD FFE4E1 F0FFF0 F0F8FF F5F5F5 FAEBD7",
      darkhaze="5050A0 3CB4B4 BA3E67 A56B37 ACA13A B4483C 3EBA3E 3E80BA 7A7A7A A27B47",
      grey="C8C8C8 B2B2B2 9C9C9C 868686 707070 565656 424242 2E2E2E 232323",
      alphabet="F0A3FF 0075DC 993F00 4C005C 191919 005C31 2BCE48 FFCC99 808080 94FFB5 8F7C00 9DCC00 C20088 003380 FFA405 FFA8BB 426600 FF0010 5EF1F2 00998F E0FF66 740AFF 990000 FFFF80 FF5005",
      few="4D4D4D 5DA5DA FAA43A 60BD68 F17CB0 B2912F B276B2 DECF3F F15854",
      # based on Zeileis, Hornik and Murrell (2009): Escaping RGBland:
      # Selecting Colors for Statistical Graphics // Computational Statistics
      # & Data Analysis Volume 53, Issue 9, 1 July 2009, Pages 3259-3270
      # after
      # http://graphicdesign.stackexchange.com/questions/3682/where-can-i-find-a-large-palette-set-of-contrasting-colors-for-coloring-many-d
      zeileis="023FA5 7D87B9 BEC1D4 D6BCC0 BB7784 8E063B 4A6FE3 8595E1 B5BBE3 E6AFB9 E07B91 D33F6A 11C638 8DD593 C6DEC7 EAD3C6 F0B98D EF9708 0FCFC0 9CDED6 D5EAE7 F3E1EB F6C4E1 F79CD4",
      vizi="D24775 6DA742 8064D8 C59D31 C05DB6 629F6D 767AC4 C36942 5CA6D3 BA6C85"
      )

#' Return all or selected color palettes
#'
#' Return all or selected color palettes from the plotwidget palette set
#'
#' The plotwidgets package contains a number of predefined palettes,
#' different from those in RColorBrewer.
#'
#' \itemize{
#'  \item \code{default}: a standard, relatively safe (see below) palette 
#'  \item \code{safe}:  safe for color blind persons, based
#'                on Wang B. "Points of view: Color blindeness", Nature Methods 8, 441(2011)
#'  \item \code{neon}: a bright palette suitable for drawing on dark backgrounds
#'  \item \code{pastel}: a dimmed pastel palette
#'  \item \code{haze}: very delicate pastel colors
#'  \item \code{dark}: same hues as haze, but much darker
#'  \item \code{grey}: different shades of grey
#'  \item \code{alphabet}: based on "A colour alphabet..." by Paul Green-Armytage
#'  \item \code{few}: a palette based on Stephen Few's book
#'  \item \code{zeileis}: based on Zeileis et al. 2009
#'  \item \code{vizi}: automatically generated palette
#' }
#'
#' \if{html}{\figure{palettes.svg}{options: width=800 alt="List of palettes"}}
#' \if{latex}{\figure{palettes.pdf}{options: width=5.5in}}
#' 
#' You can get all the names of palettes with names(plotPals()), and
#' showcase them with showPalettes(). Furthermore, you can use the
#' \code{pal} parameter to \code{\link{plotwidgetGallery}} to see how this
#' palette looks like with different plot widgets.
#' 
#' @param pal Name of the palette(s) to return
#' @param alpha Control transparency - set alpha channel to alpha (0 -
#'        fully transparent, 1 - fully opaque)
#' @return Either a list of palettes, or (if only one palette was selected)
#'         a character vector with colors
#' @seealso \code{\link{col2rgb.2}}, \code{\link{rgb2col}},
#'          \code{\link{hsl2col}}, \code{\link{col2hsl}},
#'          \code{\link{modCol}}, \code{\link{modhueCol}}, \code{\link{darkenCol}}, \code{\link{saturateCol}}
#' @examples
#' safe <- plotPals("safe") # colorblind-safe palette
#' plotwidgetGallery(pal=safe)
#' @export
plotPals <- function(pal=NULL, alpha=1) {
  p <- .pals()
  alpha <- sprintf("%02X", round(alpha * 255))
  p <- lapply(p, .palfromstring, transparent=alpha)
  if(!is.null(pal)) {
    if(any(!pal %in% names(p))) {
      error <- paste0("No such palette(s): ", pal[ !pal %in% names(p) ], collapse=", ")
      stop(error)
    }
    p <- p[pal]
  }
  if(length(p) == 1) p <- p[[1]]
  p
}


#' Demonstrate selected palettes
#'
#' Show a plot demonstrating all colors in the provided palettes
#' 
#' @param pal Either a character vector of colors or a list of character
#'            vectors
#' @param numbers On each of the colors, show a number
#' @examples
#' ## Show all palettes in plotwidget
#' showPals(plotPals())
#'
#' ## Show just a few colors
#' showPals(c("red", "green", "blue"))
#' @export
showPals <- function(pal=NULL, numbers=T) {

  oldpar <- par(mar=rep(0.5, 4))
  on.exit(par(oldpar))

  if(is.null(pal)) pal <- plotPals()

  if(!is.list(pal)) {
    pal <- list(pal)
  }

  npal <- length(pal)
  nmax <- max(sapply(pal, length))

  h <- min(0.8/npal, 0.1)
  w <- 0.8 / nmax

  plot.new()

  for(i in 1:npal) {
    nn <- length(pal[[i]])
    x <- w * (nn + 1) / 2
    y <- h/2 + (i-1)/npal
    wgRug(x=x, y=1-y, w=w * nn, h=h, v=rep(1, nn), col=pal[[i]])
    x <- (1:nn)*w
    if(numbers)
      text(x, 1-y, 1:nn, col=contrastcol(pal[[i]], alpha=1))
  }

  if(!is.null(names(pal))) {
    x <- w * (sapply(pal, length) + 1)
    y <- h/2 + seq.int(0, (npal-1)/npal, length.out=npal)
    text(x, 1-y, names(pal), pos=4)
  }
}




#' Convert colors from and to RGB and HSL formats
#' 
#' Convert colors from and to RGB and HSL formats
#'
#' These functions convert between RGB and HSL color spaces, and character
#' vectors which contain color names or hash-encoded RGB values ("#FFCC00").
#' 
#' All functions support an alpha channel. For example,
#' unlike the grDevices::col2rgb, col2rgb.2 returns a matrix with four
#' rows: three for R, G and B channels and one for the alpha channel.
#'
#' @param col a character vector with colors to convert (palette)
#' @param rgb a numeric matrix with three or four rows (red, green, blue and alpha)
#' @param hsl a numeric matrix with three or four rows (hue, saturation, luminosity and alpha)
#' @return col2rgb.2 and col2hsl return a four-row matrix. rgb2col and hsl2col return a character
#'         vector.
#' @seealso \code{\link{modCol}}, \code{\link{modhueCol}}, \code{\link{darkenCol}}, \code{\link{saturateCol}}
#' @examples
#' haze <- plotPals("haze")
#' col2rgb(haze)
#' col2hsl(haze)
#' @name colorConversions
NULL

#' @describeIn colorConversions Convert a character vector of color names
#' (palette) to a matrix with RGB values
#' @export
col2rgb.2 <- function(col) {

  alphas <- rep(255, length(col))

  pat <- "^#([[:xdigit:]]{6})([[:xdigit:]]{2})"
  sel <- grep(pat, col)

  alphas[sel] <- strtoi( paste0("0X", gsub(pat, "\\2", col[sel]))) # / 255
  if(all(alphas == 255)) {
    ret <- col2rgb(col)
  } else {
    ret <- rbind(col2rgb(col), alpha=alphas)
  }
  ret
}

#' @describeIn colorConversions Convert a character vector of color names (palette) to a matrix with HSL values
#' @export
col2hsl <- function(col) {
  rgb2hsl(col2rgb.2(col))
}


#' @describeIn colorConversions Convert hsl matrix (3 or 4 row) to character vector of color names
#' @export
hsl2col <- function(hsl) {
  rgb2col(hsl2rgb(hsl))
}



#' @describeIn colorConversions Convert rgb matrix (3 or 4 row) to character vector of color names
#' @export
rgb2col <- function(rgb) {

  rgb <- round(rgb)
  if(nrow(rgb) == 4) {
    #rgb[4,] <- rgb[4,] * 255
    rgb <- apply(rgb, 2, function(x) sprintf("#%02X%02X%02X%02X", x[1], x[2], x[3], x[4]))
  } else {
    rgb <- apply(rgb, 2, function(x) sprintf("#%02X%02X%02X", x[1], x[2], x[3]))
  }

  rgb
}

#' @describeIn colorConversions Convert a 3- or 4-row matrix of RGB(A) values to a matrix of HSL(A) values
#' @export
rgb2hsl <- function(rgb) {

  if(nrow(rgb) == 4) {
    alpha <- rgb[4,,drop=F]
    rgb   <- rgb[-4,,drop=F] 
  } else {
    alpha <- NULL
  }

  rgb <- rgb / 255

  mins <- apply(rgb, 2, min)
  maxs <- apply(rgb, 2, max)
  d <- maxs - mins
  L <- (maxs+mins)/2

  S <- d/(1 - abs(2*L - 1))
  sel <- d == 0


  S[sel] <- 0
  

  wmax <- apply(rgb, 2, which.max)
  
  H <- L
  HR <- (rgb[2,] - rgb[3,])/(maxs - mins)
  HG <- 2 + (rgb[3,] - rgb[1,])/(maxs - mins)
  HB <- 4 + (rgb[1,] - rgb[2,])/(maxs - mins)

  sel <- wmax == 1
  H[sel] <- HR[sel]
  sel <- wmax == 2
  H[sel] <- HG[sel]
  sel <- wmax == 3
  H[sel] <- HB[sel]

  H <- (H * 60) %% 360

  H[ mins == maxs ] <- 0

  ret <- rbind(H=H, S=S, L=L, alpha=alpha)
  return(ret)
}

#' @describeIn colorConversions Convert a matrix of HSL values into a matrix of RGB values
#' @export
hsl2rgb <- function(hsl) {

  if(nrow(hsl) == 4) {
    alpha <- hsl[4,,drop=F]
    hsl   <- hsl[-4,,drop=F]
  } else {
    alpha <- NULL
  }

  H <- hsl[1,]
  S <- hsl[2,]
  L <- hsl[3,]

  C <- (1 - abs(2*L - 1)) * S
  X <- C * (1- abs( ((H/60) %% 2) - 1))
  m <- L - C/2

  rgb <- matrix(0, ncol=ncol(hsl), nrow=3)
  rownames(rgb) <- c("R", "G", "B")

  iX <- c(2, 1, 3, 2, 1, 3)
  iC <- c(1, 2, 2, 3, 3, 1)

  for(i in 1:6) {
    sel <- 60 * (i - 1) <= H & H < 60 * i
    kX <- iX[i]
    kC <- iC[i]
    rgb[kX,sel] <- X[sel]
    rgb[kC,sel] <- C[sel]
  }

  rgb <- rgb + rep(m, each=3)

  rgb <- round(rgb * 255)
  if(!is.null(alpha)) 
    rgb <- rbind(rgb, alpha=alpha)

  rgb
}


luminosity.adj <- function(hsl, by=0) {

  #hsl[3,] <- hsl[3,] * (1 + by)

  if(by > 0) { # lighten
    hsl[3,] <- 1 - (1 - by) * (1 - hsl[3,])
  } else { # darken
    hsl[3,] <- hsl[3,] * (1 + by)
  }

  hsl
}

saturation.adj <- function(hsl, by=0) {
  if(by > 0) { # saturate
    hsl[2,] <- 1 - (1 - by) * (1 - hsl[2,])
  } else { # desaturate
    hsl[2,] <- hsl[2,] * (1 + by)
  }

  hsl
}

hue.adj <- function(hsl, by=0) {
  hsl[1,] <- hsl[1,] + by
  hsl[1,] <- hsl[1,] %% 360
  hsl
}


#' Modify colors 
#'
#' Modify colors by shading, saturating and changing hue
#'
#' This function use the HSL (hue, saturation, luminosity) scheme to modify
#' colors in a palette. 
#' 
#' modCol is just a wrapper for the other three functions allowing to
#' modify three parameters in one go.
#'
#' saturateCol, darkenCol and modhueCol modify the saturation, luminosity
#' and hue in the HSL color model.
#'
#' contrastcol() returns black for each light color (with L > 0.5) and
#' white for each dark color (with L < 0.5).
#'
#' @param col a character vector of colors (palette) to modify -- a character vector
#' @param darken Use negative values to lighten, and positive to darken.
#' @param saturate Use negative values to desaturate, and positive to saturate
#' @param modhue Change the hue by a number of degrees (0-360)
#' @param by parameter for the saturateCol, darkenCol and modhueCol functions
#' @param alpha alpha value (from 0, transparent, to 255, fully opaque)
#' @return a character vector containing the modified palette
#' @examples 
#' plot.new()
#' ## Loop over a few saturation / lightess values
#' par(usr=c(-0.5, 0.5, -0.5, 0.5))
#' v <- c(10, 9, 19, 9, 15, 5)
#' pal <- plotPals("zeileis")
#' for(sat in seq.int(-0.4, 0.4, length.out=5)) {
#'   for(lgh in seq.int(-0.4, 0.4, length.out=5)) {
#'     cols <- saturateCol(darkenCol(pal, by=sat), by=lgh)
#'     wgPlanets(x=sat, y=lgh, w=0.16, h=0.16, v=v, col=cols)
#'   }
#' }
#' axis(1)
#' axis(2)
#' title(xlab="Darkness (L) by=", ylab="Saturation (S) by=")
#' 
#' ## Now loop over hues
#' a2xy <- function(a, r=1, full=FALSE) {
#'   t <- pi/2 - 2 * pi * a / 360
#'   list( x=r * cos(t), y=r * sin(t) )
#' }
#' 
#' plot.new()
#' par(usr=c(-1,1,-1,1))
#' hues <- seq(0, 360, by=30)
#' pos <- a2xy(hues, r=0.75)
#' for(i in 1:length(hues)) {
#'   cols <- modhueCol(pal, by=hues[i])
#'   wgPlanets(x=pos$x[i], y=pos$y[i], w=0.5, h=0.5, v=v, col=cols)
#' }
#'
#' pos <- a2xy(hues[-1], r=0.4)
#' text(pos$x, pos$y, hues[-1])
#' @export
modCol <- function(col, darken=0, saturate=0, modhue=0) {
  modhueCol(saturateCol(darkenCol(col, by=darken), by=saturate), by=modhue)
}

#' @describeIn modCol Change the saturation of a color or palette by a fraction of "by" 
#' @export
saturateCol <- function(col, by=0) {
  hsl <- rgb2hsl(col2rgb.2(col))
  hsl <- saturation.adj(hsl, by=by)
  rgb2col(hsl2rgb(hsl))
}


#' @describeIn modCol Modify the darkness of a color or palette (positve \code{by} - darken, negative \code{by} -- lighten)
#' @export
darkenCol <- function(col, by=0) {
  hsl <- rgb2hsl(col2rgb.2(col))
  hsl <- luminosity.adj(hsl, by=-by)
  rgb2col(hsl2rgb(hsl))
}


#' @describeIn modCol Modify the hue of a character vector of colors by \code{by} degrees
#' @export
modhueCol <- function(col, by=0) {
  hsl <- rgb2hsl(col2rgb.2(col))
  hsl <- hue.adj(hsl, by=by)
  rgb2col(hsl2rgb(hsl))
}

#' @describeIn modCol Return white for dark colors, return black for light colors
#' @export
contrastcol <- function(col, alpha=NULL) {

  hsl <- rgb2hsl(col2rgb.2(col))

  sel <- hsl[3,] < 0.5
  hsl[3,sel] <- 1
  hsl[3,!sel] <- 0

  if(!is.null(alpha)) 
    hsl <- rbind(hsl[1:3,], round(alpha * 255))
    #hsl[4,] <- round(alpha * 255)

  rgb2col(hsl2rgb(hsl))
}

