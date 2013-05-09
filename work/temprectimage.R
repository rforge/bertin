rect <-function (xleft, ybottom, xright, ytop, density = NULL, angle = 45, 
    col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), 
    ...) 
{
    if (is.numeric(density) && all(is.na(density) | density < 
        0)) 
        density <- NULL
    if (!is.null(density) && !is.null(angle)) {
        if (is.logical(border) && !is.na(border)) {
            if (border) 
                border <- col
            else border <- NA
        }
        n <- range(length(xleft), length(xright), length(ybottom), 
            length(ytop))
        if (n[1L] == 0) 
            stop("invalid rectangle specification")
        n <- n[2L]
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1L]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1L]
        polygon(x, y, col = col, border = border, lty = lty, 
            lwd = lwd, density = density, angle = angle, ...)
    }
    else .Internal(rect(as.double(xleft), as.double(ybottom), 
        as.double(xright), as.double(ytop), col = col, border = border, 
        lty = lty, lwd = lwd, ...))
}
<environment: namespace:graphics>
> methods(image)
[1] image.bertin  image.default
image.default <-function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, zlim = range(z[is.finite(z)]), 
    xlim = range(x), ylim = range(y), col = heat.colors(12), 
    add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab, breaks, 
    oldstyle = FALSE, useRaster = FALSE, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                if (is.null(dim(x))) 
                  stop("argument must be matrix-like")
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
            if (missing(xlab)) 
                xlab <- ""
            if (missing(ylab)) 
                ylab <- ""
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        xn <- deparse(substitute(x))
        if (missing(xlab)) 
            xlab <- paste(xn, "x", sep = "$")
        if (missing(ylab)) 
            ylab <- paste(xn, "y", sep = "$")
        y <- x$y
        x <- x$x
    }
    else {
        if (missing(xlab)) 
            xlab <- if (missing(x)) 
                ""
            else deparse(substitute(x))
        if (missing(ylab)) 
            ylab <- if (missing(y)) 
                ""
            else deparse(substitute(y))
    }
    if (any(!is.finite(x)) || any(!is.finite(y))) 
        stop("'x' and 'y' values must be finite and non-missing")
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z)) 
        stop("'z' must be a matrix")
    if (length(x) > 1 && length(x) == nrow(z)) {
        dx <- 0.5 * diff(x)
        x <- c(x[1L] - dx[1L], x[-length(x)] + dx, x[length(x)] + 
            dx[length(x) - 1])
    }
    if (length(y) > 1 && length(y) == ncol(z)) {
        dy <- 0.5 * diff(y)
        y <- c(y[1L] - dy[1L], y[-length(y)] + dy, y[length(y)] + 
            dy[length(y) - 1])
    }
    if (missing(breaks)) {
        nc <- length(col)
        if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 
            0)) 
            stop("invalid z limits")
        if (diff(zlim) == 0) 
            zlim <- if (zlim[1L] == 0) 
                c(-1, 1)
            else zlim[1L] + c(-0.4, 0.4) * abs(zlim[1L])
        z <- (z - zlim[1L])/diff(zlim)
        zi <- if (oldstyle) 
            floor((nc - 1) * z + 0.5)
        else floor((nc - 1e-05) * z + 1e-07)
        zi[zi < 0 | zi >= nc] <- NA
    }
    else {
        if (length(breaks) != length(col) + 1) 
            stop("must have one more break than colour")
        if (any(!is.finite(breaks))) 
            stop("breaks must all be finite")
        zi <- .C("bincode", as.double(z), length(z), as.double(breaks), 
            length(breaks), code = integer(length(z)), (TRUE), 
            (TRUE), nok = TRUE, NAOK = TRUE, DUP = FALSE, PACKAGE = "base")$code - 
            1
    }
    if (!add) 
        plot(NA, NA, xlim = xlim, ylim = ylim, type = "n", xaxs = xaxs, 
            yaxs = yaxs, xlab = xlab, ylab = ylab, ...)
    if (length(x) <= 1) 
        x <- par("usr")[1L:2]
    if (length(y) <= 1) 
        y <- par("usr")[3:4]
    if (length(x) != nrow(z) + 1 || length(y) != ncol(z) + 1) 
        stop("dimensions of z are not length(x)(-1) times length(y)(-1)")
    if (useRaster) {
        dx <- diff(x)
        dy <- diff(y)
        if ((length(dx) && !isTRUE(all.equal(dx, rep(dx[1], length(dx))))) || 
            (length(dy) && !isTRUE(all.equal(dy, rep(dy[1], length(dy)))))) 
            stop("useRaster=TRUE can only be used with a regular grid")
        if (!is.character(col)) {
            p <- palette()
            pl <- length(p)
            col <- as.integer(col)
            col[col < 1L] <- NA_integer_
            col <- p[((col - 1L)%%pl) + 1L]
        }
        zc <- col[zi + 1L]
        dim(zc) <- dim(z)
        zc <- t(zc)[ncol(zc):1L, ]
        rasterImage(as.raster(zc), min(x), min(y), max(x), max(y), 
            interpolate = FALSE)
    }
    else .Internal(image(as.double(x), as.double(y), as.integer(zi), 
        col))
}
<environment: namespace:graphics>
> 