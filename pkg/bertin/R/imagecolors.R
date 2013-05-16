#$HeadURL$
#$HeadURL$
#$Id$
#$Revision$
#$Date$
#$Author$

#extracted from imagem.default
#! improve support for data.frame, list

# return a matrix of colour indices, based on range
imagecolindex <-function(z, zlim= range(z[is.finite(z)]), ncolour, oldstyle=FALSE) {
#$Revision$
        #nc <- length(col)
        z <- as.matrix(z)
        if (missing(ncolour)) ncolour <- length(palette())
        if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 
            0)) 
            stop("invalid z limits")
        if (diff(zlim) == 0) 
            zlim <- if (zlim[1L] == 0) 
                c(-1, 1)
            else zlim[1L] + c(-0.4, 0.4) * abs(zlim[1L])
        z <- (z - zlim[1L])/diff(zlim)
        zi <- if (oldstyle) 
            floor((ncolour - 1) * z + 0.5)
        else floor((ncolour - 1e-05) * z + 1e-07)
        zi[zi < 0 | zi >= ncolour] <- NA
        return(zi+1) # 1 based, following R conventions
        }

# return a matrix of colour indices, based on a break table
imagecolbybreak <- function(z, breaks, ncolour)
{
#$Revision$
        z <- as.matrix(z)
       if (missing(ncolour)) ncolour <- length(palette())
       if (length(breaks) != ncolour + 1) 
            stop("must have one more break than colour")
        if (any(!is.finite(breaks))) 
            stop("breaks must all be finite")
        zi <- .C("bincode", as.double(z), length(z), as.double(breaks), 
            length(breaks), code = integer(length(z)), (TRUE), 
            (TRUE), nok = TRUE, NAOK = TRUE, DUP = FALSE, PACKAGE = "base")$code - 
            1
          return(zi)
    }
    