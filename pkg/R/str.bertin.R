str.bertin <- function(object, ...)
{
	
	cat("'bertin':	Still under construction.","\n",sep="")
    # ## Method to 'str' for  'data.frame' objects
    # if(! is.data.frame(object)) {
	# warning("str.data.frame() called with non-data.frame -- coercing to one.")
	# object <- data.frame(object)
    # }

    ## Show further classes // Assume that they do NOT have an own Method --
    ## not quite perfect ! (.Class = 'remaining classes', starting with current)
    cl <- oldClass(object); cl <- cl[cl != "data.frame"]  #- not THIS class
    if(0 < length(cl)) cat("Classes", paste(sQuote(cl), collapse=", "), "and ")

    # cat("'data.frame':	", nrow(object), " obs. of  ",
	# (p <- length(object)), " variable", if(p != 1)"s", if(p > 0)":",
	# "\n",sep="")

    ## calling next method, usually  str.default:
    if(length(l <- list(...)) && any("give.length" == names(l)))
	invisible(NextMethod("str", ...))
    else invisible(NextMethod("str", give.length=FALSE,...))
}
