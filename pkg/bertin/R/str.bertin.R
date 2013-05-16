str.bertin <- function(object, ...)
{
	
	cat("'bertin':	Still under construction. Data structure may change.","\n",sep="")
    # ## Method to 'str' for  'data.frame' objects
    # if(! is.data.frame(object)) {
	# warning("str.data.frame() called with non-data.frame -- coercing to one.")
	# object <- data.frame(object)
    # }

cat("Main tile: ", attr(object,"main"),"\n",sep=" ")

if (is.matrix(object)) {
	cat("Dim:", dim(object),"\n",sep=" ")
	vo <- attr(object, "var.orientation")
	cat("Variables ",vo,"\n",sep=" ")

	if (vo =="byrow") {cat("Variables: ", rownames(object),"\n",sep=" ")} else {
		cat("variables: ", colnames(object),"\n",sep=" ")}
	}

	
cat("\n","More information: zcol is a cache of the currently transform of the variable values.","\n",sep=" ")

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
