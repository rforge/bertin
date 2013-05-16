whatis <-
# inspect an arbitrary object, and report object type information
function (ob) {
	obname<-deparse(substitute(ob))
	cat(obname)
	cat(" type=",typeof(ob),sep="")
	cat(" mode=",mode(ob),sep="")
	xx <- class(ob); if (!is.null(xx)) cat(paste(" class:",class(ob)))
	cat(paste(" length=",length(ob)))
	xx <- dim(ob); if (!is.null(xx)) cat(" dim=",xx)
	xx <- names(attributes(ob))
		if (!is.null(xx)) 
			cat("\n"," names(attributes(", obname, "))=",xx,xxsep=" ")
	xx <- dimnames(ob)
	if (!is.null(xx)) { 
		xx <- names(xx)
		if (!is.null(xx)) 
			cat("\n"," names(dimnames(",obname,"))=",xx)
	}
	cat("\n")
	if (typeof(ob)=="expression") {
		cat("expression components: ");str(as.list(ob[[1]]))
	}
} # whatis
