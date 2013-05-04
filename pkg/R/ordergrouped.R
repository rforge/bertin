#$Id: ordergrouped.R 45 2011-09-23 20:12:00Z gsawitzki $
ordergrouped <- function(..., group, na.last = TRUE, decreasing = FALSE){
	if (missing(group))
	{order(..., na.last = na.last, decreasing = decreasing)} else {
		ox <- order(..., na.last = na.last, decreasing = decreasing)
		ogro <- order(group[ox])
		ox[ogro]
	}
}
