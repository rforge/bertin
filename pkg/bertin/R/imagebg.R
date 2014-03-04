# make a neutral background for undefined slots.
# Note: colour should be adjusted to give a neutral grey when compared to the data points.
imageBG <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, zlim = range(z[is.finite(z)]), 
    xlim = range(x), ylim = range(y), col = "#80808080", add=TRUE, ...)
{
	zNA <- ifelse(is.na(z[[]]),1,NA) #??
	dim(zNA) <- dim(z)
	image(x,y,zNA,zlim,xlim,ylim,col,add,...)
}
