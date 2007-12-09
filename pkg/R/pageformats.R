a4 <- c(9.952913, 5.927717) # a4 paper size  par(pin=a4) ## could go to pdf 11.192913  7.767717
pdfa4 <- function(...) pdf(height=11.192913, width=7.767717, ...)
# pdfa4 <- function(...) pdf(height=9.952913, width=5.927717, ...)
a4w <- c(5.927717,9.952913) # a4 paper size  par(pin=a4) ## could go to pdf
pdfa4w <- function(...) pdf(width=9.952913, height=5.927717, ...)
