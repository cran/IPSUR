# utility functions

# delete "main" argument from Hist



# Modified wrapper function for histograms

Hist <- function(x, scale=c("frequency", "percent", "density"), ...){
    xlab <- deparse(substitute(x))
    x <- na.omit(x)
    scale <- match.arg(scale)
    if (scale == "frequency") hist(x, xlab=xlab, ...)
###########
    else if (scale == "density") hist(x, freq=FALSE, xlab=xlab, ...)
###########
    else {
        n <- length(x)
###########
        hist(x, axes=FALSE, xlab=xlab, ylab="Percent", ...)
###########
        axis(1)
        max <- ceiling(10*par("usr")[4]/n)
        at <- if (max <= 3) (0:(2*max))/20
                else (0:max)/10
        axis(2, at=at*n, labels=at*100)
        }
    box()
    abline(h=0, col="gray")
    invisible(NULL)
    }



