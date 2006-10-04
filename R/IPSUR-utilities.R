# last modified 23 Feb 06 by J. Fox + slight changes 12 Aug 04 by Ph. Grosjean
                                                                                       
# utility functions


# 7294
    # Modified wrapper function for histograms

Hist <- function(x, scale=c("frequency", "percent", "density"), ...){
    xlab <- deparse(substitute(x))
    x <- na.omit(x)
    scale <- match.arg(scale)
    if (scale == "frequency") hist(x, xlab=xlab, ...)
    else if (scale == "density") hist(x, freq=FALSE, xlab=xlab, ...)
    else {
        n <- length(x)
        hist(x, axes=FALSE, xlab=xlab, ylab="Percent", ...)
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
    
#################################################################################################################
#################################################################################################################
#
#
# # defmacro is as in Rcmdr
#    # functions for building dialog boxes
#
## the following function is slightly modified from Thomas Lumley,
##   "Programmer's Niche: Macros in R," R-News, Sept. 2001, Vol. 1, No. 3, pp.11-13.
#defmacro <- function(..., expr){
#    expr <- substitute(expr)
#    len <- length(expr)
#    expr[3:(len+1)] <- expr[2:len]
#    ## delete "macro" variables starting in ..
#    expr[[2]] <- quote(on.exit(remove(list=objects(pattern="^\\.\\.", all.names=TRUE))))
#    a <- substitute(list(...))[-1]
#    ## process the argument list
#    nn <- names(a)
#    if (is.null(nn)) nn <- rep("", length(a))
#    for (i in seq(length=length(a))){
#        if (nn[i] == "") {
#            nn[i] <- paste(a[[i]])
#            msg <- paste(a[[i]], gettext("not supplied", domain="R-Rcmdr"))
#            a[[i]] <- substitute(stop(foo), list(foo = msg))
#            }
#        }
#    names(a) <- nn
#    a <- as.list(a)
#    ff <- eval(substitute(
#        function(){
#            tmp <- substitute(body)
#            eval(tmp, parent.frame())
#            },
#        list(body = expr)))
#    ## add the argument list
#    formals(ff) <- a
#    ## create a fake source attribute
#    mm <- match.call()
#    mm$expr <- NULL
#    mm[[1]] <- as.name("macro")
#    expr[[2]] <- NULL # get "local" variable removal out of source
#    attr(ff, "source") <- c(deparse(mm), deparse(expr))
#    ## return the macro
#    ff
#    }
#
##################################################################################################
## Added datan and sim arguments

#OKCancelHelp <- defmacro(window=top, helpSubject=NULL, datan=FALSE, model=FALSE, sim=FALSE,
#    expr={
#        buttonsFrame <- tkframe(window, borderwidth=5)
#        OKbutton <- tkbutton(buttonsFrame, text=gettextRcmdr("OK"), fg="darkgreen", width="12", command=onOK, default="active",
#            borderwidth=3)
#        onCancel <- function() {
#            if (datan) putRcmdr("datasetNumber", getRcmdr("datasetNumber") - 1)
#            if (model) putRcmdr("modelNumber", getRcmdr("modelNumber") - 1)
#            if (sim) putRcmdr("simsetNumber", getRcmdr("simsetNumber") - 1)
#            if (GrabFocus()) tkgrab.release(window)
#            tkdestroy(window)
#            tkfocus(CommanderWindow())
#            }
#        cancelButton <- tkbutton(buttonsFrame, text=gettextRcmdr("Cancel"), fg="red", width="12", command=onCancel, borderwidth=3)
#        if (!is.null(helpSubject)){
#            onHelp <- function() {
#                if (GrabFocus() && .Platform$OS.type != "windows") tkgrab.release(window)
#                if (as.numeric(R.Version()$major) >= 2) print(help(helpSubject))
#                else help(helpSubject)
#                }
#            helpButton <- tkbutton(buttonsFrame, text=gettextRcmdr("Help"), width="12", command=onHelp, borderwidth=3)
#            }
#        tkgrid(OKbutton, tklabel(buttonsFrame, text="  "), cancelButton, tklabel(buttonsFrame, text="            "),
#            if (!is.null(helpSubject)) helpButton, sticky="w")
#        })
#
#


######################################################################################
######################################################################################



browseManual <- function() {
    browseURL(paste(file.path(.path.package(package="IPSUR")[1], "doc"),
        "/Getting-Started-with-the-Rcmdr.pdf", sep=""))
    }





####################################################################

listMultiLevelFactors <- function(dataSet=ActiveDataSet()){
    factors <- listFactors(dataSet)
    if(length(factors) == 0) return(NULL)
    factors[sapply(factors, function(.x)
        2 < length(levels(eval(parse(text=.x), envir=eval(parse(text=dataSet),
            envir=.GlobalEnv)))))]
    }
    
    
checkMultiLevelFactors <- function(n=1){
    if (length(MultiLevelFactors()) < n){
        if (n > 1)
            Message(message=sprintf(gettextRcmdr("There fewer than %d multi-level factors in the active data set."), n),
                    type="error")
        else Message(message=gettextRcmdr("There are no multi-level factors in the active data set."),
                    type="error")
        tkfocus(CommanderWindow())
        FALSE
        }
    else TRUE
    }

MultiLevelFactors <- function(names){
    if (missing(names)) getRcmdr("multiLevelFactors")
    else putRcmdr("multiLevelFactors", names)
    }
    
multiLevelFactorsP <- function(n=1) activeDataSetP() && length(listMultiLevelFactors()) >= n
