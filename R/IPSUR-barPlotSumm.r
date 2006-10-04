
  barPlotSumTable <- function(){
    require("abind")
    env <- environment()
    initializeDialog(title=gettextRcmdr("Bar Graph for Summarized Data"))
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)

    setUpTable <- function(...){
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        make.col.names <- "tklabel(.tableFrame, text='')"
        for (j in 1:ncols) {
            col.varname <- paste(".colname.", j, sep="")
            assign(col.varname, tclVar(paste("Response ", j, sep="")), envir=env)
            make.col.names <- paste(make.col.names, ", ", "tkentry(.tableFrame, width='10', textvariable=",
                    col.varname, ")", sep="")
            }
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows){
            varname <- paste(".tab.", i, ".1", sep="")
            assign(varname, tclVar("") , envir=env)
            row.varname <- paste(".rowname.", i, sep="")
            assign(row.varname, tclVar(paste("Group ", i, sep="")), envir=env)
            make.row <- paste("tkentry(.tableFrame, width='10', textvariable=",
                row.varname, ")", sep="")
            make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='10', textvariable=",
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar(""), envir=env)
                make.row <- paste(make.row, ", ", "tkentry(.tableFrame, width='10', textvariable=",
                    varname, ")", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
            }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
        }
    rowColFrame <- tkframe(top)
    rowsValue <- tclVar("2")
    rowsSlider <- tkscale(rowColFrame, from=1, to=10, showvalue=FALSE, variable=rowsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- tklabel(rowColFrame, textvariable=rowsValue, width=2, justify="right")
    colsValue <- tclVar("2")
    colsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=colsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    colsShow <- tklabel(rowColFrame, textvariable=colsValue, width=2, justify="right")

    onOK <- function(){

     ####################################################
     # table setup
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        cell <- 0
        counts <- rep(NA, nrows*ncols)
        row.names <- rep("", nrows)
        col.names <- rep("", ncols)
        for (i in 1:nrows) row.names[i] <-
            eval(parse(text=paste("tclvalue(", paste(".rowname.", i, sep=""),")", sep="")))
        for (j in 1:ncols) col.names[j] <-
            eval(parse(text=paste("tclvalue(", paste(".colname.", j, sep=""),")", sep="")))
        for (i in 1:nrows){
            for (j in 1:ncols){
                cell <- cell+1
                varname <- paste(".tab.", i, ".", j, sep="")
                counts[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                }
            }
        counts <- na.omit(counts)
        if (length(counts) != nrows*ncols){
            errorCondition(recall=barPlotSumTable, message=sprintf(gettextRcmdr("Number of valid entries (%d)\nnot equal to number of rows (%d) * number of columns (%d)."), length(counts), nrows, ncols))
            return()
            }
        if (length(unique(row.names)) != nrows){
            errorCondition(recall=barPlotSumTable, message=gettextRcmdr("Row names are not unique."))
            return()
            }
        if (length(unique(col.names)) != ncols){
            errorCondition(recall=barPlotSumTable, message=gettextRcmdr("Column names are not unique."))
            return()
            }

        closeDialog()
        command <- paste("matrix(c(", paste(counts, collapse=","), "), ", nrows, ", ", ncols,
            ", byrow=TRUE)", sep="")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Table <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", row.names, "'", sep=""), collapse=", "), ")", sep="")
        justDoIt(paste("rownames(.Table) <- ", command, sep=""))
        logger(paste("rownames(.Table) <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", col.names, "'", sep=""), collapse=", "), ")", sep="")
        justDoIt(paste("colnames(.Table) <- ", command, sep=""))
        logger(paste("colnames(.Table) <- ", command, sep=""))
        #############################################################

        ############################################################
        title <- tclvalue(titleVariable)
        leg <- tclvalue(legendVariable) == "1"
        prop <- tclvalue(propVariable) == "1"
        besid <- tclvalue(typeVariable) == "beside"

        if (tclvalue(coloVariable) == 1){
            colcomm <- paste(', col=rainbow(', nrows, '))', sep="")
        } else {
            colcomm <- ', col=NULL)'
        }
        ############################################################

        if (prop){

            if ( nrows == 1) {
            command <- paste('barplot(prop.table(.Table), main="', title,
                    '", ylab="Relative Frequency"', colcomm, sep="")
            logger(command)
            justDoIt(command)
            } else {
            command <- paste('barplot(prop.table(.Table), main="', title,
                    '", ylab="Relative Frequency", legend.text=', leg,
                    ', beside=', besid, colcomm, sep="")
            logger(command)
            justDoIt(command)
            }

        } else {

            if ( nrows == 1 ) {
            command <- paste('barplot(.Table, main="', title,
                    '", ylab="Frequency"', colcomm, sep="")
            logger(command)
            justDoIt(command)
            } else {
            command <- paste('barplot(.Table, main="', title,
                    '", ylab="Frequency", legend.text=', leg, ', beside=', besid, colcomm, sep="")
            logger(command)
            justDoIt(command)
            }
        }
    }
    OKCancelHelp(helpSubject="barplot")

    ###################################################################
    titleFrame <- tkframe(top)
    titleVariable <- tclVar(gettextRcmdr(""))
    titleField <- tkentry(titleFrame, width="40", textvariable=titleVariable)
    tkgrid(tklabel(titleFrame, text=gettextRcmdr("Title: "), fg="blue"), titleField, sticky="w")
    tkgrid(titleFrame, sticky="w")
    ###################################################################

    ###################
    # table setup
    tkgrid(tklabel(rowColFrame, text=gettextRcmdr("Number of Columns (reponses):")), colsSlider, colsShow, sticky="w")
    tkgrid(tklabel(rowColFrame, text=gettextRcmdr("Number of Rows (groups):")), rowsSlider, rowsShow, sticky="w")
    tkgrid(rowColFrame, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Enter counts:"), fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")

    ###################
    radioButtons(name="type", buttons=c("segmented", "beside"),
        labels=gettextRcmdr(c("Stacked bars", "Side-by-side bars")), title=gettextRcmdr("Display groups with:"))
    optionsFrame <- tkframe(top)
    legendVariable <- tclVar("1")
    legendCheckBox <- tkcheckbutton(optionsFrame, variable=legendVariable)
    propVariable <- tclVar("0")
    propCheckBox <- tkcheckbutton(optionsFrame, variable=propVariable)
    coloVariable <- tclVar("1")
    coloCheckBox <- tkcheckbutton(optionsFrame, variable=coloVariable)

    tkgrid(tklabel(top, text=gettextRcmdr("Options:"), fg="blue"), sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Relative Frequencies: "), justify="left"),
        propCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Rainbow: "), justify="left"),
        coloCheckBox, sticky="w")
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Legend (groups>1): "), justify="left"),
        legendCheckBox, sticky="w")
    tkgrid(optionsFrame, sticky="w")

    tkgrid(typeFrame, sticky="w")

    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=7, columns=2)
    }
