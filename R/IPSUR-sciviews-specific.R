# SciViews specific R Commander code

svCommander <- function(Version = "1.1-0"){
    # The SciViews specific Commander() function
    if (is.SciViews()) {
        # TO DO: automatically generate the menu from "Rcmdr-menus.txt"
        # Display the R commander menu
        #...
        setOption <- function(option, default, global=TRUE) {
            opt <- if (is.null(current[[option]])) default else current[[option]]
            if (global) putRcmdr(option, opt)
            else opt
            }
        etc <- file.path(.path.package(package="IPSUR")[1], "etc")
        # Do NOT sink error messages!
        #assign(".messages.connection", textConnection(".messages", open = "w"), envir=.GlobalEnv)
        #sink(.messages.connection, type="message")
        messageTag(reset=TRUE)
		putRcmdr("RcmdrVersion", Version)
		#putRcmdr("length.messages", 0)
        putRcmdr(".activeDataSet", NULL)
        putRcmdr(".activeModel", NULL)
        putRcmdr("logFileName", NULL)
        putRcmdr("outputFileName", NULL)
        putRcmdr("saveFileName", NULL)
        putRcmdr("modelNumber", 0)
        putRcmdr("rgl", FALSE)
        current <- options("Rcmdr")[[1]]
        setOption("log.font.size", if (.Platform$OS.type == "windows") 10 else 12)
        putRcmdr("logFont", tkfont.create(family="courier", size=getRcmdr("log.font.size")))
    	putRcmdr("operatorFont", tkfont.create(family="courier", size=getRcmdr("log.font.size")))
		scale.factor <- current$scale.factor
        if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
        if (packageAvailable("car")) {
            require("car")
            setOption("contrasts", c("contr.Treatment", "contr.poly"))
            }
        else setOption("contrasts", c("contr.treatment", "contr.poly"))
        setOption("log.commands", TRUE)
        #assign("logCommands", if (log.commands) tclVar("1") else tclVar("0"))
        setOption("console.output", TRUE) # Must be set to TRUE for SciViews app!
        log.height <- as.character(setOption("log.height", if (!getRcmdr("log.commands")) 0 else 10, global=FALSE))
        log.width <- as.character(setOption("log.width", 80, global=FALSE))
    	output.height <- as.character(setOption("output.height",
        	if (getRcmdr("console.output")) 0
        	else if ((as.numeric(log.height) != 0) || (!getRcmdr("log.commands"))) 2*as.numeric(log.height)
        	else 20, global=FALSE))
        setOption("output.height", output.height)
        putRcmdr("saveOptions", options(warn=1, contrasts=getRcmdr("contrasts"), width=as.numeric(log.width),
            na.action="na.exclude", graphics.record=TRUE))
        setOption("double.click", FALSE)
        setOption("sort.names", TRUE)
        setOption("grab.focus", TRUE)
        setOption("attach.data.set", FALSE)
        setOption("log.text.color", "black")
        setOption("command.text.color", "red")
        setOption("output.text.color", "darkblue")
        setOption("error.text.color", "red")
        setOption("warning.text.color", "darkgreen")
        setOption("multiple.select.mode", "extended")
        setOption("suppress.X11.warnings", .Platform$GUI == "X11") # to address problem in Linux
        setOption("showData.threshold", 100)
    	setOption("retain.messages", FALSE)
        setOption("crisp.dialogs",  (.Platform$OS.type == "windows") && (getRversion() >= "2.1.1"))
		if (.Platform$OS.type != "windows") {
        	putRcmdr("oldPager", options(pager=RcmdrPager))
        	default.font.size <- as.character(setOption("default.font.size", 12, global=FALSE))
        	default.font <- setOption("default.font",
            	paste("*helvetica-medium-r-normal-*-", default.font.size, "*", sep=""), global=FALSE)
        	.Tcl(paste("option add *font ", default.font, sep=""))
        	}
    	#if (getRcmdr("crisp.dialogs")) tclServiceMode(on=FALSE)
    	#if (getRcmdr("suppress.X11.warnings")) {
        #	putRcmdr("messages.connection", textConnection(".messages", open = "w", local=FALSE))
        #	sink(getRcmdr("messages.connection"), type="message")
        #	putRcmdr("length.messages", 0)
        #	}
        putRcmdr("commanderWindow", NULL)
        .commander <- NULL
		placement <- setOption("placement", "-40+20", global=FALSE)
#        source.files <- list.files(etc, pattern="\\.R$")  # duplicate line commented out by J. Fox
#        .commander.done <<- tclVar("0")
   #     source.files <- list.files(etc, pattern="\\.[Rr]$")
   #     for (file in source.files) {
   #          source(file.path(etc, file))
   #          cat(paste("Sourced:", file, "\n"))
   #          }
   #     Menus <- read.table(file.path(etc, "Rcmdr-menus.txt"), as.is=TRUE)
	    # TO DO: we need another treatment for this!
        #for (m in 1:nrow(Menus)){
        #    if (Menus[m, 1] == "menu") assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE))
        #    else if (Menus[m, 1] == "item") {
        #         if (Menus[m, 3] == "command")
        #             tkadd(eval(parse(text=Menus[m, 2])),"command", label=Menus[m, 4], command=eval(parse(text=Menus[m, 5])))
        #         else if (Menus[m, 3] == "cascade")
        #             tkadd(eval(parse(text=Menus[m, 2])),"cascade", label=Menus[m, 4], menu=eval(parse(text=Menus[m, 5])))
        #         else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        #         }
        #    else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        #    }
        putRcmdr("Menus", list())
		exceptions <- scan(file.path(etc, "log-exceptions.txt"), what="", quiet=TRUE, comment.char="#")
        putRcmdr("modelClasses", scan(file.path(etc, "model-classes.txt"), what="", quiet=TRUE, comment.char="#"))
		putRcmdr("dataSetName", tclVar("<No active dataset>"))
		putRcmdr("dataSetLabel", NULL)
        putRcmdr("logWindow", NULL)
        putRcmdr("outputWindow", NULL)
		putRcmdr("messagesWindow", NULL)
		putRcmdr("modelName", tclVar("<No active model>"))
        putRcmdr("modelLabel", NULL)
		show.edit.button <- options("Rcmdr")[[1]]$show.edit.button
        show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
        if (!packageAvailable("rgl")) Message(gettextRcmdr("The rgl package is absent; 3D plots are unavailable."), type="warning")
    	Message(paste(gettextRcmdr("R Commander Version "), getRcmdr("RcmdrVersion"), ": ", date(), sep=""))
		}
    }

