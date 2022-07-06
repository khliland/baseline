### $Id: baselineGUI.R 193 2012-06-24 21:13:42Z kristl $

## Baseline parameters, expandable list
#' List of available baseline algorithms for GUI function
#' 
#' A list with data.frames containing parameters, minimum and maximum values
#' for GUIs, step lengths for sliders, default values and currently selected
#' values, plus a short description of each parameter. The list is used by the
#' GUIs, and is user customizable.
#' 
#' The list is not meant for usage by end-users, but is extendable and
#' customizable, allowing for extra algorithms, removal of algoritms or
#' changing of parameter sets.
#' 
#' @name baselineAlgorithmsGUI
#' @docType data
#' @keywords baseline
#' @examples
#' 
#' ## Get a list of all algorithms:
#' names(baselineAlgorithmsGUI)
#' ## Add new algorithm:
#' baselineAlgorithmsGUI$my.alg <- as.data.frame(matrix(c(0,20,1,1, 0,20,1,1), 2,4, byrow=TRUE))
#' dimnames(baselineAlgorithmsGUI$my.alg) <- list(par=c("kappa", "gamma"),
#' 	val=c("min","max","step","default"))
#' baselineAlgorithmsGUI$my.alg$current <- c(1,1)
#' baselineAlgorithmsGUI$my.alg$name <- c("Subtractive constand", "Additive constant")
#' @export
baselineAlgorithmsGUI <- list()
baselineAlgorithmsGUI$irls					    <- as.data.frame(matrix(c(0,10,0.1,5, 5,15,0.1,8, 0,0.5,0.01,0.05, 50,200,25,100), 4,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$irls)		<- list(par=c("lambda1", "lambda2", "wi", "maxit"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$irls$current		 	<- c(5,8,0.05,100)
baselineAlgorithmsGUI$irls$name				  <- c("Primary smoothing", "Main smoothing", "Weighting", "Maximum iterations")
baselineAlgorithmsGUI$modpolyfit			    <- as.data.frame(matrix(c(0,10,1,4, 0,15,1,4, 25,200,25,100), 3,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$modpolyfit)<- list(par=c("degree", "tol", "rep"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$modpolyfit$current	<- c(4,4,100)
baselineAlgorithmsGUI$modpolyfit$name		  <- c("Polynomial degree", "Update tolerance 10^-", "Max #iterations")
baselineAlgorithmsGUI$als					  <- as.data.frame(matrix(c(0,15,1,6, 0,0.5,0.001,0.05), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$als)	<- list(par=c("lambda", "p"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$als$current		<- c(6,0.05)
baselineAlgorithmsGUI$als$name			<- c("Smoothing parameter", "Residual weighting")
baselineAlgorithmsGUI$rollingBall 			    <- as.data.frame(matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$rollingBall)	<- list(par=c("wm", "ws"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$rollingBall$current	  <- c(300,200)
baselineAlgorithmsGUI$rollingBall$name		  <- c("Min/max window width", "Smoothing window width")
baselineAlgorithmsGUI$medianWindow			     <- as.data.frame(matrix(c(0,500,10,300, 0,500,10,200), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$medianWindow) <- list(par=c("hwm", "hws"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$medianWindow$current	 <- c(300,200)
baselineAlgorithmsGUI$medianWindow$name		   <- c("Median window half width", "Smoothing window half width")
baselineAlgorithmsGUI$fillPeaks		 		    <- as.data.frame(matrix(c(0,15,1,6, 10,500,10,50, 1,100,1,10, 100,5000,100,2000), 4,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$fillPeaks)	<- list(par=c("lambda", "hwi", "it", "int"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$fillPeaks$current		<- c(6,50,10,2000)
baselineAlgorithmsGUI$fillPeaks$name		  <- c("Primary smoothing", "Half width of local windows", "Maximum number of iterations", "Number of buckets")
baselineAlgorithmsGUI$peakDetection		 	      <- as.data.frame(matrix(c(50,500,50,300, 10,200,10,50), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$peakDetection) <- list(par=c("left.right", "lwin.rwin"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$peakDetection$current   <- c(300,50)
baselineAlgorithmsGUI$peakDetection$name	    <- c("Peak window width", "Smoothing window width")
baselineAlgorithmsGUI$rfbaseline		 	     <- as.data.frame(matrix(c(100,5000,10,1000, 1,5,0.5,3.5), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$rfbaseline) <- list(par=c("NoXP", "b"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$rfbaseline$current	 <- c(1000,3.5)
baselineAlgorithmsGUI$rfbaseline$name		   <- c("Number of regression points", "Relative weighting")
baselineAlgorithmsGUI$shirley			 	    <- as.data.frame(matrix(c(10,1000,10,50, 1e-7,1e-5,1e-7,1e-6), 2,4, byrow=TRUE))
dimnames(baselineAlgorithmsGUI$shirley)	<- list(par=c("maxit", "err"),val=c("min","max","step","default"))
baselineAlgorithmsGUI$shirley$current		<- c(50,1e-6)
baselineAlgorithmsGUI$shirley$name			<- c("Max #iterations", "Error")



#' @title Interactive plotting tool
#' 
#' @description An interactive plotting tool for dynamic visualization of baselines and
#' their effect using the gWidgets2 package with GTK+ or Tcl/Tk.
#' 
#' @details Creates and updates a list containing current baseline and spectrum
#' (baseline.result).  Make sure a gWidget2 implementation is available, e.g
#' gWidgets2RGtk2 or gWidgets2tcltk and a corresponding backend like GTK+ or
#' Tcl/Tk. The GUI was developed using GTK which is an external dependency in
#' Windows ans OS X.
#' 
#' @param spectra Matrix with spectra in rows
#' @param method Baseline correction method (optional)
#' @param labels Labels for X-axis (optional)
#' @param rev.x Reverse X-axis (optional, default=FALSE)
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @keywords baseline spectra
#' @examples
#' 
#' data(milk)
#' \dontrun{
#' # Dependent on external software
#' baselineGUI(milk$spectra)
#' }
#' @export
baselineGUI <- function(spectra, method='irls', labels, rev.x=FALSE){
  if(requireNamespace("gWidgets2", quietly = TRUE)){
    if(exists("baselineAlgorithmsGUI",envir=.GlobalEnv)){
      bAGUI <- get("baselineAlgorithmsGUI",envir=.GlobalEnv)
    } else {
      bAGUI <- baselineAlgorithmsGUI
    }
    
    
    ##
    ## Ititialise variables that are shared between the elements of the GUI:
    ##
    
    
    ## Spectrum parameters
    Y <- spectra; specNo <- 1
    n <- length(Y[1,]); n1 <- length(Y[1,])
    x <- 1:n1
    if(missing(labels)) # X-axis labels
      labels <- 1:n
    
    ## Plotting parameters
    xz <- 1; yz <- 1; xc <- 0; yc <-0
    setZoom <- function(){
      xz <<- 1; yz <<- 1; xc <<- 0; yc <<-0
    }
    gridOn <- FALSE           # Is the grid on?
    visibleZoom <- FALSE      # Has the zoom tools been activated?
    visibleCustomize <- FALSE # Has the parameter customization been activated?
    visibleExport <- FALSE	  # Has the export of all spectra been started?
    
    ##
    ## Define functions that are used by the GUI elements
    ##
    
    
    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Baseline computation
    baseline.compute <- function(){
      clearPlot()
      ## Compute baseline based on current method and settings
      spec <- list()
      command <- "spec <- baseline(Y[specNo,,drop=FALSE]"
      for(i in 1:dim(bAGUI[[method]])[1]){
        command <- paste(command, ", ", rownames(bAGUI[[method]])[i], "=", bAGUI[[method]]$current[i], sep="")
      }
      command <- paste(command, ", method='", method, "')", sep="")
      eval(parse(text=command))
      ## Kludge to aviod warnings from R CMD check:
      # assign("baseline.result", spec, .GlobalEnv)
      putBaselineEnv("baseline.result", spec)
      putBaselineEnv("baseline.current", list(method=method, parNames=rownames(bAGUI[[method]]), parValues=bAGUI[[method]]$current))
      updatePlot()
    }                                   # end of baseline.compute
    
    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Clear plot
    clearPlot <- function(){
      par(new = TRUE, mfrow = c(1,1))
      plot(0, 0, xlim=c(-1,1), ylim=c(-1,1), xlab="", ylab="", main="",
           axes=FALSE, col='white')
      par(new=FALSE)
      C <- as.list(par("usr")); names(C) <- c("xmin", "xmax", "ymin", "ymax")
      ##print(unix.time(
      rect(C$xmin, C$ymin, C$xmax, C$ymax, angle = 0, density = 20,
           col = 'white', lwd = 2)
      ##))
      text(0, 0, labels = "Calculating baseline...", col = 'blue', cex = 2)
    }
    
    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Update plot
    updatePlot <- function() {
      ## FIXME: get is a kludge to avoid warnings from R CMD check:
      plot(getBaselineEnv("baseline.result"), grid = gridOn, labels = labels, rev.x = rev.x,
           zoom = list(xz = xz, yz = yz, xc = xc, yc = yc))
    }                                   # end of updatePlot
    
    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Zoom control window
    zoomControl <- function(){
      ## Make zoom window
      zoomWindow <- gWidgets2::gwindow("Plot properties", width=300)
      superGroup <- gWidgets2::ggroup(horizontal=FALSE,container=zoomWindow)
      subgroupXz <- gWidgets2::gframe("X zoom",horizontal=FALSE, container=superGroup)
      subgroupXc <- gWidgets2::gframe("X center",horizontal=FALSE, container=superGroup)
      subgroupYz <- gWidgets2::gframe("Y zoom",horizontal=FALSE, container=superGroup)
      subgroupYc <- gWidgets2::gframe("Y center",horizontal=FALSE, container=superGroup)
      subgroup3 <- gWidgets2::ggroup(horizontal=TRUE,expand=TRUE, container=superGroup)
      
      ## Initialize zoom sliders
      visibleZoom <<- TRUE
      zoomX <- gWidgets2::gslider(from=1,to=100,by=.1, value=1, handler = function(h,...){ xz <<- gWidgets2::svalue(zoomX); updatePlot()}, container = subgroupXz)
      zoomY <- gWidgets2::gslider(from=1,to=100,by=.5, value=1, handler = function(h,...){ yz <<- gWidgets2::svalue(zoomY); updatePlot()}, container = subgroupYz)
      centerX <- gWidgets2::gslider(from=-100,to=100,by=.1, value=0, handler = function(h,...){ xc <<- gWidgets2::svalue(centerX); updatePlot()}, container = subgroupXc)
      centerY <- gWidgets2::gslider(from=-100,to=100,by=.1, value=0, handler = function(h,...){ yc <<- gWidgets2::svalue(centerY); updatePlot()}, container = subgroupYc)
      resetZoom <- gWidgets2::gbutton(text = "Reset zoom and center", handler = function(h,...){ gWidgets2::svalue(zoomX)<-1; gWidgets2::svalue(zoomY)<-1; gWidgets2::svalue(centerX)<-0; gWidgets2::svalue(centerY)<-0; updatePlot()}, container = subgroup3)
      gridCheck <- gWidgets2::gcheckbox('Grid', handler = function(h,...){ gridOn <<- gWidgets2::svalue(gridCheck); updatePlot()}, container = subgroup3)
      
      ## Add zoom sliders
      gWidgets2::add(subgroupXz,zoomX,expand=TRUE)
      gWidgets2::add(subgroupXc,centerX,expand=TRUE)
      gWidgets2::add(superGroup,subgroupXz,expand=TRUE)
      gWidgets2::add(superGroup,subgroupXc,expand=TRUE)
      gWidgets2::addSpace(superGroup,20)
      gWidgets2::add(subgroupYz,zoomY,expand=TRUE)
      gWidgets2::add(subgroupYc,centerY,expand=TRUE)
      gWidgets2::add(superGroup,subgroupYz,expand=TRUE)
      gWidgets2::add(superGroup,subgroupYc,expand=TRUE)
      gWidgets2::add(subgroup3,resetZoom,expand=TRUE)
      gWidgets2::add(subgroup3,gridCheck,expand=FALSE)
      gWidgets2::add(superGroup,subgroup3,expand=TRUE)
      gWidgets2::addHandlerDestroy(zoomWindow, handler=function(h,...){visibleZoom <<- FALSE})
    }                                   # end of zoomControl
    
    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Apply to all spectra
    exportControl <- function(){
      visibleExport <<- TRUE
      exportWindow <- gWidgets2::gwindow("Apply correction to all spectra", width=300)
      superGroup   <- gWidgets2::ggroup(horizontal=FALSE,container=exportWindow)
      subgroup     <- gWidgets2::gframe("Object name",horizontal=FALSE,container = superGroup)

      spec <- list()
      command <- "spec <- baseline(Y"
      for(i in 1:dim(bAGUI[[method]])[1]){
        command <- paste(command, ", ", rownames(bAGUI[[method]])[i], "=", bAGUI[[method]]$current[i], sep="")
      }
      command <- paste(command, ", method='", method, "')", sep="")
      eval(parse(text=command))
      
      exportName <- gWidgets2::gedit(text="corrected.spectra", width=20, container = subgroup)
      doExport   <- gWidgets2::gbutton(text = "Apply and export", handler = function(h,...){
        the.name <- gWidgets2::svalue(exportName) 
        cat("\nCorrecting ...")
        putBaselineEnv('the.export', spec)
        eval(parse(text = paste(the.name, ' <- getBaselineEnv("the.export")', sep="")),envir = .GlobalEnv)
        #assign(the.name, the.export,envir = .GlobalEnv);
        gWidgets2::dispose(exportWindow);cat("\nSaved as: ",the.name, sep="")},
        container = subgroup
      )
      gWidgets2::add(subgroup,exportName,expand=TRUE)
      gWidgets2::add(subgroup,doExport,expand=TRUE)
      gWidgets2::add(superGroup,subgroup,expand=TRUE)
      gWidgets2::addHandlerDestroy(exportWindow, handler=function(h,...){visibleExport <<- FALSE})
    }
    
    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Slider configuration window
    parameterControl <- function(){
      ## Initialize spectrum type chooser and parameter fields
      visibleCustomize <<- TRUE
      
      saveParameters <- function(){
        theSet <- bAGUI[method][[1]]
        for(i in 1:length(parameterList)){
          for(j in 1:4){
            theSet[i,j] <- gWidgets2::svalue(parameterList[[i]][[j]])
          }
        }
        bAGUI[method][[1]][,1:3] <<- theSet[,1:3]
        bAGUI[method][[1]]$current <<- theSet[,4]
        gWidgets2::delete(outerParam,remParam); createMethodSliders(); gWidgets2::dispose(parameterWindow)# ; baseline.compute()
      }                               # end of saveParameters
      
      addParameterGroup <- function(nameStr, lineNo){
        parameterList[[lineNo]]  <<- c(gWidgets2::gedit(text = "", width=1,coerce.with=as.numeric, cont=parameterGroup),gWidgets2::gedit(width=5,coerce.with=as.numeric, cont=parameterGroup),gWidgets2::gedit(width=10,coerce.with=as.numeric, cont=parameterGroup),gWidgets2::gedit(width=15,coerce.with=as.numeric, cont=parameterGroup))
        # parameterGroup[lineNo+1,1] <<- gWidgets2::glabel(text=nameStr, cont=parameterGroup)
        parameterGroup[lineNo+1,1] <<- nameStr
        size(parameterList[[lineNo]][[1]]) <- 20
        size(parameterList[[lineNo]][[2]]) <- 20
        size(parameterList[[lineNo]][[3]]) <- 20
        size(parameterList[[lineNo]][[4]]) <- 20
        # size(parameterList[[lineNo]][[1]]) <- c(60,20)
        # size(parameterList[[lineNo]][[2]]) <- c(60,20)
        # size(parameterList[[lineNo]][[3]]) <- c(60,20)
        # size(parameterList[[lineNo]][[4]]) <- c(60,20)
        parameterGroup[lineNo+1,2] <<- parameterList[[lineNo]][[1]]
        parameterGroup[lineNo+1,3] <<- parameterList[[lineNo]][[2]]
        parameterGroup[lineNo+1,4] <<- parameterList[[lineNo]][[3]]
        parameterGroup[lineNo+1,5] <<- parameterList[[lineNo]][[4]]
      }                               # end of gWidgets2::addParameterGroup
      
      setParameters <- function(){
        theSet <- bAGUI[method][[1]]
        for(i in 1:length(parameterList)){
          for(j in 1:4){
            gWidgets2::svalue(parameterList[[i]][[j]]) <<- theSet[i,j]
          }
        }
      }
      
      ## Make parameter window
      parameterWindow <- gWidgets2::gwindow("Slider configuration", width=300)
      superGroup <- gWidgets2::ggroup(horizontal=FALSE,container=parameterWindow)
      #        choiceGroup <- gWidgets2::gframe("Type of spectra", container=superGroup, horizontal=FALSE)
      #        gWidgets2::add(choiceGroup,typeChooser,expand=FALSE)
      gWidgets2::addSpace(superGroup,10)
      
      # Button group and save
      subgroupButtons <- gWidgets2::ggroup(horizontal=TRUE, container=superGroup)
      saveButton <- gWidgets2::gbutton(text = "Apply parameters", handler = function(h,...) saveParameters(), container=subgroupButtons)

      ## Add edit fields
      parameterList <- list()
      parameterGroup <- gWidgets2::glayout(homogeneous = FALSE, spacing = 5, container=superGroup)
      parameterGroup[1,1] <- ""
      parameterGroup[1,2] <- "From:"
      parameterGroup[1,3] <- "To:"
      parameterGroup[1,4] <- "Spacing:"
      parameterGroup[1,5] <- "Start:"
      nameStrs <- rownames(bAGUI[method][[1]])
      for(i in 1:length(nameStrs))
        addParameterGroup(nameStrs[i],i)
      setParameters()
#      gWidgets2::visible(parameterGroup) <- TRUE
      gWidgets2::addSpring(superGroup)
      
      ## Add buttons
#      subgroupButtons <- gWidgets2::ggroup(horizontal=TRUE, container=superGroup) # Moved up
      gWidgets2::add(subgroupButtons,saveButton,expand=FALSE)
      gWidgets2::add(superGroup,subgroupButtons,expand=FALSE)
      gWidgets2::addHandlerDestroy(parameterWindow, handler=function(h,...){visibleCustomize <<- FALSE})
    }                                   # end of parameterControl
    
    ## --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
    ## Set up sliders according to chosen baseline correction method
    sVals <- list()
    sliders <- resets <- fields <- tmps <- tmpsIn <- list()
    iI <- 0
    
    createMethodSliders <- function(){
      remParam <<- gWidgets2::ggroup(horizontal=FALSE, container=outerParam)
      
      #############
      ## General ##
      #############
      
      ## Collect values for sliders
      sVals <<- bAGUI[[method]]
      sliders <<- resets <<- fields <<- tmps <<- tmpsIn <<- list()
      iI <<- dim(sVals)[1]
      for(i in 1:iI){
        tmps[[i]]    <<- gWidgets2::gframe(paste(sVals[i,6], " (", rownames(sVals)[i], ")", sep=""), container=remParam, horizontal=TRUE)
        tmpsIn[[i]]    <<- gWidgets2::ggroup(container=tmps[[i]], horizontal=TRUE)
        resets[[i]]  <<- gWidgets2::gbutton(text = "Reset", handler = function(h,...){
          for(a in 1:iI) gWidgets2::svalue(sliders[[a]])<<-sVals[a,4]}, cont=tmpsIn[[i]], expand=FALSE)
        sliders[[i]] <<- gWidgets2::gslider(from=sVals[i,1], to=sVals[i,2], by=sVals[i,3], value=bAGUI[[method]]$current[i],
                                            cont=tmpsIn[[i]],
                                            handler = function(h,...){ for(a in 1:iI){
                                              bAGUI[[method]]$current[a] <<- gWidgets2::svalue(sliders[[a]])
                                              bc <- getBaselineEnv("baseline.current")
                                              bc$parValues[a] <- gWidgets2::svalue(sliders[[a]])
                                              putBaselineEnv("baseline.current",bc)
                                              gWidgets2::svalue(fields[[a]]) <- gWidgets2::svalue(sliders[[a]])}}, expand=TRUE)
        fields[[i]] <<- gWidgets2::glabel(text = sVals[i,4], cont=tmpsIn[[i]], expand=FALSE)
        # gWidgets2::add(tmps[[i]], sliders[[i]], expand=TRUE)
        # addSpring(tmpsIn[[i]])
        # gWidgets2::add(tmps[[i]], fields[[i]], expand=FALSE)
        # gWidgets2::add(tmps[[i]], resets[[i]], expand=FALSE)
      }
    }                                   # end of createMethodSliders
    
    
    
    ##
    ## Create main GUI window
    ##
    
    
    if(exists("baselineAlgorithms",envir=.GlobalEnv)){
      bA <- get("baselineAlgorithms",envir=.GlobalEnv)
    } else {
      bA <- baselineAlgorithms
    }
    GUI.names <- sort(names(bAGUI))
    names <- character(length(GUI.names))
    for(i in 1:length(GUI.names)){ # Let bAGUI control, and bA have descriptions -------------
      names[i] <- paste("'", ifelse(is.null(bA[[GUI.names[i]]]@description),"",bA[[GUI.names[i]]]@description), " (", GUI.names[i], ")'", sep="")
    }
    # Moved methodChooser down to attain container
    # methodChooser <- gWidgets2::gcombobox(names,
    #                                      selected=which(GUI.names==method), handler = function(h,...){method <<- GUI.names[gWidgets2::svalue(methodChooser,index=TRUE)]; gWidgets2::delete(outerParam,remParam); createMethodSliders(); setZoom(); baseline.compute()})
    
    ## Initialize window and main containers
    window <- gWidgets2::gwindow("Baseline correction", width=300)

    ## Initialize spectrum chooser slider and method chooser
    superGroup2 <- gWidgets2::ggroup(horizontal=FALSE,container=window)
    if(dim(Y)[1] > 1){
      tmp <- gWidgets2::gframe("Spectrum number", container=superGroup2, horizontal=TRUE)
      tmpIn <- gWidgets2::ggroup(container=tmp)
      spectrumNo <- gWidgets2::gslider(from=1, to=dim(Y)[1], by=1, value=1, handler = function(h,...){ 
        specNo<<-gWidgets2::svalue(spectrumNo)
        gWidgets2::svalue(spectrumNoField) <- specNo}, container=tmpIn, expand=TRUE)
      spectrumNoField <- gWidgets2::glabel("1", container=tmpIn, expand=FALSE)
      # gWidgets2::add(tmp, spectrumNo, expand=TRUE)
    }
    methodChooser <- gWidgets2::gcombobox(names,
                                          selected=which(GUI.names==method), handler = function(h,...){method <<- GUI.names[gWidgets2::svalue(methodChooser,index=TRUE)]; gWidgets2::delete(outerParam,remParam); createMethodSliders(); setZoom(); baseline.compute()},
                                          container=superGroup2)
    gWidgets2::add(superGroup2,methodChooser, expand=FALSE)
    gWidgets2::addSpring(superGroup2)
    Settings <- gWidgets2::ggroup(container=superGroup2, horizontal=FALSE)
    outerParam <- gWidgets2::ggroup(horizontal=FALSE, container=Settings)
    remParam <- gWidgets2::ggroup(horizontal=FALSE, container=Settings)

    # ## Initialize spectrum chooser slider and method chooser
    # if(dim(Y)[1] > 1)
    #   spectrumNo <- gWidgets2::gslider(from=1, to=dim(Y)[1], by=1, value=1, handler = function(h,...) specNo<<-gWidgets2::svalue(spectrumNo), container=superGroup2)
    #    remParam <- gWidgets2::ggroup()
    
    createMethodSliders()               # Add algorithm slides
    
    ## Add buttons
    gWidgets2::addSpring(superGroup2)
    buttonGroup <- gWidgets2::ggroup(horizontal=TRUE, container=superGroup2)
    plotButton <- gWidgets2::gbutton(text = "Update plot", handler=function(h,...) baseline.compute(), cont=buttonGroup)
    gWidgets2::add(buttonGroup,plotButton,expand=TRUE)
#    newZoom <- gWidgets2::gbutton(text = "Zoom", handler = function(h,...){ if(visibleZoom==FALSE) zoomControl() }, cont=buttonGroup)
#    gWidgets2::add(buttonGroup,newZoom,expand=FALSE)
    newParameter <- gWidgets2::gbutton(text = "Customize", handler = function(h,...){ if(visibleCustomize==FALSE) parameterControl()}, cont=buttonGroup)
    gWidgets2::add(buttonGroup,newParameter,expand=FALSE)
    newExport <- gWidgets2::gbutton(text = "Apply to all", handler = function(h,...){ if(visibleExport==FALSE) exportControl()}, cont=buttonGroup)
    gWidgets2::add(buttonGroup,newExport,expand=FALSE)
    
    ## Display initial plot
    plot(0,0, xlim=c(-1,1), ylim=c(-1,1), xlab="", ylab="", main="", axes=FALSE, col='white')
    baseline.compute()
  } else {                                       # end of baselineGUI
    warning('Package gWidgets2 not installed')
    return(list())
  }
}
