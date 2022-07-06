### $Id: plotOptim.R 193 2012-06-24 21:13:42Z kristl $



#' @title Plotting tool for result objects from optimization
#' 
#' @description A graphical user interface for plotting optimisation results, either one
#' algorithm at the time or comparing algorithms.
#' 
#' @details \code{plotOptim} creates a user interface based on the supplied results.
#' Curve and level plots from single algorithms or comparison of algorithms is
#' avilable.
#' 
#' For single algorithms subsets, levels corresponding to local or global
#' minima, and averages can be extracted for plotting. For comparison of
#' algorithms levels corresponding to local or global minima can be used, or
#' levels corresponding to the minimum when averaging over selected values of
#' the regression parameter, e.g. selected components in PLSR.
#' 
#' @param results Result list from optimization
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @keywords baseline spectra
#' @export
plotOptim <- function(results){
  ## Plot optimisation through GUI
  
  if(requireNamespace("gWidgets2", quietly = TRUE)){
    if(requireNamespace("lattice", quietly = TRUE)){
      
      # Initialize parameters and paramter lists
      nAlgs <- 0
      progress <- toPlot <- groupPlots <- parameterList <- parameterPlots <- rGroups <- groups <- labels <- plotOne <- plotTwo <- plotFlip <- param <- list()
      oneDimNames <- character(length(results$results))
      if(exists("baselineAlgorithmsGUI",envir=.GlobalEnv)){
        bAGUI <- get("baselineAlgorithmsGUI",envir=.GlobalEnv)
      } else {
        bAGUI <- baselineAlgorithmsGUI
      }
      if(exists("baselineAlgorithms",envir=.GlobalEnv)){
        bA <- get("baselineAlgorithms",envir=.GlobalEnv)
      } else {
        bA <- baselineAlgorithms
      }
      
      # Functions for parsing beween method numbers and mehod names
      methodParse2 <- character(length(bA))
      for(i in 1:length(bA)){
        methodParse2[i] <- baselineAlgorithms[[i]]@funcName
      }
      
      # Set up main window with internal container
      win <- gWidgets2::gwindow("Optimisation results", width=550)
      main <- gWidgets2::ggroup(horizontal=FALSE, container = win)
      gWidgets2::add(win,main)

      # ############# #
      # Main notebook #
      # ############# #
      nb <- gWidgets2::gnotebook(container = main)
      gWidgets2::add(main,nb,expand=TRUE)
      
      # Prepare notebook for algorithms
      for(i in 1:length(results$results)){
        name <- names(bA)[which(methodParse2==results$baselineTests[[i]]@algorithm@funcName)]
        groups[[i]] <- gWidgets2::ggroup(horizontal=FALSE, container = nb,label=name)
        # gWidgets2::add(nb,groups[[i]],label=name)
        progress[[i]] <- gWidgets2::glabel('Setting up GUI...', container = groups[[i]])
        gWidgets2::add(groups[[i]], progress[[i]], expand=FALSE)
      }
      sGroup <- gWidgets2::ggroup(horizontal=FALSE, container = nb, label="Compare")
      mGroup <- gWidgets2::ggroup(horizontal=TRUE, container = sGroup)
      
      
      # Function for adding a baseline correction method, including sub-functions and variables
      addAlg <- function(nm, result){
        gWidgets2::delete(groups[[i]], progress[[i]])
        # Initialize parameters and groups
        nAlgs <<- nAlgs + 1
        name <- names(bA)[nm]
        nameLong <- bA[[nm]]@description
        method <- name
        
        # Add some space and the name of the baseline correction algorithm
        gWidgets2::addSpace(groups[[nAlgs]],10)
        gWidgets2::add(groups[[nAlgs]],gWidgets2::glabel(nameLong, container = groups[[nAlgs]]))
        
        # GUI for plotting
        groupPlots[[nAlgs]] <<- gWidgets2::ggroup(horizontal=FALSE, container = groups[[nAlgs]])
        
        # Function for setting up parameter plotting array in GUI
        addparameterPlots <- function(nameStr, lineNo){
          if(lineNo>1){ # Baseline parameter
            if(is.null(result@param[[nameStr]])){ # Used default value during optimization
              parameterList[[nAlgs]][[lineNo]]  <<- c(gWidgets2::gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gWidgets2::gcheckbox('default', checked=TRUE))
              gWidgets2::enabled(parameterList[[nAlgs]][[lineNo]][[1]]) <- FALSE
              parameterList[[nAlgs]][[i]][[3]][[4]] <- TRUE
              # gWidgets2::tag(parameterList[[nAlgs]][[lineNo]][[1]], "default") <- TRUE
            } else {
              if(max(nchar(as.character(result@param[[nameStr]],scientific=TRUE)))>8){ # More than 8 digits => use scientific format
                parameterList[[nAlgs]][[lineNo]]  <<- c(gWidgets2::gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gWidgets2::gcheckboxgroup(format(result@param[[nameStr]], scientific=TRUE, digits=3), horizontal=TRUE))
              } else {
                parameterList[[nAlgs]][[lineNo]]  <<- c(gWidgets2::gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gWidgets2::gcheckboxgroup(format(result@param[[nameStr]], scientific=FALSE), horizontal=TRUE))
              }
            }
          } else { # Regression parameter
            if(is.null(result@param[[1]])){
              parameterList[[nAlgs]][[lineNo]]  <<- c(gWidgets2::gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE), gWidgets2::gcheckbox('default', checked=TRUE))
              gWidgets2::enabled(parameterList[[nAlgs]][[lineNo]][[1]]) <- FALSE
              # parameterList[[nAlgs]][[lineNo]][[3]] <- TRUE
            } else {
              if(max(nchar(as.character(result@param[[1]])))){
                parameterList[[nAlgs]][[lineNo]]  <<- c(gWidgets2::gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE, selected=4, container = parameterPlots[[nAlgs]]), gWidgets2::gcheckboxgroup(format(result@param[[1]], scientific=TRUE, digits=3), horizontal=TRUE, container = parameterPlots[[nAlgs]]))
              } else {
                parameterList[[nAlgs]][[lineNo]]  <<- c(gWidgets2::gradio(c('Overall min.','Min.','Avg.','All','Chosen'),horizontal=TRUE, selected=4, container = parameterPlots[[nAlgs]]), gWidgets2::gcheckboxgroup(format(result@param[[1]], scientific=FALSE), horizontal=TRUE, container = parameterPlots[[nAlgs]]))
              }
            }
          }
          
          # Check which buttons should be available for current choices
          parameterList[[nAlgs]][[lineNo]][[3]] <<- list(lineNo, nAlgs, nameStr, FALSE)
          # gWidgets2::tag(parameterList[[nAlgs]][[lineNo]][[1]], "no") <- lineNo
          # gWidgets2::tag(parameterList[[nAlgs]][[lineNo]][[1]], "alg") <- nAlgs
          # gWidgets2::tag(parameterList[[nAlgs]][[lineNo]][[1]], "name") <- nameStr
          gWidgets2::addHandlerChanged(parameterList[[nAlgs]][[lineNo]][[1]], handler = function(h,...){
            linNo <- lineNo
#            nAlg <- nAlgs
            nAlg <- length(parameterList)
            # linNo <- gWidgets2::tag(h$obj)$no
            # nAlg <- gWidgets2::tag(h$obj)$alg
            if(gWidgets2::svalue(h$obj,index=TRUE) == 1){
              gWidgets2::svalue(parameterList[[nAlg]][[linNo]][[2]],index=TRUE) <- NULL}
            else {
              gWidgets2::svalue(parameterList[[nAlg]][[linNo]][[2]],index=TRUE) <- 1:length(result@param[[nameStr]])}
            if(gWidgets2::svalue(h$obj,index=TRUE) == 1 || gWidgets2::svalue(h$obj,index=TRUE) == 4){
              # gWidgets2::enabled(parameterList[[nAlg]][[linNo]][[2]]) <- FALSE
              }
            else{
              # gWidgets2::enabled(parameterList[[nAlg]][[linNo]][[2]]) <- TRUE
              }
            checkPlot(nAlg)
          })
          # parameterList[[nAlgs]][[lineNo]][[4]] <- c(lineNo, nAlgs, nameStr, FALSE)
          # gWidgets2::tag(parameterList[[nAlgs]][[lineNo]][[2]], "no") <- lineNo
          # gWidgets2::tag(parameterList[[nAlgs]][[lineNo]][[2]], "alg") <- nAlgs
          # gWidgets2::tag(parameterList[[nAlgs]][[lineNo]][[2]], "name") <- nameStr
          if(lineNo!=1)
            gWidgets2::enabled(parameterList[[nAlgs]][[lineNo]][[2]]) <- FALSE
          if(lineNo==1)
            gWidgets2::svalue(parameterList[[nAlgs]][[lineNo]][[2]],index=TRUE) <- 1:length(result@param[[nameStr]])
          gWidgets2::addHandlerChanged(parameterList[[nAlgs]][[lineNo]][[2]], handler = function(h,...){
            linNo <- lineNo
            nAlg <- nAlgs
            # linNo <- gWidgets2::tag(h$obj)$no
            # nAlg <- gWidgets2::tag(h$obj)$alg
            checkPlot(nAlg)
          })
          
          # Set up visual optimization array for parameters
          parameterPlots[[nAlgs]][lineNo+1,1] <<- gWidgets2::glabel(text=paste(nameStr,":",sep=""), container = parameterPlots[[nAlgs]])
          parameterPlots[[nAlgs]][lineNo+1,2] <<- parameterList[[nAlgs]][[lineNo]][[1]]
          parameterPlots[[nAlgs]][lineNo+1,3] <<- parameterList[[nAlgs]][[lineNo]][[2]]
        }
        # Set up visual optimization array for parameters
        parameterList[[nAlgs]] <<- list()
        parameterPlots[[nAlgs]] <<- glayout(homogeneous = FALSE, spacing = 5, container=groupPlots[[nAlgs]])
        parameterPlots[[nAlgs]][1,1] <<- gWidgets2::glabel("", container = parameterPlots[[nAlgs]])
        parameterPlots[[nAlgs]][1,2] <<- gWidgets2::glabel("Which:", container = parameterPlots[[nAlgs]])
        parameterPlots[[nAlgs]][1,3] <<- gWidgets2::glabel("", container = parameterPlots[[nAlgs]])
        nameStrs <- c(names(result@param[1]), rownames(bA[[method]])) ##### ##### #### ####### ##### ####
        lns <- length(nameStrs)
        for(i in 1:lns)
          addparameterPlots(nameStrs[i],i)
        
        gWidgets2::addSpace(groups[[nAlgs]], 10)
        gWidgets2::add(groups[[nAlgs]], groupPlots[[nAlgs]], expand=FALSE)
        
        rGroups[[nAlgs]] <<- gWidgets2::ggroup(horizontal=TRUE, container = groups[[nAlgs]])

        # Plot buttons
        plotOne[[nAlgs]] <<- gWidgets2::gbutton("Curve plot", container = rGroups[[nAlgs]])
        plotTwo[[nAlgs]] <<- gWidgets2::gbutton("Level plot", container = rGroups[[nAlgs]])
        gWidgets2::tag(plotOne[[nAlgs]], "alg") <- nAlgs
        gWidgets2::tag(plotTwo[[nAlgs]], "alg") <- nAlgs
        
        # Plot curves
        gWidgets2::addHandlerChanged(plotOne[[nAlgs]], handler = function(h,...){
          nAlg <- length(plotTwo)
          # nAlg <- nAlgs
          # nAlg <- gWidgets2::tag(h$obj)$alg
          if(is.vector(toPlot[[nAlg]])){ # Single curve
            plot(names(toPlot[[nAlg]]),toPlot[[nAlg]], type='l', ylab=results$results[[nAlg]]@qualMeasName, xlab=oneDimNames[nAlg])
          } else { # Multiple curves
            if(gWidgets2::svalue(plotFlip[[nAlg]])==FALSE){
              plot(rownames(toPlot[[nAlg]]),toPlot[[nAlg]][,1], type='l', xlab=names(dimnames(toPlot[[nAlg]]))[1], ylim=c(min(toPlot[[nAlg]]),max(toPlot[[nAlg]])), ylab=results$results[[nAlg]]@qualMeasName, axes=FALSE)
              axis(1, at=rownames(toPlot[[nAlg]]))
              axis(2)
              box()
              for(i in 2:dim(toPlot[[nAlg]])[2]){
                lines(rownames(toPlot[[nAlg]]),toPlot[[nAlg]][,i], col=i)
              }
              legend(x="topright", legend=colnames(toPlot[[nAlg]]), col=1:dim(toPlot[[nAlg]])[2], lty=1, title=names(dimnames(toPlot[[nAlg]]))[2])
            } else { # Transpose matrix before plotting
              plot(colnames(toPlot[[nAlg]]),toPlot[[nAlg]][1,], type='l', xlab=names(dimnames(toPlot[[nAlg]]))[2], ylim=c(min(toPlot[[nAlg]]),max(toPlot[[nAlg]])), ylab=results$results[[nAlg]]@qualMeasName, axes=FALSE)
              axis(1, at=colnames(toPlot[[nAlg]]))
              axis(2)
              box()
              for(i in 2:dim(toPlot[[nAlg]])[1]){
                lines(colnames(toPlot[[nAlg]]),toPlot[[nAlg]][i,], col=i)
              }
              legend(x="topright", legend=rownames(toPlot[[nAlg]]), col=1:dim(toPlot[[nAlg]])[1], lty=1, title=names(dimnames(toPlot[[nAlg]]))[1])
            }
          }
        })
        
        # Level plot
        gWidgets2::addHandlerChanged(plotTwo[[nAlgs]], handler = function(h,...){
          nAlg <- length(plotTwo)
#          nAlg <- nAlgs
          # nAlg <- gWidgets2::tag(h$obj)$alg
          Gray <- function(n){gray(seq(0,1, length.out=n))}
          if(gWidgets2::svalue(plotFlip[[nAlg]])==FALSE){
            plot(lattice::levelplot(toPlot[[nAlg]], xlab=names(dimnames(toPlot[[nAlg]]))[1], ylab=names(dimnames(toPlot[[nAlg]]))[2], col.regions = Gray))
          } else { # Transpose matrix before plotting
            plot(lattice::levelplot(t(toPlot[[nAlg]]), xlab=names(dimnames(toPlot[[nAlg]]))[2], ylab=names(dimnames(toPlot[[nAlg]]))[1], col.regions = Gray))
          }
        })
        plotFlip[[nAlgs]] <<- gWidgets2::gcheckbox("flip", container = rGroups[[nAlgs]])
        gWidgets2::enabled(plotTwo[[nAlgs]]) <<- FALSE
        gWidgets2::enabled(plotFlip[[nAlgs]]) <<- FALSE
        # rGroups[[nAlgs]] <<- gWidgets2::ggroup(horizontal=TRUE, container = groups[[nAlgs]])
        gWidgets2::add(rGroups[[nAlgs]], plotOne[[nAlgs]],expand=FALSE)
        gWidgets2::add(rGroups[[nAlgs]], plotTwo[[nAlgs]],expand=FALSE)
        gWidgets2::add(rGroups[[nAlgs]], plotFlip[[nAlgs]],expand=FALSE)
        gWidgets2::addSpace(groups[[nAlgs]], 10)
        gWidgets2::add(groups[[nAlgs]], rGroups[[nAlgs]],expand=FALSE)
        
        # gWidgets2::add(nb,groups[[nAlgs]],label=name)
        # visible(parameterPlots[[nAlgs]]) <<- TRUE
        checkPlot(nAlgs)
      }
      
      # Check what can be plotted
      checkPlot <- function(nAlg){
        m <- list(results$results[[nAlg]])
        nam <- ""
        mins <- avg <- character(0)
        j <- 2
        k <- 0
        l <- 0
        # Prepare call to function 'qualMeas'
        for(i in 1:length(parameterList[[nAlg]])){
          def <- parameterList[[nAlg]][[i]][[3]][[4]]
          if(!def){
            if(gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="Overall min."){
              m[[j]] <- "overall"
              nam[j] <- parameterList[[nAlgs]][[i]][[3]][[3]]
              # nam[j] <- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
              j <- j+1
            } else
              if(gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="All"){
                m[[j]] <- "all"
                nam[j] <- parameterList[[nAlgs]][[i]][[3]][[3]]
                # nam[j] <- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
                j <- j+1
              } else
                if(gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="Chosen"){
                  m[[j]] <- gWidgets2::svalue(parameterList[[nAlg]][[i]][[2]],index=TRUE)
                  nam[j] <- parameterList[[nAlgs]][[i]][[3]][[3]]
                  # nam[j] <- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
                  j <- j+1
                } else
                  if(gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="Min."){
                    k <- k+1
                    mins[k] <- parameterList[[nAlgs]][[i]][[3]][[3]]
                    # mins[k] <- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
                    m[[j]] <- gWidgets2::svalue(parameterList[[nAlg]][[i]][[2]],index=TRUE)
                    nam[j] <- parameterList[[nAlgs]][[i]][[3]][[3]]
                    # nam[j] <- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
                    j <- j+1
                  }
            if(gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="Avg."){
              l <- l+1
              avg[l] <- parameterList[[nAlgs]][[i]][[3]][[3]]
              # avg[l] <- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
              m[[j]] <- gWidgets2::svalue(parameterList[[nAlg]][[i]][[2]],index=TRUE)
              nam[j] <- parameterList[[nAlgs]][[i]][[3]][[3]]
              # nam[j] <- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
              j <- j+1
            }
          }
        }            # End for loop
        if(k>0){
          m[[j]] <- mins
          nam[j] <- "MIN"
          j <- j+1
        }
        if(l>0){
          m[[j]] <- avg
          nam[j] <- "AVG"
        }
        
        names(m) <- nam
        toPlot[[nAlg]] <<- drop(do.call(qualMeas,m)) # What will be plotted?
        if(length(dim(toPlot[[nAlg]]))>2){ # Too many dimensions to plot
          gWidgets2::enabled(plotOne[[nAlg]]) <- FALSE
          gWidgets2::enabled(plotTwo[[nAlg]]) <- FALSE
          gWidgets2::enabled(plotFlip[[nAlg]]) <- FALSE
        } else if((length(dim(toPlot[[nAlg]]))==2) && (prod(dim(toPlot[[nAlg]]))>0)){ # Two dimensions to plot
          gWidgets2::enabled(plotOne[[nAlg]]) <- TRUE
          gWidgets2::enabled(plotTwo[[nAlg]]) <- TRUE
          gWidgets2::enabled(plotFlip[[nAlg]]) <- TRUE
        } else if(is.null(dim(toPlot[[nAlg]])) && (length(toPlot[[nAlg]])>1)){ # One dimension to plot
          gWidgets2::enabled(plotOne[[nAlg]]) <- TRUE
          gWidgets2::enabled(plotTwo[[nAlg]]) <- FALSE
          gWidgets2::enabled(plotFlip[[nAlg]]) <- FALSE
          for(i in 1:length(parameterList[[nAlg]])){
            if((gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="Chosen" || gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="Avg." || gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="All" || gWidgets2::svalue(parameterList[[nAlg]][[i]][[1]])=="Avg.") && length(gWidgets2::svalue(parameterList[[nAlg]][[i]][[2]]))>1){
              oneDimNames[nAlg] <<- parameterList[[nAlgs]][[i]][[3]][[3]]
              # oneDimNames[nAlg] <<- gWidgets2::tag(parameterList[[nAlg]][[i]][[1]])$name
            }
          }
        } else { # No dimensions to plot
          gWidgets2::enabled(plotOne[[nAlg]]) <- FALSE
          gWidgets2::enabled(plotTwo[[nAlg]]) <- FALSE
          gWidgets2::enabled(plotFlip[[nAlg]]) <- FALSE
        }
      }
      
      # ######### #
      # Comparing #
      # ######### #
      algNames <- c("-> Choose first result set")
      for(i in 1:length(results$results)){ # Prepare choices for result set chooser
        name <- names(bA)[which(methodParse2==results$baselineTests[[i]]@algorithm@funcName)]
        algNames[i+1] <- name
      }
      
      # Label, button, radio buttons and groups
      compareLabel <- gWidgets2::glabel('Compare algorithms', container = sGroup)
      curveButton <- gWidgets2::gbutton(paste('Plot',results$results[[1]]@qualMeasName,'against',names(results$results[[1]]@param)[1]), handler = function(h,...){
        ns <- list()
        if(gWidgets2::svalue(minBest,index=TRUE)==3){
          nams <- character(length(results$results))
          for(i in 1:length(results$results)){
            nam <- c("", names(results$results[[1]]@param)[1], "AVG")
            m <- list(results$results[[i]])
            minOver <- gWidgets2::svalue(minWhich,index=TRUE)
            m[[2]] <- minOver
            m[[3]] <- names(results$results[[1]]@param)[1]
            names(m) <- nam
            ns.tmp <- do.call(qualMeas,m)
            the.min <- which(ns.tmp==min(ns.tmp), arr.ind = TRUE)
            the.dim <- dimnames(ns.tmp)
            
            nam <- c("")
            m <- list(results$results[[i]])
            nam <- append(nam, names(results$results[[1]]@param)[1])
            m[[2]]   <- "all"
            for(j in 2:length(results$results[[i]]@param)){
              nam <- append(nam, names(results$results[[i]]@param)[j])
              m[[j+1]] <- the.min[j]
            }
            names(m) <- nam
            ns[[i]] <- drop(do.call(qualMeas,m))
            nams[i] <- names(bA)[which(methodParse2==results$baselineTests[[i]]@algorithm@funcName)]
          }
        } else {
          nams <- character(length(results$results))
          nam <- c("", names(results$results[[1]]@param)[1], "DEFAULT")
          for(i in 1:length(results$results)){
            m <- list(results$results[[i]])
            m[[2]] <- "all"
            if(gWidgets2::svalue(minBest)=="Overall min.")
              m[[3]] <- "overall.min"
            else
              m[[3]] <- "cond.min"
            names(m) <- nam
            ns[[i]] <- drop(do.call(qualMeas,m))
            nams[i] <- names(bA)[which(methodParse2==results$baselineTests[[i]]@algorithm@funcName)]
          }
        }
        plot(names(ns[[1]]),ns[[1]], type='l', xlab=nam[2], ylab=results$results[[1]]@qualMeasName)
        legend(x="topright", legend=nams, col=1:length(ns), lty=1)
        if(length(ns)>1){
          for(i in 2:length(ns)){
            lines(names(ns[[i]]),ns[[i]], col=i)
          }
        }
      }, container = mGroup)
      #	minBest <- gWidgets2::gradio(c("overall.min","cond.min"), horizontal=TRUE)
      result <- results$results[[1]]
      method <- names(bA)[which(methodParse2==results$baselineTests[[1]]@algorithm@funcName)]
      nameStrs <- c(names(result@param[1]), rownames(bA[[method]]))
      if(max(nchar(as.character(result@param[[nameStrs[1]]],scientific=TRUE)))>8){ # More than 8 digits => use scientific format
        minBest  <- gWidgets2::gradio(c("Overall min.","Min.", "Min. avg."), container = mGroup)
        minWhich <- gWidgets2::gcheckboxgroup(format(result@param[[nameStrs[1]]], checked=!logical(length(result@param[[nameStrs[1]]])), scientific=TRUE, digits=3), horizontal=TRUE, container = mGroup)
        # gWidgets2::enabled(minWhich) <- FALSE
      } else {
        minBest  <- gWidgets2::gradio(c("Overall min.","Min.", "Min. avg."), container = mGroup)
        minWhich <- gWidgets2::gcheckboxgroup(result@param[[nameStrs[1]]], checked=logical(length(result@param[[nameStrs[1]]])), horizontal=TRUE, container = mGroup)
        # gWidgets2::enabled(minWhich) <- FALSE
        }
      gWidgets2::addHandlerChanged(minBest, handler = function(h,...){
        if(gWidgets2::svalue(minBest,index=TRUE)==3){
          # gWidgets2::enabled(minWhich) <- TRUE
          gWidgets2::svalue(minWhich, index=TRUE) <- 1:length(result@param[[nameStrs[1]]])
        } else {
          # gWidgets2::enabled(minWhich) <- FALSE
          gWidgets2::svalue(minWhich, index=TRUE) <- integer()}
      })
      gWidgets2::addHandlerChanged(minWhich, handler = function(h,...){
        if(length(gWidgets2::svalue(minWhich, index=TRUE)) == 0 && gWidgets2::svalue(minBest,index=TRUE)==3){
          gWidgets2::enabled(curveButton) <- FALSE
        } else {
          gWidgets2::enabled(curveButton) <- TRUE}
      })
      
      # sGroup <- gWidgets2::ggroup(horizontal=FALSE, container = nb) # Moved up
      # mGroup <- gWidgets2::ggroup(horizontal=TRUE, container = sGroup)
      
      # Combine elements
      gWidgets2::addSpace(sGroup,10)
      gWidgets2::add(sGroup,compareLabel,expand=FALSE)
      gWidgets2::addSpace(sGroup,10)
      gWidgets2::add(mGroup,minBest,expand=FALSE)
      gWidgets2::add(mGroup,minWhich,expand=FALSE)
      gWidgets2::addSpace(mGroup,10)
      gWidgets2::add(mGroup,curveButton,expand=FALSE)
      gWidgets2::add(sGroup,mGroup,expand=FALSE)
      # gWidgets2::add(nb, sGroup, label="Compare")
      
      # Build GUI for algorithms
      for(i in 1:length(results$results)){
        nm <- which(methodParse2==results$baselineTests[[i]]@algorithm@funcName)
        addAlg(nm, results$results[[i]]);
      }
    } else {
      warning('Package lattice not installed')
      return(list())
    }
  } else {
    warning('Package gWidgets2 not installed')
    return(list())
  }
}
