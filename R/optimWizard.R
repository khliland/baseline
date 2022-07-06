### $Id: optimWizard.R 193 2012-06-24 21:13:42Z kristl $



#' @title Visual tool for setting up optimization
#' 
#' @description Set up optimization through a graphical user interface. Optionally
#' collecting values directly from 'baselineGUI'.  Retrieve optimisation
#' parameters and results with \code{getOptim} and \code{getOptimRes},
#' respectively.
#' 
#' 
#' @aliases optimWizard getOptim getOptimRes
#' @param X Matrix with spectra in rows
#' @param y Response vector or matrix in analysis
#' @param postproc Custum function for post processing of spectra (optional)
#' @param predictionTest Custom prediction object (optional)
#' @param cvsegments Cross-validation segments (optional)
#' @author Kristian Hovde Liland and Bjørn-Helge Mevik
#' @keywords baseline spectra
#' @examples
#' 
#' \dontrun{
#' # Computationally intensive
#' data(milk)
#' X <- milk$spectra[,-1]
#' y <- milk$spectra[,1]
#' optimWizard(X,y)
#' 
#' # Retrieve optimisation
#' myResults <- getOptimRes()
#' 
#' # After optimisation is complete
#' plotOptim(myResults)
#' }
#' @export
optimWizard <- function(X, y, postproc, predictionTest, cvsegments){
  ## Organize optimization through GUI
  
  if(missing(X))
    stop('No data specified')
  if(missing(y))
    stop('No response specified')
  if(missing(predictionTest)){
    rr <- FALSE
    predictionTest <- NULL}
  else
    rr <- TRUE
  if(missing(postproc)){
    pp <- FALSE
    postproc <- NULL}
  else
    pp <- TRUE
  if(missing(cvsegments)){
    cc <- FALSE
    cvsegments <- NULL}
  else
    cc <- TRUE
  bltest <- NULL
  
  if(requireNamespace("gWidgets2", quietly = TRUE)){
    if(requireNamespace("pls", quietly = TRUE)){
      
      # Set up main window with internal container
      win  <- gWidgets2::gwindow("Optimisation wizard", width=450, height=300)
      main <- gWidgets2::ggroup(horizontal=FALSE, container=win)
      nb   <- gWidgets2::gnotebook(container=main)
      gWidgets2::add(win,main)
      
      # Notebook containing settings and baseline correction methods, group initialization
      # nb <- gWidgets2::gnotebook() # Moved up
      sGroup <- gWidgets2::ggroup(horizontal=TRUE, container=nb, label='Settings')
      sGroup1 <- gWidgets2::ggroup(horizontal=FALSE, container = sGroup)
      # size(sGroup1) <- c(400,300)
      sGroup2 <- gWidgets2::gframe("Optimisation", horizontal=FALSE, container = sGroup)
      # size(sGroup2) <- c(170,300)
      methGroup <- gWidgets2::ggroup(horizontal=TRUE, container = sGroup1)
      methFrame <- gWidgets2::gframe('Baseline correction', horizontal=FALSE, container = methGroup)
      normGroup <- gWidgets2::ggroup(horizontal=TRUE, container = sGroup1)
      normFrame <- gWidgets2::gframe('Post processing', horizontal=FALSE, container = normGroup)
      reggroup <- gWidgets2::ggroup(horizontal=TRUE, container = sGroup1)
      regframe <- gWidgets2::gframe('Analysis and quality measure', horizontal=FALSE, container = reggroup)
      verbCheck <- gWidgets2::gcheckbox("Verbose", checked=TRUE, container = sGroup2)
      
      
      # Initialize parameters and parameter lists
      nAlgs <- 0
      used <- numeric(0)
      if(exists("baselineAlgorithmsGUI",envir=.GlobalEnv)){
        bAGUI <- get("baselineAlgorithmsGUI",envir=.GlobalEnv)
      } else {
        bAGUI <- baselineAlgorithmsGUI
      }
      GUI.names <- sort(names(bAGUI))
      if(exists("baselineAlgorithms",envir=.GlobalEnv)){
        bA <- get("baselineAlgorithms",envir=.GlobalEnv)
      } else {
        bA <- baselineAlgorithms
      }
      genChoosers <- rGroups <- groups <- removes <- parameterGroup <- parameterList <- method <- list()
      
      
      # Function for adding a baseline correction method, including sub-functions and variables
      addAlg <- function(nm){
        # Initialize parameters and groups
        nAlgs <<- nAlgs + 1
        used[nAlgs] <<- 1
        groups[[nAlgs]] <<- gWidgets2::ggroup(horizontal=FALSE, container=nb, label=nm)
        rGroups[[nAlgs]] <<- gWidgets2::ggroup(horizontal=TRUE, container=groups[[nAlgs]])
        #    name <- names(bA)[nm]
        name <- nm
        nameLong <- bA[[name]]@description
        method[[nAlgs]] <<- name
        
        # Add some space and the name of the baseline correction algorithm
        gWidgets2::addSpace(groups[[nAlgs]],20)
        gWidgets2::add(groups[[nAlgs]],gWidgets2::glabel(nameLong, container = groups[[nAlgs]]))
        gWidgets2::addSpace(groups[[nAlgs]],10)
        
        # Function for setting up parameter array in GUI together with buttons and functions
        addParameterGroup <- function(nameStr, lineNo){
          genChoosers[[nAlgs]][[lineNo]] <<- gcombobox(c("-> Generate", "Linear","Exponential"), selected=1, container = parameterGroup[[nAlgs]])
          gWidgets2::tag(genChoosers[[nAlgs]][[lineNo]],"no") <<- lineNo
          gWidgets2::tag(genChoosers[[nAlgs]][[lineNo]],"name") <<- nameStr
          parameterList[[nAlgs]][[lineNo]]  <<- c(gWidgets2::gedit(text = "", width=1,coerce.with=as.numeric,container=parameterGroup[[nAlgs]]),gWidgets2::gedit(width=5,coerce.with=as.numeric,container=parameterGroup[[nAlgs]]),gWidgets2::gedit(width=10,coerce.with=as.numeric,container=parameterGroup[[nAlgs]]),gWidgets2::gedit(width=15,container=parameterGroup[[nAlgs]]),genChoosers[[nAlgs]][[lineNo]])
          # Generate sequence based on 'From', 'To', 'Steps' and choice from droplist
          gWidgets2::addHandlerChanged(parameterList[[nAlgs]][[lineNo]][[5]], handler = function(h,...){
            if((type <- gWidgets2::svalue(genChoosers[[nAlgs]][[lineNo]],index=TRUE))>1){
              # type <- gWidgets2::svalue(h$obj,index=TRUE)-1
              type <- type - 1
              gWidgets2::svalue(genChoosers[[nAlgs]][[lineNo]],index=TRUE) <<- 1
              linNo <- lineNo
              # linNo <- gWidgets2::tag(h$obj)$no
              if(is.finite(gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[1]])) && is.finite(gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[2]])) && is.finite(gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[3]]))){
                if(type==1){
                  linSeq <- seq(gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[1]]),gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[2]]),length.out=gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[3]]))
                  linOut <- linSeq[1]
                  if(length(linSeq)>1)
                    for(i in 2:length(linSeq))
                      linOut <- paste(linOut, linSeq[i], sep=", ")
                  gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[4]]) <- linOut
                }
                if(type==2){
                  linSeq <- exp(seq(log(gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[1]])),log(gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[2]])),length.out=gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[3]])))
                  linOut <- linSeq[1]
                  if(length(linSeq)>1)
                    for(i in 2:length(linSeq))
                      linOut <- paste(linOut, linSeq[i], sep=", ")
                  gWidgets2::svalue(parameterList[[nAlgs]][[linNo]][[4]]) <- linOut
                }
              } else {
                gmessage("Missing value(s) in 'From', 'To' or 'Steps'", title="Sequence", icon = "warning")
              }
            }
          })
          # Set up visual optimization array for parameters
          parameterGroup[[nAlgs]][lineNo+1,1] <<- nameStr
          # parameterGroup[[nAlgs]][lineNo+1,1] <<- gWidgets2::glabel(text=nameStr)
          size(parameterList[[nAlgs]][[lineNo]][[1]]) <<- 28
          size(parameterList[[nAlgs]][[lineNo]][[2]]) <<- 28
          size(parameterList[[nAlgs]][[lineNo]][[3]]) <<- 28
          size(parameterList[[nAlgs]][[lineNo]][[4]]) <<- 28
          size(parameterList[[nAlgs]][[lineNo]][[5]]) <<- 28
          # size(parameterList[[nAlgs]][[lineNo]][[1]]) <<- c(60,28)
          # size(parameterList[[nAlgs]][[lineNo]][[2]]) <<- c(60,28)
          # size(parameterList[[nAlgs]][[lineNo]][[3]]) <<- c(20,28)
          # size(parameterList[[nAlgs]][[lineNo]][[4]]) <<- c(220,28)
          # size(parameterList[[nAlgs]][[lineNo]][[5]]) <<- c(120,28)
          parameterGroup[[nAlgs]][lineNo+1,2] <<- parameterList[[nAlgs]][[lineNo]][[1]]
          parameterGroup[[nAlgs]][lineNo+1,3] <<- parameterList[[nAlgs]][[lineNo]][[2]]
          parameterGroup[[nAlgs]][lineNo+1,4] <<- parameterList[[nAlgs]][[lineNo]][[3]]
          parameterGroup[[nAlgs]][lineNo+1,5] <<- parameterList[[nAlgs]][[lineNo]][[4]]
          parameterGroup[[nAlgs]][lineNo+1,6] <<- parameterList[[nAlgs]][[lineNo]][[5]]
        }
        
        # Set up visual optimization array for parameters
        parameterList[[nAlgs]] <<- genChoosers[[nAlgs]] <<- list()
        parameterGroup[[nAlgs]] <<- gWidgets2::glayout(homogeneous = FALSE, spacing = 5, container=groups[[nAlgs]])
        parameterGroup[[nAlgs]][1,1] <<- ""
        parameterGroup[[nAlgs]][1,2] <<- "From:"
        parameterGroup[[nAlgs]][1,3] <<- "To:"
        parameterGroup[[nAlgs]][1,4] <<- "Steps:"
        parameterGroup[[nAlgs]][1,5] <<- "Sequence:"
        # parameterGroup[[nAlgs]][1,1] <<- gWidgets2::glabel("")
        # parameterGroup[[nAlgs]][1,2] <<- gWidgets2::glabel("From:")
        # parameterGroup[[nAlgs]][1,3] <<- gWidgets2::glabel("To:")
        # parameterGroup[[nAlgs]][1,4] <<- gWidgets2::glabel("Steps:")
        # parameterGroup[[nAlgs]][1,5] <<- gWidgets2::glabel("Sequence:")
        nameStrs <- rownames(bAGUI[[method[[nAlgs]]]])
        lns <- length(nameStrs)
        for(i in 1:lns)
          addParameterGroup(nameStrs[i],i)
        parameterGroup[[nAlgs]][lns+2,2] <- gWidgets2::gbutton("Collect", handler = function(h,...){
          #      if(exists("baseline.current")){
          if(getBaselineEnv("baseline.current")$method == name){
            for(i in 1:lns){
              gWidgets2::svalue(parameterList[[nAlgs]][[i]][[1]]) <- getBaselineEnv("baseline.current")$parValues[i]
            }
          } else {
            gmessage(paste("'baseline.current$method' is not equal to '", name, "'", sep=""), title="Sequence", icon = "warning")
          }
          #      } else {
          #        gmessage("'baseline.current' not found", title="Sequence", icon = "warning")
          #      }
        }, container = parameterGroup[[nAlgs]])
        parameterGroup[[nAlgs]][lns+2,3] <- gWidgets2::gbutton("Collect", handler = function(h,...){
          if(getBaselineEnv("baseline.current")$method == name){
            for(i in 1:lns){
              gWidgets2::svalue(parameterList[[nAlgs]][[i]][[2]]) <- getBaselineEnv("baseline.current")$parValues[i]
            }
          } else {
            gmessage(paste("'baseline.current$method' is not equal to '", name, "'", sep=""), title="Sequence", icon = "warning")
          }
        }, container = parameterGroup[[nAlgs]])
        # parameterGroup[[nAlgs]][lns+2,4] <- gWidgets2::glabel("<- from current algorithm in baselineGUI")
        parameterGroup[[nAlgs]][lns+2,4] <- "<- from current algorithm in baselineGUI"
        
        # Button for removal of method and adding the method to the main window's notebook
        removes[[nAlgs]] <<- gWidgets2::gbutton("Remove baseline correction method", container = rGroups[[nAlgs]])
        gWidgets2::tag(removes[[nAlgs]],"nAlg") <<- nAlgs
        addHandlerClicked(removes[[nAlgs]], handler = function(h,...){
          nAlg <- nAlgs
#          nAlg <- gWidgets2::tag(h$obj)$nAlg
          gWidgets2::dispose(nb)
          used[nAlg] <<- 0
        })
        gWidgets2::add(rGroups[[nAlgs]],removes[[nAlgs]],expand=FALSE)
        gWidgets2::addSpace(groups[[nAlgs]],20)
        gWidgets2::add(groups[[nAlgs]],rGroups[[nAlgs]],expand=FALSE)
        names(groups[[nAlgs]]) <- name
#        gWidgets2::add(nb,groups[[nAlgs]])
        # gWidgets2::visible(parameterGroup[[nAlgs]]) <- TRUE
      }
      
      # Droplists for adding a baseline correction method and choosing post processing
      namesG <- character(length(GUI.names)+1)
      namesG[1] <- '-> Choose method for optimisation'
      for(i in 1:length(GUI.names)){ # Let bAGUI control, and bA have descriptions -------------
                                     namesG[i+1] <- paste("'", ifelse(is.null(bA[[GUI.names[i]]]@description),"",bA[[GUI.names[i]]]@description), " (", GUI.names[i], ")'", sep="")
      }
      methodChooser <- gcombobox(namesG,
                                 selected=1, handler = function(h,...){if(gWidgets2::svalue(methodChooser,index=TRUE)>1) addAlg(GUI.names[gWidgets2::svalue(methodChooser,index=TRUE)-1]); gWidgets2::svalue(methodChooser,index=TRUE)<-1}, container = methFrame)
      postChooser <- gcombobox(c("None","Norm (L2)", "Mean", "Median",
                                 "Sum", "Sum of squares", "L1 postproc", "Maximum"),	selected=1, container = normFrame)
      regChosen <- FALSE
      regOutGroup <- segChooser <- segNumber <- regParam <- numeric(0)
      regFrom <- regTo <- regSteps <- lambdaSequence <- numeric(0)
      # Analysis droplist
      regChooser <- gcombobox(c("-> Choose an analysis","PLSR / RMSEP","Ridge Regression / RMSEP"),	selected=1, handler = function(h,...){
        if(regChosen == TRUE){
          gWidgets2::delete(regframe, regOutGroup) # Remove old analysis if chosen
        }
        if(gWidgets2::svalue(regChooser,index=TRUE)>1){ # Analysis chosen
          regOutGroup <<- gWidgets2::ggroup(horizontal=TRUE, container = regframe)
          regIntGroupB <- gWidgets2::ggroup(horizontal=FALSE, container = regOutGroup)
          regIntGroupA <- gWidgets2::ggroup(horizontal=TRUE, container = regIntGroupB)
          regIntGroup1 <- gWidgets2::ggroup(horizontal=FALSE, container = regIntGroupA)
          regIntGroup2 <- gWidgets2::ggroup(horizontal=FALSE, container = regIntGroupA)
          regIntGroup3 <- gWidgets2::ggroup(horizontal=FALSE, container = regIntGroupA)
          regIntGroup4 <- gWidgets2::ggroup(horizontal=TRUE, container = regIntGroupA)
          regIntGroup5 <- gWidgets2::ggroup(horizontal=FALSE, container = regIntGroupB)
          if(gWidgets2::svalue(regChooser,index=TRUE)==2){ # Display extra parameters for PLSR
            if(cc)
              segChooser <<- gcombobox(c("random", "consecutive", "interleaved","custom"), selected=4, handler = function(h,...){
                if(gWidgets2::svalue(segChooser, index=TRUE)==4){
                  gWidgets2::svalue(segNumber) <- length(cvsegments)
                  gWidgets2::enabled(segNumber) <- FALSE
                } else
                  gWidgets2::enabled(segNumber) <- TRUE
              }, container = regIntGroup1)
            else
              segChooser <<- gcombobox(c("random", "consecutive", "interleaved"), selected=1, container = regIntGroup1)
            segNumber <<- gWidgets2::gedit("10", container = regIntGroup2)
            regParam <<- gWidgets2::gedit(container = regIntGroup3)
            gWidgets2::add(regIntGroup1, gWidgets2::glabel("CV segment type", container = regIntGroup1), expand=FALSE)
            gWidgets2::add(regIntGroup1, segChooser, expand=FALSE)
            if(cc){
              # gWidgets2::add(regIntGroup1, gWidgets2::glabel(length(cvsegments)), expand=FALSE)
              gWidgets2::svalue(segNumber) <- length(cvsegments)
              gWidgets2::enabled(segNumber) <- FALSE
            }
            gWidgets2::add(regIntGroup2, gWidgets2::glabel("Number of segments", container = regIntGroup2), expand=FALSE)
            gWidgets2::add(regIntGroup2, segNumber, expand=FALSE)
            gWidgets2::add(regIntGroup3, gWidgets2::glabel("Number of components", container = regIntGroup3), expand=FALSE)
            gWidgets2::add(regIntGroup3, regParam, expand=FALSE)
            gWidgets2::add(regOutGroup, regIntGroup1, expand=FALSE)
            gWidgets2::add(regOutGroup, regIntGroup2, expand=FALSE)
            gWidgets2::add(regOutGroup, regIntGroup3, expand=FALSE)
          }
          if(gWidgets2::svalue(regChooser,index=TRUE)==3){ # Display extra parameters for Ridge regression
            regFrom <<- gWidgets2::gedit(coerce.with=as.numeric, container = regIntGroup1)
#            size(regFrom) <<- c(60,24)
            regTo <<- gWidgets2::gedit(coerce.with=as.numeric, container = regIntGroup2)
#            size(regTo) <<- c(60,24)
            regSteps <<- gWidgets2::gedit(coerce.with=as.numeric, container = regIntGroup3)
#            size(regSteps) <<- c(60,24)
            lambdaSequence <<- gcombobox(c("-> Generate", "Linear","Exponential"), container = regIntGroup4)
            # Generate sequence based on 'From', 'To', 'Steps' and choice from droplist
            gWidgets2::addHandlerChanged(lambdaSequence, handler = function(h,...){
              if(gWidgets2::svalue(lambdaSequence,index=TRUE)>1){
                type <- gWidgets2::svalue(lambdaSequence,index=TRUE)-1
                gWidgets2::svalue(lambdaSequence,index=TRUE) <<- 1
                if(is.finite(gWidgets2::svalue(regFrom)) && is.finite(gWidgets2::svalue(regTo)) && is.finite(gWidgets2::svalue(regSteps))){
                  if(type==1){
                    linSeq <- seq(gWidgets2::svalue(regFrom),gWidgets2::svalue(regTo),length.out=gWidgets2::svalue(regSteps))
                    linOut <- linSeq[1]
                    if(length(linSeq)>1)
                      for(i in 2:length(linSeq))
                        linOut <- paste(linOut, linSeq[i], sep=", ")
                    gWidgets2::svalue(regParam) <- linOut
                  }
                  if(type==2){
                    linSeq <- exp(seq(log(gWidgets2::svalue(regFrom)),log(gWidgets2::svalue(regTo)),length.out=gWidgets2::svalue(regSteps)))
                    linOut <- linSeq[1]
                    if(length(linSeq)>1)
                      for(i in 2:length(linSeq))
                        linOut <- paste(linOut, linSeq[i], sep=", ")
                    gWidgets2::svalue(regParam) <- linOut
                  }
                } else {
                  gmessage("Missing value(s) in 'From', 'To' or 'Steps'", title="Sequence", icon = "warning")
                }
              }
            })
            #paramLabel <- gWidgets2::glabel("Ridge parameter")
            regParam <<- gWidgets2::gedit(container = regIntGroup5)
            gWidgets2::add(regIntGroup1, gWidgets2::glabel("From", container = regIntGroup1), expand=FALSE)
            gWidgets2::add(regIntGroup1, regFrom, expand=FALSE)
            gWidgets2::add(regIntGroup2, gWidgets2::glabel("To", container = regIntGroup2), expand=FALSE)
            gWidgets2::add(regIntGroup2, regTo, expand=FALSE)
            gWidgets2::add(regIntGroup3, gWidgets2::glabel("Steps", container = regIntGroup3), expand=FALSE)
            gWidgets2::add(regIntGroup3, regSteps, expand=FALSE)
            gWidgets2::add(regIntGroup4, lambdaSequence, expand=FALSE)
            gWidgets2::add(regIntGroup5, gWidgets2::glabel("Lambda sequence", container = regIntGroup5), expand=FALSE)
            gWidgets2::add(regIntGroup5, regParam, expand=FALSE)
            gWidgets2::add(regIntGroupA, regIntGroup1, expand=FALSE)
            gWidgets2::add(regIntGroupA, regIntGroup2, expand=FALSE)
            gWidgets2::add(regIntGroupA, regIntGroup3, expand=FALSE)
            gWidgets2::add(regIntGroupA, regIntGroup4, expand=FALSE)
            gWidgets2::add(regIntGroupB, regIntGroupA, expand=FALSE)
            gWidgets2::add(regIntGroupB, regIntGroup5, expand=FALSE)
            gWidgets2::add(regOutGroup, regIntGroupB, expand=FALSE)
          }
          gWidgets2::add(regframe,regOutGroup,expand=FALSE)
          regChosen <<- TRUE
        }
      }, container = regframe)
      

      # Verification of optimization parameters
      verifyButton <- gWidgets2::gbutton("Verify setup", handler = function(h,...){
        gWidgets2::enabled(saveButton) <- FALSE
        gWidgets2::enabled(startButton) <- FALSE
        # Check basic settings for optimisation
        options(warn=-1)
        if(gWidgets2::svalue(regChooser, index=TRUE)>1)
          rp <- as.numeric(strsplit(gWidgets2::svalue(regParam), c(","))[[1]])
        else
          rp <- NA
        options(warn=0)
        if(sum(used)==0){
          gmessage("No baseline correction algorithm chosen", title="Not ready", icon = "warning")
        } else if(gWidgets2::svalue(regChooser, index=TRUE)==1){
          gmessage("No analysis chosen", title="Not ready", icon = "warning")
        } else if(!rr && nchar(gWidgets2::svalue(regParam))==0){
          gmessage("Regression parameter not specified", title="Not ready", icon = "warning")
        } else if(!rr && (!is.finite(sum(rp)))){
          gmessage("Regression parameter incorrectly specified", title="Not ready", icon = "warning")
        } else {
          u <- 0
          faulty <- ""
          for(i in 1:nAlgs){
            if(used[i] == 1){
              for(j in 1:length(parameterList[[i]])){
                m <- as.numeric(strsplit(gWidgets2::svalue(parameterList[[i]][[j]][[4]]), c(","))[[1]])
                if((length(m)==0 || sum(is.na(m))>0) && u==0){ # Where did error occur?
                  u <- u+1
                  faulty <- paste((rownames(bAGUI[[method[[i]]]])[j]), "of baseline correction algorithm", method[[i]])
                }
              }
            }
          }
          if(u==0){
            # Collect data for analysis
            if(!rr){
              if(gWidgets2::svalue(regChooser, index=TRUE) == 2){
                if(cc)
                  predictionTest <<- new("PLSRTest", ncomp = as.numeric(gWidgets2::svalue(regParam)), cvsegments = pls::cvsegments)
                else
                  predictionTest <<- new("PLSRTest", ncomp = as.numeric(gWidgets2::svalue(regParam)), cvsegments = pls::cvsegments(dim(X)[1], as.numeric(gWidgets2::svalue(segNumber)), type=gWidgets2::svalue(segChooser)))
              } else if(gWidgets2::svalue(regChooser, index=TRUE) == 3){
                predictionTest <<- new("ridgeRegressionTest", lambda = as.numeric(strsplit(gWidgets2::svalue(regParam), c(","))[[1]]))
              }
            }
            bltest <<- list()
            q <- 0
            for(i in 1:nAlgs){
              if(used[i] == 1){
                q <- q+1
                params <- list()
                parNames <- character(length(parameterList[[i]]))
                for(j in 1:length(parameterList[[i]])){
                  params[[j]] <- as.numeric(strsplit(gWidgets2::svalue(parameterList[[i]][[j]][[4]]), c(","))[[1]])
                  parNames[j] <- rownames(bAGUI[[method[[i]]]])[j]
                }
                names(params) <- parNames
                bltest[[q]] <<- new("baselineAlgTest", algorithm = bA[[method[[i]]]],
                                    param = params)
              }
            }
            # Choice of normalisation
            if(!pp){
              if(gWidgets2::svalue(postChooser, index=TRUE)==1)
                postproc <<- NULL
              else {
                if(gWidgets2::svalue(postChooser, index=TRUE)==2) # Norm (L2)
                  postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/sqrt(X[i,]%*%X[i,])};X}
                if(gWidgets2::svalue(postChooser, index=TRUE)==3) # Mean
                  postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/mean(X[i,])};X}
                if(gWidgets2::svalue(postChooser, index=TRUE)==4) # Median
                  postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/median(X[i,])};X}
                if(gWidgets2::svalue(postChooser, index=TRUE)==5) # Sum
                  postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/sum(X[i,])};X}
                if(gWidgets2::svalue(postChooser, index=TRUE)==6) # Sum of squares
                  postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/(X[i,]%*%X[i,])};X}
                if(gWidgets2::svalue(postChooser, index=TRUE)==7) # L1 postproc
                  postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/sum(abs(X[i,]))};X}
                if(gWidgets2::svalue(postChooser, index=TRUE)==8) # Maximum
                  postproc <<- function(X){ for(i in 1:dim(X)[1]){ X[i,] <- X[i,]/max(X[i,])};X}
              }
            }
            gWidgets2::enabled(saveButton) <- TRUE
            gWidgets2::enabled(startButton) <- TRUE
          } else
            gmessage(paste("Check parameter",faulty), title="Not ready", icon = "warning")
        }
      }, container = sGroup2)
      saveButton <- gWidgets2::gbutton("Save setup", handler = function(h,...){
        putBaselineEnv("bltests", bltest)
        putBaselineEnv("predictionTest", predictionTest)
        putBaselineEnv("postproc", postproc)
        cat(paste("\n# To run optimization later:\nopts <- getOptim()\noptimRes <- doOptim(opts$bltests, X, y, opts$predictionTest,\n        postproc = opts$postproc, verbose =", gWidgets2::svalue(verbCheck), ", cleanTmp = TRUE)\n"))
      }, container = sGroup2)
      startButton <- gWidgets2::gbutton("START", handler = function(h,...){
        # Run optimisation
        putBaselineEnv("optimRes", doOptim(bltest, X, y, predictionTest,
                                           postproc = postproc, verbose = gWidgets2::svalue(verbCheck),
                                           cleanTmp = TRUE))
        cat("# To retrieve optimisation results later:\nmyResults <- getOptimRes()")
      }, container = sGroup2)
      gWidgets2::enabled(saveButton) <- FALSE
      gWidgets2::enabled(startButton) <- FALSE
      
      # Settings for correction, normalization and analysis
      gWidgets2::addSpace(sGroup1, 10)
      gWidgets2::add(methFrame, methodChooser, expand=FALSE)
      gWidgets2::add(methGroup, methFrame, expand=FALSE)
      gWidgets2::add(sGroup1, methGroup, expand=FALSE)
      gWidgets2::addSpace(sGroup1, 15)
      if(!pp)
        gWidgets2::add(normFrame, postChooser, expand=FALSE)
      else
        gWidgets2::add(normFrame, gWidgets2::glabel("User specified", container = normFrame), expand=FALSE)
      gWidgets2::add(normGroup, normFrame, expand=FALSE)
      gWidgets2::add(sGroup1, normGroup, expand=FALSE)
      gWidgets2::addSpace(sGroup1, 15)
      if(!rr)
        gWidgets2::add(regframe, regChooser, expand=FALSE)
      else
        gWidgets2::add(normFrame, gWidgets2::glabel("User specified", container = normFrame), expand=FALSE)
      gWidgets2::add(reggroup, regframe, expand=FALSE)
      gWidgets2::add(sGroup1, reggroup, expand=FALSE)
      
      # Settings for optimization
      gWidgets2::addSpace(sGroup2, 10)
      gWidgets2::add(sGroup2, verbCheck, expand=FALSE)
      gWidgets2::addSpace(sGroup2, 30)
      gWidgets2::add(sGroup2, verifyButton, expand=FALSE)
      gWidgets2::addSpace(sGroup2, 30)
      gWidgets2::add(sGroup2, saveButton, expand=FALSE)
      gWidgets2::addSpace(sGroup2, 10)
      gWidgets2::add(sGroup2, startButton, expand=FALSE)
      
      gWidgets2::add(sGroup, sGroup1)
      gWidgets2::addSpace(sGroup, 30)
      gWidgets2::add(sGroup, sGroup2)
      #gWidgets2::add(nb, sGroup, label="Settings")
      gWidgets2::add(main,nb,expand=TRUE)
      # gWidgets2::add(main,gstatusbar("Tester statusbar"))
    } else {
      warning('Package pls not installed')
      return(list())
    }
  } else {
    warning('Package gWidgets2 not installed')
    return(list())
  }
}

getOptim <- function(){
  list(bltests        = getBaselineEnv("bltests"),
       predictionTest = getBaselineEnv("predictionTest"),
       postproc       = getBaselineEnv("postproc"))
}
getOptimRes <- function(){
  getBaselineEnv("optimRes")
}
