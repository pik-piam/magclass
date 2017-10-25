#' Converts a report from one model to another
#' 
#' This function converts the content of a reporting file from one model to
#' another
#' 
#' The function converts data based on a region mapping and transformation
#' rules which are stored in the variable magclassdata which comes with this
#' library.
#' 
#' @param rep Report. Either the file name of a mif file or a report already
#' read in in R.
#' @param inmodel Model the input comes from. If NULL the script tries to
#' detect the inmodel automatically.
#' @param outmodel Model format the data should be converted to. Currently,
#' "MAgPIE" and "REMIND" are available
#' @param full Boolean deciding whether only the converted output should be
#' returned (FALSE) or the new output together with the input (TRUE)
#' @param as.list if TRUE a list is returned (default), if FALSE it is tried to
#' merge all information in one MAgPIE object (still under development and
#' works currently only if the entries for the different models and scenarios
#' have exactly the same regions and years).
#' @author Jan Philipp Dietrich
#' @seealso
#' \code{\link{read.report}},\code{\link{write.report}},\code{\link{magclassdata}}
#' @examples
#' 
#' \dontrun{convert.report("report.mif")}
#' 
#' @export convert.report
convert.report <- function(rep,inmodel=NULL,outmodel="MAgPIE",full=FALSE,as.list=TRUE) {
  # Commets would improve the code
  .convert <- function(input,inmodel=NULL,outmodel="MAgPIE",full=FALSE) {
    # load all region mappings available and check wether inmodel and outmodel are available
    map  <- magclassdata$map
    if(!(outmodel %in% names(map)))            stop("No existing transformation rules for output model \"",outmodel,"\"!",call.=FALSE)
    if(!(inmodel %in% names(map[[outmodel]]))) stop("No existing transformation rules for input model \"",inmodel,"\" in combination with output model \"",outmodel,"\"!",call.=FALSE)
    if(outmodel %in% names(input))             stop("Input already contains data for model \"",outmodel,"\"",call.=FALSE)
    # read regional mapping from inmodel regions to outmodel regions
    map <- map[[outmodel]][[inmodel]]
    if(!inmodel %in% names(input)) stop(paste0("The inmodel ",inmodel," is not available in the names of input: ",names(input)))
    mag <- input[[inmodel]]
    # add "GLO" if present in data
    if("GLO" %in% getRegions(mag)) map$GLO <- "GLO"
    # construct empty outmag object without region names and with variable names that are defined in trans and present in the input data
    outmag <- mag[rep(1,length(map)),,unlist(magclassdata$trans)[unlist(magclassdata$trans) %in% getNames(mag)]]
    outmag[,,] <- NA
    # set regions names to outmodel regions
    dimnames(outmag)[[1]] <- names(map)
    # transfer data
    # map[[reg]] refers to the inmodel region
    # reg refers to the outmodel region
    for(reg in names(map)) {
      #sum
      elem <- getNames(mag)[getNames(mag) %in% magclassdata$trans$sum]
      if(length(elem)>0) if(!is.na(map[[reg]])) suppressWarnings(outmag[reg,,elem] <- colSums(mag[map[[reg]],,elem]))
      #mean
      elem <- getNames(mag)[getNames(mag) %in% magclassdata$trans$mean]
      if(length(elem)>0) if(!is.na(map[[reg]])) suppressWarnings(outmag[reg,,elem] <- colMeans(mag[map[[reg]],,elem]))    
    }
    out <- list();
    if(full) out[[inmodel]]=mag;
    out[[outmodel]]=outmag;
    return(out)
  }
  
  if(is.character(rep)) rep <- read.report(rep)
  if(is.null(inmodel)) {
    if(length(names(rep[[1]]))==1) inmodel <- names(rep[[1]])
    else stop("Not clear which model should be used as input!")
  }
  
  # convert data from inmodel to outmodel
  if(inmodel!=outmodel) {
    rep <- lapply(rep,.convert,inmodel,outmodel,full)  
  } 
  
  if(!as.list) {
    for(scenario in names(rep)) {
      for(model in names(rep[[scenario]])) {
        getNames(rep[[scenario]][[model]]) <- paste(scenario,model,getNames(rep[[scenario]][[model]]),sep=".")
      }
    }
    rep <- mbind(unlist(rep,recursive=FALSE))
    names(dimnames(rep))[3] <- "scenario.model.value"
  }
  return(rep)  
}
