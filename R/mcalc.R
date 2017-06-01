#' mcalc
#' 
#' Select values from a MAgPIE-object
#' 
#' This functions only work for MAgPIE objects with named dimensions as the
#' dimension name (set_name) has to be used to indicate in which dimension the
#' entries should be searched for!
#' 
#' @aliases mcalc mcalc<-
#' @param x MAgPIE object
#' @param f A formula describing the calculation that should be performed
#' @param dim The dimension in which the manipulation should take place. If set
#' to NULL function tries to detect the dimension automatically.
#' @param append If set to TRUE the result will be appended to x, otherwise the
#' result will be returned.
#' @return The calculated MAgPIE object in the case that append is set to
#' FALSE. Otherwise nothing is returned (as x is appended in place)
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{mselect}}
#' @examples
#' 
#'  data(population_magpie)
#'  population_magpie
#'  mcalc(population_magpie,X12 ~ A2*B1,append=TRUE)
#'  population_magpie
#'  mcalc(population_magpie,`Nearly B1` ~ 0.5*A2 + 99.5*B1)
#'  
#' 
#' @export mcalc
#' @importFrom stats as.formula
mcalc <- function(x,f,dim=NULL,append=FALSE) {
  x <- clean_magpie(x)
  f <- as.formula(f)
  vars <- all.vars(f[[3]])
  
  if(is.null(dim)) dim <- getDim(vars,x)
  
  for(v in vars) {
    l <- list()
    l[dim] <- v
    tmp <- mselect(x,l,collapseNames=FALSE)
    getNames(tmp,dim=dim) <- all.vars(f[[2]])
    assign(v,tmp) 
  }
  if(append) {
    assign(as.character(as.list(match.call())$x),mbind(x,eval(f[[3]])),envir =  parent.frame()) 
  } else {
    return(eval(f[[3]]))
  }
}
