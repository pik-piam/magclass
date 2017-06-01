#' convergence
#' 
#' Cross-Fades the values of one MAGPIE object into the values of another over
#' a certain time
#' 
#' 
#' @param origin an object with one name-column
#' @param aim Can be twofold: An magpie object or a numeric value.
#' @param start_year year in which the convergence from origin to aim starts.
#' If set to NULL the the first year of aim is used as start_year
#' @param end_year year in which the convergence from origin to aim shall be
#' (nearly) reached. If set to NULL the the last year of aim is used as
#' end_year.
#' @param direction NULL, "up" or "down". NULL means normal convergence in both
#' directions, "up" is only a convergence if origin<aim, "down" means only a
#' convergence if origin>aim
#' @param type "smooth", "s", "linear" or "decay". Describes the type of
#' convergence: linear means a linear conversion , s is an s-curve which starts
#' from origin in start_year and reaches aim precisely in end_year. After 50
#' percent of the convergence time, it reaches about the middle of the two
#' values. Its based on the function min(1, pos^4/(0.07+pos^4)*1.07) smooth is
#' a conversion based on the function x^3/(0.1+x^3).  In the latter case only
#' 90\% of convergence will be reached in the end year, because full
#' convergence is reached in infinity.  decay is a conversion based on the
#' function x/(1.5 + x)*2.5.
#' @param par parameter value for convergence function; currently only used for
#' type="decay"
#' @return returns a time-series with the same timesteps as origin, which
#' lineary fades into the values of the aim object
#' @author Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{lin.convergence}}
#' @examples
#' 
#' data(population_magpie)
#' population <- add_columns(population_magpie,"MIX")
#' population[,,"MIX"]<-convergence(population[,,"A2"],population[,,"B1"])
#' 
#' 
#' @export convergence
convergence <- function(origin, aim, start_year=NULL, end_year=NULL,
                        direction=NULL, type="smooth", par=1.5) {

  ### Basic checks ###
  if(!is.magpie(origin)) stop("origin is no magpie object")

  if(is.null(dim(aim))) aim<-as.magpie(array(aim,dim(origin),dimnames(origin)))
  if(!is.magpie(aim)) stop("aim is no magpie object")

  if (all(dimnames(aim)[[1]]!=dimnames(origin)[[1]]))
    stop("regions have to be the same")

  if (ndata(origin)!=1 & !identical(getNames(origin), getNames(aim)))
    stop("If there ist more than one name-column, dimnames have to be the same")

  if(nyears(aim)==1) {
    tmp <- setYears(aim,NULL)
    aim <- origin
    aim[,,] <- tmp
    rm(tmp)
  }
  
  if(nyears(origin)==1) {
    tmp <- setYears(origin,NULL)
    origin <- aim
    origin[,,] <- tmp
    rm(tmp)
  }

  if (any(getYears(aim) != getYears(origin)))
    stop("Objects need the same timesteps, or aim has to have only one timestep")

  if (is.null(start_year)) start_year <- getYears(aim)[1]
  if (is.null(end_year)) end_year <- getYears(aim)[nyears(aim)]

  if(isYear(start_year,with_y=TRUE)) start_year <- substr(start_year,2,5)
  if(isYear(end_year,with_y=TRUE))   end_year   <- substr(end_year,2,5)
  start_year <- as.numeric(start_year)
  end_year   <- as.numeric(end_year)
  if(!isYear(start_year,with_y=FALSE)) stop("wrong year format for convergence aim")
  if(!isYear(end_year,with_y=FALSE)) stop("wrong year format for convergence aim")


  # In the case of direction up or down data should only be manipulated in one
  # direction. Therefor, the aim object is manipulated accordingly
  if(!is.null(direction)) {
    aim <- as.array(aim)
    if(direction=="up")   aim[which(aim<origin)] <- as.array(origin)[which(aim<origin)]
    else if(direction=="down") aim[which(aim>origin)] <- as.array(origin)[which(aim>origin)]
    else stop("Illegal direction setting, only up and down are allowed arguments!")
    aim <- as.magpie(aim)
  }                                                                                            
                        
  years<-new.magpie("GLO",getYears(origin),NULL,getYears(origin,as.integer=TRUE))                                                 
  pos <- (years - start_year)/(end_year - start_year)
  pos[pos<0] <- 0
  pos[pos>1] <- 1
  if (type == "linear") { mix <- pos 
  } else if (type == "s") { mix <- pos^4/(0.07+pos^4)*1.07
  } else if (type == "smooth") {mix <- pos^3/(0.1 + pos^3)
#  } else if (type == "decay") {mix <- pos/(0.5 + pos)*1.5
  } else if (type == "decay") {mix <- pos/(par + pos)*(par+1)
  } else {stop("type does not exist")}
  converged <- aim * mix + origin * (1 - mix)

  return(converged)
}
