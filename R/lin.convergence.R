#' lin.convergence
#' 
#' Cross-Fades the values of one MAGPIE object into the values of another over
#' a certain time
#' 
#' 
#' @param origin an object with one name-column
#' @param aim Can be twofold: An object with one name-column and the same
#' timesteps as origin. Then the model fades over from timestep 1, in which the
#' value of origin is valid, to the last timestep, n which the value of aim is
#' valid. In the second case, the aim object has to have only one timestep,
#' which is also in origin. Then, the data will be faded from the value of
#' origin in the first timestep to the value of aim in the timestep passed on
#' by aim.
#' @param convergence_time_steps In the case of
#' timesteps(origin)==timesteps(aim), convergence_time_steps delivers the
#' number of time_steps in which the convergence process shall be completed
#' (e.g. 6 for y2055).
#' @param start_year year in which the convergence from origin to aim starts.
#' Value can also be a year not contained in the dataset.
#' @param end_year year in which the convergence from origin to aim shall be
#' reached. Value can also be a year not contained in the dataset. Can be used
#' only alternatively to convergence_time_steps.
#' @param before "stable" leaves the value at origin. If a year is entered,
#' convergence begins at aim, reaches origin at start_year, and goes back to
#' aim until end_year.
#' @param after "stable" leaves the value at aim. All other values let the
#' convergence continue in the same speed even beyond the end_year, such that
#' the values of aim are left.
#' @return returns a time-series with the same timesteps as origin, which
#' lineary fades into the values of the aim object
#' @author Benjamin Bodirsky
#' @seealso \code{\link{lin.convergence}}
#' @examples
#' 
#' pop <- maxample("pop")
#' population <- add_columns(pop,"MIX")
#' population[,,"MIX"] <- lin.convergence(population[,,"A2"],population[,,"B1"],
#'                                        convergence_time_steps=10)
#' 
#' @export lin.convergence
lin.convergence<-function(origin, aim, convergence_time_steps=NULL,start_year=NULL, end_year=NULL, before="stable", after="stable") {

  if(!is.magpie(origin)){stop("origin is no magpie object")}
  if(!is.magpie(aim)){
    if(is.numeric(aim)){
      aim<-as.magpie(array(aim,dim=dim(origin),dimnames=dimnames(origin)))
    } else {stop("aim is no magpie object")}}
  if (all(dimnames(aim)[[1]]!=dimnames(origin)[[1]])) stop("regions have to be the same")
  if (dim(origin)[3]!=1) {
    if (identical(dimnames(origin)[[3]], dimnames(aim)[[3]]) == FALSE) {
      stop("If there ist more than one name-column, dimnames have to be the same")    
    }
  }
  
  if(dim(aim)[2]==1) {
    if (is.null(convergence_time_steps)&is.null(end_year)) {
        end_year<-getYears(aim)
        end_year_num<-getYears(aim,as.integer=TRUE)
    }
    aim_new<-new.magpie(dimnames(origin)[[1]],dimnames(origin)[[2]],dimnames(origin)[[3]])
    for (name_x in 1:dim(aim)[3]) {
      aim_new[,,name_x]<-aim[,,name_x]
    }
    aim<-aim_new
    rm(aim_new)
  
  } 
  if (any(dimnames(aim)[[2]]!=dimnames(origin)[[2]])) stop("Objects need the same timesteps, or aim has to have only one timestep")

  if (is.null(start_year)) {start_year<-getYears(aim)[1]}
  if (is.null(end_year)) {
    if(is.null(convergence_time_steps)) { 
      end_year <- getYears(aim)[length(getYears(aim))]
    } else {
      end_year <- getYears(aim)[which(getYears(aim)==start_year)+ convergence_time_steps - 1]
    }
  } else {
     if(!is.null(convergence_time_steps)) {stop("cannot use both convergence_time_steps and end_year")}
  }

  if(isYear(end_year,with_y=TRUE)){end_year_num<-as.numeric(substr(end_year,2,5))}else{stop("wrong year format for convergence aim")}
  if(isYear(start_year,with_y=TRUE)){start_year_num<-as.numeric(substr(start_year,2,5))}else {stop("wrong year format for convergence aim")}  
  convergence_distance<-end_year_num-start_year_num
  dimnames(aim)[[3]]<-dimnames(origin)[[3]]
  if (isYear(before)) {
    before_num<-as.numeric(substr(before,2,5))
    convergence_distance_back<-start_year_num-before_num
  }
  
  converged<-origin 
  
  for (name_x in getNames(converged)) {
    for (year_x in getYears(converged)) {
      year_x_num<-as.numeric(substr(year_x,2,5))
      mix_up    <-  (year_x_num - start_year_num)/(convergence_distance)
      mix_down  <-  1-mix_up    
      if ((after=="stable")&(mix_up>1)) {
        mix_up<-1
        mix_down<-0
      }
      if (before=="stable"){
        if(mix_up<0) {
          mix_up<-0
          mix_down<-1
        }
      } else if (isYear(before)) {
        if(mix_up<0) {
          mix_up    <-  (start_year_num - year_x_num )/(convergence_distance_back)
          mix_down  <-  1-mix_up 
        }
      }
      converged[,year_x,name_x]<-aim[,year_x,name_x]*mix_up + origin[,year_x,name_x]*mix_down
    }
  }
  
  return(converged)
}
