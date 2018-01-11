#' calibrate_it
#' 
#' Standardized functions to calibrate values to a certain baseyear.
#' 
#' 
#' @param origin Original Values (MAgPIE object)
#' @param cal_to Values to calibrate to (MAgPIE object).
#' @param cal_type "none" leaves the values as they are, "convergence" starts
#' from the aim values and then linearily converges towards the values of
#' origin, "growth_rate" uses the growth-rates of origin and applies them on
#' aim.
#' @param cal_year year on which the dataset should be calibrated.
#' @param end_year only for cal_type="convergence". Year in which the
#' calibration shall be faded out.
#' @param report_calibration_factors prints out the multipliers which are used
#' for calibration.
#' @return Calibrated dataset.
#' @author Benjamin Bodirsky
#' @seealso \code{\link{convergence}},\code{\link{lin.convergence}}
#' @examples
#' 
#'   data(population_magpie)
#'   test<-as.magpie(array(1000,dim(population_magpie[,,"A2"]),dimnames(population_magpie[,,"A2"])))
#'   calibrate_it(origin=population_magpie,cal_to=test[,"y1995",],cal_type="growth_rate")
#'   calibrate_it(origin=population_magpie,cal_to=test[,"y1995",],cal_type="convergence", 
#'                cal_year="y1995", end_year="y2055")
#'   calibrate_it(origin=population_magpie,cal_to=test[,"y1995",],cal_type="none")
#' 
#' @export calibrate_it
calibrate_it<-function(origin, cal_to, cal_type="convergence", cal_year=NULL, end_year=NULL, report_calibration_factors=FALSE) {
#data(population_magpie) 
#origin = population_magpie[,,"A2"]
#cal_to= population_magpie[,,"B1"]
#cal_year="y1995"
#end_year="y2155"
#cal_type="convergence"
#origin=population_country_inputdata[,names_years_sres,definition_population_x]
#cal_to=population_calibrate_to_country[,,c("worldbank")]
#cal_type=definition_population_calib_x
#cal_year=definition_calib_year
#end_year=calibration_convergence_year
  if(cal_type=="convergence"&(is.null(cal_year) | is.null(end_year))){stop("for convergence, cal_year and end_year is required")}
  if(!is.magpie(origin)){stop("origin is no magpie object")}
  if(!is.magpie(cal_to)){stop("cal_to is no magpie object")}
  if(!is.null(cal_year)){
    if (cal_year %in% getYears(cal_to)) {cal_to<-cal_to[,cal_year,]}
  }
  if (dim(cal_to)[[2]]!=1){stop("cal_to has more timesteps than one.")}
  if ((is.null(cal_year))&(dim(cal_to)[2]==1)) {
    cal_year<-getYears(cal_to)
    cal_to<-setNames(cal_to,NULL)
  }  
  if ((!is.null(cal_year))&(cal_year==getYears(cal_to))) {cal_to<-setNames(cal_to,NULL)}
  if (!is.null(getYears(cal_to))&(cal_year!=getYears(cal_to))) {stop("cal_year has to be in cal_to, or cal_to has to be NULL")}  
  if (!is.null(getNames(cal_to))&(!identical(getNames(origin),getNames(cal_to)))) {stop("names of cal_to has to be identical with origin or NULL")}    
  calibration_factor<-as.magpie(array(NA,dim(origin),dimnames(origin)))
  calibration_factor[,,]<-1
  calibrated <-as.magpie(array(NA,dim(origin),dimnames(origin)))
  
  cal_to <- setYears(cal_to[,cal_year,],NULL)
  
  if (cal_type=="none") {
    calibrated<-origin
    if(report_calibration_factors==TRUE){
      print(1)
    }       
  } else if (cal_type=="convergence") {
    calibration_factor[,,]<-cal_to[,,]/setYears(origin[,cal_year,],NULL)
    calibration_factor<-convergence(origin=calibration_factor, aim=1, start_year=cal_year, end_year=end_year, direction=NULL, type="linear") 
#   calibration_factor<-lin.convergence(origin=calibration_factor, aim=1, start_year=cal_year, end_year=end_year,before=before, after=after)
    calibrated<-origin*calibration_factor
    if(report_calibration_factors==TRUE){
      print(calibration_factor)
    }
  } else if (cal_type=="growth_rate") {
    cal_origin <- setYears(origin[,cal_year,],NULL)
    calibrated[,,]  <- origin[,,]/cal_origin[,,]*cal_to[,,]
    if(report_calibration_factors==TRUE){
      print(cal_origin[,,]*cal_to[,,])
    }   
  } else {stop("unknown cal_type")}
  calibrated <- updateMetadata(calibrated, origin, unit="copy", source="copy", calcHistory="copy", description="copy")
  return(calibrated)
}

