#' @title write.magpie.ncdf
#' @description Writes magpie object into netcdf4 file.
#'
#' @param x MAgPIE object. Has to be on half degree resolution. If x as comments in attr, they are plotted as global attributes.
#' @param file file path as provided in write.magpie
#' @param nc_compression Only used if filetype="nc". Sets the compression
#' level for netCDF files (default is 9). If set to an integer between 1 (least
#' compression) and 9 (most compression), the netCDF file is written in netCDF
#' version 4 format. If set to NA, the netCDF file is written in netCDF version
#' 3 format.
#' @param var_style change between variable naming in nc-file; "fullname" for ungrouped name, "grouped" for variable names divided into sub-groups
#' @param comment Vector of comments (also used for setting the unit). Comments are set as global attributes in the netcdf file. Format of comments: "indicator: comment" (e.g. "unit: Share of land area per grid cell")
#' @param verbose Boolean deciding about whether function should be verbose or not

#' @return netcdf file. Writes one file per year per
#' data column. In the case that more than one year and data column is supplied
#' several files are written with the structure filename_year_datacolumn.asc. In the case several data dimensions exist, they are saved as subcategories.
#' 
#' @author Jan Philipp Dietrich, Florian Humpenoeder, Benjamin Leon Bodirsky, Stephen Bi, Kristine Karstens
#' @seealso \code{\link{write.magpie}}
#' 
write.magpie.ncdf<-function(x,file,nc_compression = 9, var_style="fullname", comment=NULL, verbose=TRUE){
  if (!requireNamespace("ncdf4", quietly = TRUE)) stop("The package ncdf4 is required for writing NCDF4 files!")
  if (is.null(getNames(x)) | is.null(getYears(x))) 
    stop("Year and Data name are necessary for saving to NetCDF format")

  # metadata new implementation
  if(!is.null(getMetadata(x))) {
    metadata <- TRUE
    commentary <- getMetadata(x)
    indicator <- names(commentary)
    if(is.list(commentary$source)) {
      for(i in 1:length(commentary$source)) {
        indicator[[length(indicator)+1]] <- paste("source",i)
        commentary[[length(commentary)+1]] <- commentary$source[[i]]
      }
      commentary$source <- NULL
      indicator <- indicator[-match("source",indicator)]
    }
    if(!any(indicator=="unit")) {
      units <- "unknown"
    }else if (is.character(commentary$unit)) {
      units <- commentary$unit
    }else if (is(commentary$unit,"units")) {
      if (as.numeric(commentary$unit)==1) {
        units <- as.character(units(commentary$unit))
      }else {
        units <- paste(as.character(commentary$unit),as.character(units(commentary$unit)))
      }
    #Mixed units handling in development  
    #}else if is(commentary$unit,"mixed_units") {
      #units <- paste(as.character(commentary$unit),as.character(units(commentary$unit)),collapse=", ")
    }else {
      units <- "unknown"
    }
    commentary$unit <- units
    #metadata old implementation
  }else if(!is.null(comment)) {
    metadata=TRUE
    indicator = substring(text = comment,first = 1, last=regexpr(pattern = ": ",text = comment)-1)
    commentary = substring(text = comment,first = (regexpr(pattern = ": ",text = comment)+2))
    if(any(indicator=="")){
      warning("incomplete metadata entry provided as comment. For netcdf, format should be 'indicator: comment'")
      tmp<-which(indicator=="")
      indicator[tmp]<-paste0("metadata",tmp)
    }
    commentary<-commentary[which(indicator!="")]
    indicator<-indicator[which(indicator!="")]
    
    if(!any(indicator=="unit")) { 
      units="not specified" 
    } else {
      units <- commentary[which(indicator=="unit")]
    }
    
    if(any(regexpr(pattern = " ",text = indicator)==1)) { # delete space at first place
      indicator[regexpr(pattern = " ",text = indicator)==1]<-substring(indicator[regexpr(pattern = " ",text = indicator)==1],first = 2)
    }
  }  
  
 if(var_style == "grouped") getNames(x) <- gsub(pattern = "\\.",replacement = "/",getNames(x)) # '/' will be recognized as group-seperator
 #  var_style == "fullname" do not alter '.'-seperators, which lead to fullname variable in nc-file 
 
  mag <- as.array(x)
  coord <- magclassdata$half_deg[, c("lon", "lat")]
  NODATA <- NA
  lon <- seq(-179.75, 179.75, by = 0.5)
  lat <- seq(-89.75, 89.75, by = 0.5)
  time <- as.numeric(unlist(lapply(strsplit(dimnames(mag)[[2]], 
                                            "y"), function(mag) mag[2])))
  data <- dimnames(mag)[[3]]
  if(verbose) message("Converting MAgPIE Data to 720 x 360 array")
  netcdf <- array(NODATA, dim = c(720, 360, dim(mag)[2], 
                                  dim(mag)[3]), dimnames = list(lon, lat, time, 
                                                                data))
  if(verbose) pb <- txtProgressBar(min = 0, max = dim(mag)[1], style = 3)
  for (i in 1:ncells(mag)) {
    netcdf[which(coord[i, 1] == lon), which(coord[i,2] == lat), , ] <- mag[i, , , drop = FALSE]
    if(verbose) setTxtProgressBar(pb, i)
  }
  if(verbose) close(pb)
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "years", time, calendar = "standard")
  
  ncv <- list()
  for (i in dimnames(netcdf)[[4]]) ncv[[i]] <- ncdf4::ncvar_def(i, 
                                                                units=units, 
                                                                dim=list(dim_lon, dim_lat, dim_time), 
                                                                missval=NODATA, 
                                                                prec = "double", 
                                                                compression = nc_compression)
  

  
  if (file.exists(file)) 
    file.remove(file)
  ncf <- ncdf4::nc_create(file, ncv)
  if(verbose) message("Saving to NetCDF format")
  
  if(metadata){
    for (i in 1:length(indicator)){
      if(grepl("source",indicator[[i]])) {
        char <- as.character(commentary[[i]])
        commentary[[i]] <- paste(char,sep="",collapse="\n")
      }else if(indicator[[i]]=="calcHistory") {
        char <- as.character(as.data.frame(commentary$calcHistory)[[1]])
        commentary[[i]] <- paste("\n",char,sep="",collapse="")
      }else if(indicator[[i]]=="version") {
        commentary[[i]] <- paste("\n",names(commentary[[i]]),commentary[[i]],collapse = "; ")
      }
      ncdf4::ncatt_put( nc=ncf, varid=0, attname=indicator[[i]], attval=commentary[[i]],prec="text")  
    }
  }
  
  
  if(verbose) pb <- txtProgressBar(min = 0, max = dim(netcdf)[4], style = 3)
  for (i in dimnames(netcdf)[[4]]) {
    ncdf4::ncvar_put(ncf, ncv[[i]], netcdf[, , , i])
    if(verbose) setTxtProgressBar(pb, which(dimnames(netcdf)[[4]] ==  i))
  }
  if(verbose) close(pb)
  ncdf4::nc_close(ncf)
}