#' Read LPJmL from nc-file
#' 
#' Reads a LPJmL nc-file and converts it to a 3D array of the structure
#' (cells,years,datacolumn)
#' 
#' @param file_name file name including file ending (wildcards are supported).
#' Optionally also the full path can be specified here (instead of splitting it
#' to file\_name and file\_folder)
#' @param file_folder folder the file is located in (alternatively you can also
#' specify the full path in file\_name - wildcards are supported)
#' @param years a vector containing the years of interest
#' @param split_data split reading routine to avoid memory issues 
#' @param keep_month keep monthly data (month as 3rd magpie data dim)
#' @param averaging_range number of years to be averaged (if even: overweight for prevous time period)
#' @return \item{x}{MAgPIE-object}
#' 
#' @author Kristine Karstens
#' @seealso \code{"\linkS4class{magpie}"}, \code{\link{read.magpie}}
#' @examples
#' 
#' \dontrun{
#' a <- read.lpjml_nc("sdate.nc")}
#'

read.lpjml_nc <- function(file_name,
                          file_folder = "",
                          years       = NULL,
                          split_data  = FALSE , 
                          keep_month  = FALSE ,
                          averaging_range = 1) {

  file_name      <- paste(file_folder,file_name,sep="")
  nc_file        <- ncdf4::nc_open(file_name)
  if(!is.numeric(years)) years <- as.numeric(substring(years,2))
 
  #taking out lat and lon from nc file
  lat <- nc_file$dim$lat$vals
  lon <- nc_file$dim$lon$vals
  #coord from magclass data
  coord <- magclassdata$half_deg[, c("lon", "lat")]

  #######################################
  # check splitting
  #######################################
  if(split_data){
    
    cat("Start splitted processing. This might take a while.\n")
    
    mag <- NULL
    
    for(year_x in years){
      mag <- mbind(mag, read.lpjml_nc(file_name = file_name, 
                                    file_folder = file_folder,
                                          years = year_x,   
                                    split_data  = FALSE ,   
                                    keep_month  = keep_month ,
                                averaging_range = averaging_range))
      cat(paste(year_x,"done.\n"))
    }
    
    return(mag)
  }

  #######################################
  # check averaging
  #######################################
  if(averaging_range > 1){
  
    # calculate averaging steps out of averaging range (note: even numbers lead to overweight of previous time steps)
    averaging_steps <- -floor(averaging_range/2) + (0:(averaging_range - 1))
    
    # add to each year the averaging steps
    years           <- c(outer(averaging_steps, years, `+`))
  
  } else if(!(is.null(averaging_range) | averaging_range == 1)){
    stop("Averaging range has to be a number or NULL.")
  }
  
  #######################################
  # check monthly
  #######################################
  file_shortname <- tail(unlist(strsplit(file_name,'/')),1)
  if(grepl("^m",file_shortname)){
    
    start_year     <- as.integer(gsub(".*?([0-9]+).*", "\\1", nc_file$dim$time$units[1]))
    all_years      <- c(start_year:(length(nc_file$dim$time$vals)/12 + start_year))
    if(!all(is.element(years, all_years))) stop(paste0("Cannot read years outside of ", paste(all_years, collapse=", ")))

    time_level  <- (years - start_year)*12 + 1 
    time_steps  <- c(1:12)
    ntimesteps  <- 12*length(time_level)
    
  } else{
    
    start_year     <- nc_file$dim$time$vals[1]
    if(!all(is.element(years, nc_file$dim$time$vals))) stop(paste0("Cannot read years outside of ", paste(nc_file$dim$time$vals, collapse=", ")))
  
    time_level  <- years - start_year + 1 
    time_steps  <- 1
    ntimesteps  <- length(time_level)
  }
  
  #######################################
  # check 4. dimension
  #######################################
  if((dim <- length(nc_file$dim)) > 3){
    # for PFT-specific outputs, first variable is for PFT names
    nc_level    <- ncdf4::ncvar_get(nc_file,varid=names(nc_file$var)[1])
    var_names   <- names(nc_file$var)[2]
    dim         <- 4
  } else {
    nc_level    <- names(nc_file$var)[1]
    var_names   <- names(nc_file$var)[1]
  }
  
  #######################################
  # create a single array for all ncdf variables
  #######################################
  
  if(dim == 3){
    
    #######################################
    # read 3D data (monthly and annual) 
    #######################################
    nc_data <- array(NA, dim = c(nc_file$dim$lon$len, nc_file$dim$lat$len, ntimesteps))
    
    if(length(time_steps) == 12){
      #######################################
      # handle monthly data 
      #######################################
      for(i in (1:length(time_level))){
        nc_data[,,(i-1)*12+time_steps] <- ncdf4::ncvar_get(nc    = nc_file, 
                                                           varid = var_names, 
                                                           start = c(1,     1, time_level[i]), 
                                                           count = c(720, 280, length(time_steps)))
      }
      
      if(keep_month){
        dim(nc_data) <- c(720, 280, 12, length(years))
        nc_data      <- aperm(nc_data, c(1,2,4,3))
        
        time_level  <- years - start_year + 1 
        time_steps  <- 1
        ntimesteps  <- length(time_level)
        nc_level    <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
        dim         <- 4
        
      } else {
        dim(nc_data) <- c(720, 280, 12, length(years))
        nc_data      <- aperm(nc_data, c(1,2,4,3))
        nc_data      <- rowSums(nc_data, dims = 3)         
        
        time_level  <- years - start_year + 1 
        time_steps  <- 1
        ntimesteps  <- length(time_level)
      }
      
    } else {
      #######################################
      # handle annual data 
      #######################################
      for(i in (1:length(time_level))){
        nc_data[,,i]                   <- ncdf4::ncvar_get(nc    = nc_file, 
                                                           varid = var_names, 
                                                           start = c(1,     1, time_level[i]), 
                                                           count = c(720, 280, length(time_steps)))
      }
    }
    
  } else if(dim == 4){
    
    #######################################
    # read 4D data (monthly and annual) 
    #######################################
    nc_data <- array(NA, dim = c(nc_file$dim$lon$len, nc_file$dim$lat$len, ntimesteps, length(nc_level)))
    
    for(i in (1:length(time_level))){
      nc_data[,,i,] <- ncdf4::ncvar_get(nc    = nc_file, 
                                        varid = var_names, 
                                        start = c(1,     1,                1, time_level[i]), 
                                        count = c(720, 280, length(nc_level), time_steps))
    }
    
  } else {stop("Input with more than 4 dimensions cannot be processed at the moment.")}

  #######################################
  # apply averaging
  #######################################
  
  if(averaging_range > 1){
    
    dim(years) <- c(averaging_range, length(years)/averaging_range)
    years      <- ceiling(colMeans(years))
    
    if(dim == 4){
      
      tmp <- array(0, dim = c(nc_file$dim$lon$len, nc_file$dim$lat$len, length(years), length(nc_level)))
      for(i in 1:length(years)){
        for(j in averaging_range*(i-1)+(1:averaging_range)){tmp[,,i,] <- tmp[,,i,]+nc_data[,,j,]/averaging_range}
      }
    } else if (dim == 3){
      
      tmp <- array(0, dim = c(nc_file$dim$lon$len, nc_file$dim$lat$len, length(years)))
      for(i in 1:length(years)){
        for(j in averaging_range*(i-1)+(1:averaging_range)){tmp[,,i] <- tmp[,,i]+nc_data[,,j]/averaging_range}
      }
    }
    
    nc_data <- tmp
  }

  #######################################
  # transform to mag object 
  #######################################
  
  #reorder ncdf array into magpie cellular format (still as array)
  #create emtpy array in magpie cellular format
  mag <- array(NA, dim=c(59199,length(years), length(nc_level)), 
               dimnames=list(paste("GLO",1:59199,sep="."), paste("y", years,sep=""), nc_level))

  if(dim == 4){
    
    #Loop over cells to give mag values taken from nc_data. For each cell in mag, we know the exact coordinates (coord). Hence, we can use coord to map coordinates in nc_data to cells in mag.
    for (i in 1:ncells(mag)) {
      mag[i,,] <- nc_data[which(coord[i, 1]==lon), which(coord[i,2]==lat),,]
    }
    
  } else if (dim == 3){
    
    #Loop over cells to give mag values taken from nc_data. For each cell in mag, we know the exact coordinates (coord). Hence, we can use coord to map coordinates in nc_data to cells in mag.
    for (i in 1:ncells(mag)) {
      mag[i,,] <- nc_data[which(coord[i, 1]==lon), which(coord[i,2]==lat),]
    }
    
  } else {stop("Input with more than 4 dimensions cannot be processed at the moment.")}
  
  mag <- as.magpie(mag)
  
  return(mag)
}