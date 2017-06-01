#' Write MAgPIE-object to file
#' 
#' Writes a MAgPIE-3D-array (cells,years,datacolumn) to a file in one of three
#' MAgPIE formats (standard, "magpie", "magpie zipped")
#' 
#' This function can write 9 different MAgPIE file\_types. "cs2" is the new
#' standard format for cellular data with or without header and the first
#' columns (year,regiospatial) or only (regiospatial), "csv" is the standard
#' format for regional data with or without header and the first columns
#' (year,region,cellnumber) or only (region,cellnumber), "cs3" is another csv
#' format which is specifically designed for multidimensional data for usage in
#' GAMS.  All these variants are written without further specification.
#' "magpie" (.m) and "magpie zipped" (.mz) are new formats developed to allow a
#' less storage intensive management of MAgPIE-data. The only difference
#' between both formats is that .mz is gzipped whereas .m is not compressed. So
#' .mz needs less memory, whereas .m might have a higher compatibility to other
#' languages. "asc" is the ASCII grid format. "nc" is the netCDF format.  It
#' can only be applied for half degree data and writes one file per year per
#' data column. In the case that more than one year and data column is supplied
#' several files are written with the structure filename_year_datacolumn.asc
#' 
#' @param x MAgPIE-object
#' @param file_name file name including file ending (wildcards are supported).
#' Optionally also the full path can be specified here (instead of splitting it
#' to file\_name and file\_folder)
#' @param file_folder folder the file should be written to (alternatively you
#' can also specify the full path in file\_name - wildcards are supported)
#' @param file_type Format the data should be stored as. Currently 11 formats
#' are available: "cs2" (cellular standard MAgPIE format), "csv" (regional
#' standard MAgPIE format), "cs3" (Format for multidimensional MAgPIE data,
#' compatible to GAMS), "cs4" (alternative multidimensional format compatible
#' to GAMS, in contrast to cs3 it can also handle sparse data), "csvr", "cs2r",
#' "cs3r" and "cs4r" which are the same formats as the previous mentioned ones
#' with the only difference that they have a REMIND compatible format, "m"
#' (binary MAgPIE format "magpie"), "mz" (compressed binary MAgPIE format
#' "magpie zipped"), "asc" (ASCII grid format / only available for 0.5deg data)
#' and "nc" (netCDF format / only available for 0.5deg data). If
#' file\_type=NULL the file ending of the file\_name is used as format. If
#' format is different to the formats mentioned standard MAgPIE format is
#' assumed. Please be aware that the file\_name is independent of the
#' file\_type you choose here, so no additional file ending will be added!
#' @param append Decides whether an existing file should be overwritten (FALSE)
#' or the data should be added to it (TRUE). Append = TRUE only works if the
#' existing data can be combined with the new data using the mbind function
#' @param comment Vector of strings: Optional comment giving additional
#' information about the data. If different to NULL this will overwrite the
#' content of attr(x,"comment")
#' @param comment.char character: a character vector of length one containing a
#' single character or an empty string. Use "" to turn off the interpretation
#' of comments altogether.
#' @param mode File permissions the file should be written with as 3-digit
#' number (e.g. "777" means full access for user, group and all, "750" means
#' full access for user, read access for group and no acess for anybody else).
#' Set to NULL system defaults will be used. Access codes are identical to the
#' codes used in unix function chmod.
#' @param nc_compression Only used if file\_type="nc". Sets the compression
#' level for netCDF files (default is 9). If set to an integer between 1 (least
#' compression) and 9 (most compression), the netCDF file is written in netCDF
#' version 4 format. If set to NA, the netCDF file is written in netCDF version
#' 3 format.
#' @note
#' 
#' The binary MAgPIE formats .m and .mz have the following content/structure
#' (you only have to care for that if you want to implement
#' read.magpie/write.magpie functions in other languages): \cr \cr 
#' [ FileFormatVersion | Current file format version number (currently 2) | integer | 2 Byte ] \cr 
#' [ nchar_comment | Number of characters of the file comment | integer | 4 Byte ] \cr 
#' [ nchar_sets | Number of characters of all regionnames + 2 delimiter | integer | 2 Byte] \cr 
#' [ not used | Bytes reserved for later file format improvements | integer | 92 Byte ] \cr
#' [ nyears | Number of years | integer | 2 Byte ]\cr 
#' [ year_list | All years of the dataset (0, if year is not present) | integer | 2*nyears Byte ] \cr 
#' [ nregions | Number of regions | integer | 2 Byte ] \cr 
#' [ nchar_reg | Number of characters of all regionnames + (nreg-1) for delimiters | integer | 2 Byte ] \cr 
#' [ regions | Regionnames saved as reg1\\nreg2 (\\n is the delimiter) | character | 1*nchar_reg Byte ] \cr 
#' [ cpr | Cells per region | integer | 4*nreg Byte ] \cr 
#' [ nelem | Total number of data elements | integer | 4 Byte ] \cr 
#' [ nchar_data | Number of char. of all datanames + (ndata - 1) for delimiters | integer | 4 Byte ] \cr
#' [ datanames | Names saved in the format data1\\ndata2 (\\n as del.) | character | 1*nchar_data Byte ] \cr 
#' [ data | Data of the MAgPIE array in vectorized form | numeric | 4*nelem Byte ] \cr 
#' [ comment | Comment with additional information about the data | character | 1*nchar_comment Byte ] \cr 
#' [ sets | Set names with \\n as delimiter | character | 1*nchar_sets Byte] \cr
#' 
#' Please note that if your data in the spatial dimension is not ordered by
#' region name each new appearance of a region which already appeared before
#' will be treated and counted as a new region (e.g.
#' AFR.1,AFR.2,CPA.3,CPA.4,AFR.5 will count AFR twice and nregions will be set
#' to 3!).
#' @author Jan Philipp Dietrich
#' @seealso \code{"\linkS4class{magpie}"},
#' \code{\link{read.magpie}},\code{\link{mbind}}
#' @examples
#' 
#' # a <- read.magpie("lpj_yield_ir.csv")
#' # write.magpie(a,"lpj_yield_ir.mz")
#' 
#' @export write.magpie
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv write.table
write.magpie <- function(x,file_name,file_folder="",file_type=NULL,append=FALSE,comment=NULL,comment.char="*",mode=NULL,nc_compression=9) {
  if(!is.null(mode)) {
    umask <- Sys.umask()
    umask_mode <- as.character(777-as.integer(mode))
    Sys.umask(umask_mode)
  }
  if(is.null(x)) x <- as.magpie(numeric(0))
  if(is.magpie(x)) {
    years <- !(is.null(dimnames(x)[[2]]))
    
    #if file-type is not mentioned file-ending is used as file-type
    if(is.null(file_type)) {
      file_type <- tail(strsplit(file_name,'\\.')[[1]],1)
    }
    if(!file_folder==""){
      file_path <- paste(file_folder,file_name,sep="/")
    }
    else{
      file_path <- file_name
    }  
    
    #look for comment/addtitional information
    if(is.null(comment) & !is.null(attr(x,"comment"))) comment <- attr(x,"comment")
    if(is.null(comment)) comment <- "" 
    
    #expand wildcards
    file_path <- paste(Sys.glob(dirname(file_path)),basename(file_path),sep="/")
    if(length(file_path)>1) {
      file_path <- file_path[1]
      warning("file name is ambiguous, only first alternative is used!")
    }
    
    if(append & file.exists(file_path)) {
      x2 <- read.magpie(file_path)
      x <- mbind(x2,x)
    }
    
    if(file_type=="m" | file_type=="mz") {
      fformat_version <- "2"  #File format version 1 (older data has version 0)
      comment <- paste(comment,collapse="\n")
      ncells <- dim(x)[1]
      nyears <- dim(x)[2]
      ndata  <- dim(x)[3]    
      rle <- rle(gsub("\\..*$","",dimnames(x)[[1]]))
      regions <- rle$values
      cpr <- rle$lengths
      nregions <- length(regions)
      regions_collapsed <- paste(regions,collapse='\n')
      datanames <- dimnames(x)[[3]]
      datanames_collapsed <- paste(datanames,collapse='\n')    
      sets_collapsed <- paste(getSets(x,fulldim = FALSE), collapse = '\n')
      
      if(years) {
        year_list <- as.integer(substr(dimnames(x)[[2]],2,5))
      } else {
        year_list <- 0
      }
      
      if(file_type=="mz") {
        zz <- gzfile(file_path,"wb")
      } else {
        zz <- file(file_path,"wb")
      }
      
      writeBin(as.integer(fformat_version),zz,size=2)
      writeBin(as.integer(nchar(comment)),zz,size=4)
      writeBin(as.integer(nchar(sets_collapsed)),zz,size=2)
      writeBin(as.integer(rep(0,92)),zz,size=1) #92 Byte reserved for later file format improvements
      writeBin(as.integer(c(nyears,year_list,nregions,nchar(regions_collapsed))),zz,size=2)
      writeChar(regions_collapsed,zz,eos=NULL)
      writeBin(as.integer(c(cpr,ndata*ncells*nyears,nchar(datanames_collapsed))),zz,size=4)
      if(datanames_collapsed!="") writeChar(datanames_collapsed,zz,eos=NULL)
      writeBin(as.numeric(as.vector(x)),zz,size=4)
      if(comment!="") writeChar(comment,zz,eos=NULL)
      if(nchar(sets_collapsed)>0) writeChar(sets_collapsed,zz,eos=NULL)
      close(zz)     
    } else if(file_type=="asc") {
      coord <- magclassdata$half_deg[,c("lon","lat")]
      if(dim(coord)[1]!=dim(x)[1]) stop("Wrong format! Only 0.5deg data can be written as ascii grid!")
      if(any(comment!="")) warning("asc format does not support comments!")
      for(y in 1:nyears(x)) {
        tmp_file <- ifelse(nyears(x)>1,sub("(\\.[^\\.]*)$",paste("_",getYears(x)[y],"\\1",sep=""),file_path),file_path)
        for(d in 1:ndata(x)) {
          tmp2_file <- ifelse(ndata(x)>1,sub("(\\.[^\\.]*)$",paste("_",getNames(x)[d],"\\1",sep=""),tmp_file),tmp_file)
          data <- as.data.frame(as.vector((x[,y,d])))
          grid <- suppressWarnings(sp::SpatialPixelsDataFrame(points = coord[c("lon", "lat")], data = data))
          sp::write.asciigrid(grid,tmp2_file)          
        }
      }
    } else if(file_type=="nc") {
      if (is.null(getNames(x)) | is.null(getYears(x))) stop("Year and Data name are necessary for saving to NetCDF format")
      mag <- as.array(x)
      
      #coord from magclass data
      coord <- magclassdata$half_deg[,c("lon","lat")]

      # netcdf generation ####
      NODATA <- NA
      
      # 4D array: lon, lat, time, data
      lon <- seq(-179.75,179.75,by=0.5)
      lat <- seq(-89.75,89.75,by=0.5)
      time <- as.numeric(unlist(lapply(strsplit(dimnames(mag)[[2]],"y"),function(mag) mag[2])))
      data <- dimnames(mag)[[3]]

      #Convert magpie data to array; coord is used for mapping cells in mag to coordinates in netcdf
      cat("Converting MAgPIE Data to 720 x 360 array")
      netcdf <- array(NODATA,dim=c(720,360,dim(mag)[2],dim(mag)[3]),dimnames=list(lon,lat,time,data))
      pb <- txtProgressBar(min = 0, max = dim(mag)[1], style = 3)
      for (i in 1:ncells(mag)) {
        netcdf[which(coord[i, 1]==lon), which(coord[i,2]==lat),,] <- mag[i,,,drop=FALSE]
        setTxtProgressBar(pb, i)
      }
      close(pb)
      
      # NC file dimensions
      dim_lon <- ncdf4::ncdim_def("lon","degrees_east",lon)
      dim_lat <- ncdf4::ncdim_def("lat","degrees_north",lat)
      dim_time <- ncdf4::ncdim_def("time","years",time,calendar = "standard")
      
      #Define variables
      ncv <- list()
      for (i in dimnames(netcdf)[[4]]) ncv[[i]] <- ncdf4::ncvar_def(i, comment, list(dim_lon,dim_lat,dim_time), NODATA, prec="double",compression=nc_compression)
      
      #Create file
      if (file.exists(file_path)) file.remove(file_path)
      ncf <- ncdf4::nc_create(file_path, ncv)
      
      #Put data into file
      cat("Saving to NetCDF format")
      pb <- txtProgressBar(min = 0, max = dim(netcdf)[4], style = 3)
      for (i in dimnames(netcdf)[[4]]) {
        ncdf4::ncvar_put(ncf, ncv[[i]], netcdf[,,,i])
        setTxtProgressBar(pb, which(dimnames(netcdf)[[4]] == i))
      }
      close(pb)
      ncdf4::nc_close(ncf)
      } else if(file_type=="cs3" | file_type=="cs3r") {
      if(file_type=="cs3r") dimnames(x)[[2]] <- sub("y","",dimnames(x)[[2]])
      if(dim(x)[3]!=prod(fulldim(x)[[1]][-1:-2])) stop("Input data seems to be sparse but ",file_type," does not support sparse data. Please use ",sub("3","4",file_type)," instead!")
      x <- unwrap(x)
      if(dim(x)[1]==1 & length(grep("GLO",dimnames(x)[[1]]))==1) {
        dimnames(x)[[1]] <- "TODELETE"  
      } else {
        if(nregions(x) == dim(x)[1]) {
          dimnames(x)[[1]] <- sub("\\..*$","",dimnames(x)[[1]])  
        } else {
          dimnames(x)[[1]] <- sub("\\.","_",dimnames(x)[[1]])
        }
      }
      x <- wrap(x,map=list(NA,length(dim(x))))
      dimnames(x)[[1]] <- sub("^([^\\.]*)\\.([^\\.]*)","\\2\\.\\1",dimnames(x)[[1]])
      
      dimnames(x)[[1]] <- gsub("TODELETE","",dimnames(x)[[1]])
      dimnames(x)[[1]] <- gsub("\\.\\.","\\.",dimnames(x)[[1]])
      dimnames(x)[[1]] <- gsub("^\\.","",dimnames(x)[[1]])  
      dimnames(x)[[1]] <- gsub("\\.$","",dimnames(x)[[1]])  
      dimnames(x)[[1]] <- gsub("\\.",",",dimnames(x)[[1]])
      
      
      header <- dimnames(x)[[2]]
      x <- cbind(dimnames(x)[[1]],x)
      dimnames(x)[[2]] <- c(gsub("[^,]*(,|$)","dummy\\1",x[1,1]),header)
      zz <- file(file_path,open="w")
      if(any(comment!="")) writeLines(paste(comment.char,comment,sep=""),zz)
      write.csv(x,file=zz,quote=FALSE,row.names=FALSE)
      close(zz)
    } else if(file_type=="cs4" | file_type=="cs4r") {
      print_cells <- nregions(x)<ncells(x)
      print_regions <- getRegions(x)[1]!="GLO"
      print_data <- ((ndata(x)>1) | !is.null(getNames(x)))
      
      output <- as.data.frame(x)
      output <- output[c("Year","Region","Cell",names(output)[-c(1:3)])]
      
      if(!print_cells) output["Cell"] <- NULL
      if(!print_regions) output["Region"] <- NULL
      if(!print_data) output["Data1"] <- NULL
      if(!years) {
        output["Year"] <- NULL
      } else {
        if(file_type=="cs4") levels(output[["Year"]]) <- paste0("y",levels(output[["Year"]]))
      }
      zz <- file(file_path,open="w")
      if(any(comment!="")) writeLines(paste(comment.char,comment,sep=""),zz)
      write.table(output,file=zz,quote=FALSE,row.names=FALSE,col.names=FALSE,sep=",")
      close(zz)
      
    } else {
      print_cells <- nregions(x)<ncells(x)
      print_regions <- getRegions(x)[1]!="GLO"
      print_data <- ((ndata(x)>1) | !is.null(getNames(x)))
      
      #non-cellular data
      if(!print_cells & (!print_data | !years | !print_regions )) {
        if(file_type=="csvr" | file_type=="cs2r") dimnames(x)[[2]] <- sub("y","",dimnames(x)[[2]])
        if(!print_data) {
          output <-  array(x,dim=dim(x)[1:2],dimnames=list(dimnames(x)[[1]],dimnames(x)[[2]]))
          output <- aperm(output)
          if(print_regions) {
            output <- rbind(substring(dimnames(x)[[1]],1,3),output)
            if(years) output <- cbind(c("dummy",dimnames(x)[[2]]),output)
          } else {
            if(years) output <- cbind(dimnames(x)[[2]],output)          
          }
          header <- FALSE
        } else if(!years) {
          output <-  array(x,dim=dim(x)[c(1,3)],dimnames=list(dimnames(x)[[1]],dimnames(x)[[3]]))
          header <- !is.null(dimnames(output)[[2]])
          if(print_regions) output <- cbind(substring(dimnames(x)[[1]],1,3),output)
          if(header & !print_regions) {
            output <- t(output)
            header <- FALSE
            output <- cbind(dimnames(x)[[3]],output)
          }
        } else {
          output <-  array(x,dim=dim(x)[2:3],dimnames=list(dimnames(x)[[2]],dimnames(x)[[3]]))
          header <- !is.null(dimnames(output)[[2]])
          output <- cbind(dimnames(x)[[2]],output)
          dimnames(output)[[2]][1] <- "dummy"
        }
        if(header & print_regions) dimnames(output)[[2]][1] <- "dummy"
        zz <- file(file_path,open="w")
        if(any(comment!="")) writeLines(paste(comment.char,comment,sep=""),zz)
        write.table(output,zz,sep=",",col.names=header,row.names=FALSE,quote=FALSE)  
        close(zz)
      } else {      
        if(file_type=="csvr" | file_type=="cs2r") dimnames(x)[[2]] <- sub("y","",dimnames(x)[[2]])
        if(file_type=="cs2" | file_type=="cs2r") print_regions <- FALSE
        output <- array(NA,c(dim(x)[1]*dim(x)[2],dim(x)[3]+print_regions+print_cells+years))
      	output[,(1+print_regions+print_cells+years):dim(output)[2]] <- as.vector(as.matrix(x))
        if(years) {
          yearvec <- c()
          for(year in dimnames(x)[[2]]) yearvec <- c(yearvec,rep(year,dim(x)[1]))
          output[,1] <- yearvec
        }
      	if(print_regions) output[,1+years] <- substring(rep(dimnames(x)[[1]],dim(x)[2]),1,3)
      	if(print_cells) {
          if(file_type=="cs2"  | file_type=="cs2r") {
            output[,1+print_regions+years] <- rep(gsub(".","_",dimnames(x)[[1]],fixed=TRUE),dim(x)[2])
          } else {
            output[,1+print_regions+years] <- rep(1:dim(x)[1],dim(x)[2])
          }
       }
      	if(!is.null(dimnames(x)[[3]])) {
      	  dimnames(output)[[2]] <- c(rep("dummy",print_regions+print_cells+years),dimnames(x)[[3]])
      	  header <- TRUE
      	} else {
      	  header <- FALSE
      	}
        zz <- file(file_path,open="w")
        if(any(comment!="")) writeLines(paste(comment.char,comment,sep=""),zz)
        write.table(output,zz,sep=",",col.names=header,row.names=FALSE,quote=FALSE)
        close(zz)
      }
    }
  } else {
    stop("Input is not in MAgPIE-format!")
  }
  if(!is.null(mode)) Sys.umask(umask)
}
