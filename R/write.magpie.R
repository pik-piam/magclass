#' Write MAgPIE-object to file
#' 
#' Writes a MAgPIE-3D-array (cells,years,datacolumn) to a file in one of three
#' MAgPIE formats (standard, "magpie", "magpie zipped")
#' 
#' This function can write 13 different MAgPIE file\_types. "cs2" is the new
#' standard format for cellular data with or without header and the first
#' columns (year,regiospatial) or only (regiospatial), "cs2b" is identical to
#' "cs2" except that it will suppress the data name if it has only 1 element
#' in the data dimension. "csv" is the standard format for regional 
#' data with or without header and the first columns (year,region,cellnumber) 
#' or only (region,cellnumber), "cs3" is another csv format which is 
#' specifically designed for multidimensional data for usage in GAMS.  
#' All these variants are written without further specification. "rds" is
#' a R-default format for storing R objects.
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
#' @param file_type Format the data should be stored as. Currently 13 formats
#' are available: "rds" (default R-data format), "cs2" (cellular standard 
#' MAgPIE format), "cs2b" (cellular standard MAgPIE format with suppressed header ndata=1),
#' "csv" (regional standard MAgPIE format), "cs3" (Format for multidimensional MAgPIE 
#' data, compatible to GAMS), "cs4" (alternative multidimensional format compatible
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
#' @param metadata.char character: a character vector of length one containing a
#' single character or an empty string. 
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
#' @param verbose Boolean deciding about whether function should be verbose or not
#' @param ... arguments to be passed to write.magpie.ncdf
#' @note
#' 
#' The binary MAgPIE formats .m and .mz have the following content/structure
#' (you only have to care for that if you want to implement
#' read.magpie/write.magpie functions in other languages): \cr \cr 
#' [ FileFormatVersion | Current file format version number (currently 6) | integer | 2 Byte ] \cr 
#' [ nchar_comment | Number of character bytes of the file comment | integer | 4 Byte ] \cr 
#' [ nbyte_metadata | Number of bytes of the serialized metadata | integer | 4 Byte ] \cr 
#' [ nchar_sets | Number of characters bytes of all regionnames + 2 delimiter | integer | 2 Byte] \cr 
#' [ nyears | Number of years | integer | 2 Byte ]\cr 
#' [ year_list | All years of the dataset (0, if year is not present) | integer | 2*nyears Byte ] \cr 
#' [ ncells | Number of cells | integer | 4 Byte ]\cr
#' [ nchar_cell | Number of characters bytes of all regionnames + (nreg-1) for delimiters | integer | 4 Byte ] \cr 
#' [ cells | Cell names saved as cell1\\cell2 (\\n is the delimiter) | character | 1*nchar_cell Byte ] \cr 
#' [ nelem | Total number of data elements | integer | 4 Byte ] \cr 
#' [ nchar_data | Number of char. bytes of all datanames + (ndata - 1) for delimiters | integer | 4 Byte ] \cr
#' [ datanames | Names saved in the format data1\\ndata2 (\\n as del.) | character | 1*nchar_data Byte ] \cr 
#' [ data | Data of the MAgPIE array in vectorized form | numeric | 4*nelem Byte ] \cr 
#' [ comment | Comment with additional information about the data | character | 1*nchar_comment Byte ] \cr 
#' [ sets | Set names with \\n as delimiter | character | 1*nchar_sets Byte] \cr
#' [ metadata | serialized metadata information | bytes | 1*nbyte_metadata Byte] \cr 
#'
#' @author Jan Philipp Dietrich, Stephen Bi
#' @seealso \code{"\linkS4class{magpie}"},
#' \code{\link{read.magpie}},\code{\link{mbind}},\code{\link{write.magpie.ncdf}}
#' @examples
#' 
#' # a <- read.magpie("lpj_yield_ir.csv")
#' # write.magpie(a,"lpj_yield_ir.mz")
#' 
#' @export write.magpie
#' @importFrom utils setTxtProgressBar txtProgressBar write.csv write.table
write.magpie <- function(x,file_name,file_folder="",file_type=NULL,append=FALSE,comment=NULL,comment.char="*",metadata.char="~",mode=NULL,nc_compression=9,verbose=TRUE, ...) {
  umask <- Sys.umask()
  if(!is.null(mode)) {
    umask_mode <- as.character(777-as.integer(mode))
    Sys.umask(umask_mode)
  } else {
    mode <- as.character(777-as.integer(as.character(umask)))
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
    metadata <- getMetadata(x)
    
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
    
    #function to write metadata to cs* filetypes
    .writeMetadata <- function(file,metadata,char,mchar) {
      if(!is.null(metadata$unit)) {
        if (is(metadata$unit,"units")) {
          if (as.numeric(metadata$unit)==1) {
            unit <- as.character(units(metadata$unit))
          }else {
            unit <- paste(as.character(metadata$unit),as.character(units(metadata$unit)))
          }
        }else if (is.character(metadata$unit)) {
          unit <- metadata$unit
        #Mixed units handling in development  
        #}else if (is(metadata$unit,"units")) {
          #unit <- paste(as.character(metadata$unit),as.character(units(metadata$unit)),collapse=", ")
        }else {
          unit <- "unknown"
        }
        writeLines(paste(char,paste0(mchar,"unit:"),paste(unit,collapse=", ")),file)
        writeLines(char,file)
      }
      if(!is.null(metadata$user)) {
        writeLines(paste(char,paste0(mchar,"user:"),metadata$user),file)
        writeLines(char,file)
      }
      if(!is.null(metadata$date)) {
        writeLines(paste(char,paste0(mchar,"date:"),metadata$date),file)
        writeLines(char,file)
      }
      if(!is.null(metadata$version)) {
        writeLines(paste(char,paste0(mchar,"version:"),paste(names(metadata$version),metadata$version,collapse = "; ")),file)
        writeLines(char,file)
      }
      if(!is.null(metadata$description)) {
        writeLines(paste(char,paste0(mchar,"description:"),metadata$description[1]),file)
        if (length(metadata$description)>1)  writeLines(paste(char,"\t",paste(metadata$description[-1],collapse="\n\t")),file)
        writeLines(char,file)
      }
      if(!is.null(metadata$note)) {
        writeLines(paste(char,paste0(mchar,"note:"),metadata$note[1]),file)
        if (length(metadata$note)>1)  writeLines(paste(char,"\t",paste(metadata$note[-1])),file)
        writeLines(char,file)
      }
      if(!is.null(metadata$source)) {
        writeLines(paste0(char," ",mchar,"source:"),file)
        if(is.list(metadata$source)) {
          for(i in 1:length(metadata$source)) {
            if(is(metadata$source[[i]],"bibentry"))  writeLines(paste(char,toBibtex(metadata$source[[i]])),file)
            else if(is(metadata$source[[i]],"Bibtex"))  writeLines(paste(char,metadata$source[[i]]),file)
          }
        }else {
          if(is(metadata$source,"bibentry"))  writeLines(paste(char,toBibtex(metadata$source)),file)
          else if(is(metadata$source,"Bibtex"))  writeLines(paste(char,metadata$source),file)
        }
        writeLines(char,file)
      }
      if(is(metadata$calcHistory,"Node")) {
        calcHistory <- as.character(as.data.frame(metadata$calcHistory)[[1]])
        writeLines(paste0(char," ",mchar,"calcHistory:"),file)
        writeLines(paste(char,calcHistory),file,useBytes=TRUE)
        writeLines(char,file)
      }
    }
    
    if(file_type=="m" | file_type=="mz") {
      fformat_version <- "6"  #File format version (oldest data has version 0)
      comment <- paste(comment,collapse="\n")
      ncells <- dim(x)[1]
      nyears <- dim(x)[2]
      ndata  <- dim(x)[3]    
      cells <- dimnames(x)[[1]]
      cells_collapsed <- paste(cells,collapse='\n')
      datanames <- dimnames(x)[[3]]
      datanames_collapsed <- paste(datanames,collapse='\n')    
      sets_collapsed <- paste(getSets(x,fulldim = FALSE), collapse = '\n')
      metadata <- serialize(metadata,NULL,FALSE)
      
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
      writeBin(as.integer(nchar(comment, type="bytes")),zz,size=4)
      writeBin(as.integer(length(metadata)),zz,size=4)
      writeBin(as.integer(nchar(sets_collapsed, type="bytes")),zz,size=2)
      writeBin(as.integer(c(nyears,year_list)),zz,size=2)
      writeBin(as.integer(c(ncells,nchar(cells_collapsed, type="bytes"))),zz,size=4)
      writeChar(cells_collapsed,zz,eos=NULL)
      writeBin(as.integer(c(ndata*ncells*nyears,nchar(datanames_collapsed, type="bytes"))),zz,size=4)
      if(datanames_collapsed!="") writeChar(datanames_collapsed,zz,eos=NULL)
      writeBin(as.numeric(as.vector(x)),zz,size=4)
      if(comment!="") writeChar(comment,zz,eos=NULL)
      if(nchar(sets_collapsed, type="bytes")>0) writeChar(sets_collapsed,zz,eos=NULL)
      if(!is.null(metadata)) writeBin(metadata,zz)
      close(zz)    
      Sys.chmod(file_path, mode)
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
    } else if(file_type=="rds") {
      saveRDS(object=x, file=file_path, ...)
    } else if(file_type=="nc") {
      write.magpie.ncdf(x=x,
                        file=file_path,
                        nc_compression = nc_compression,
                        comment=comment,
                        verbose=verbose, ...)  
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
      if(any(comment!=""))  writeLines(paste(comment.char,comment,sep=""),zz)
      if(!is.null(metadata))  .writeMetadata(zz,metadata,comment.char,metadata.char)
      write.csv(x,file=zz,quote=FALSE,row.names=FALSE)
      close(zz)
      Sys.chmod(file_path, mode)
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
      if(!is.null(metadata))  .writeMetadata(zz,metadata,comment.char,metadata.char)
      write.table(output,file=zz,quote=FALSE,row.names=FALSE,col.names=FALSE,sep=",")
      close(zz)
      Sys.chmod(file_path, mode)
      
    } else {
      print_cells <- nregions(x)<ncells(x)
      print_regions <- getRegions(x)[1]!="GLO"
      print_data <- ((ndata(x)>1) | !is.null(getNames(x)))
      
      if(file_type=="cs2b" && ndata(x)==1) getNames(x) <- NULL
      
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
        if(!is.null(metadata))  .writeMetadata(zz,metadata,comment.char,metadata.char)
        write.table(output,zz,sep=",",col.names=header,row.names=FALSE,quote=FALSE)  
        close(zz)
        Sys.chmod(file_path, mode)
      } else {      
        if(file_type=="csvr" | file_type=="cs2r") dimnames(x)[[2]] <- sub("y","",dimnames(x)[[2]])
        if(file_type %in% c("cs2","cs2b","cs2r")) print_regions <- FALSE
        output <- array(NA,c(dim(x)[1]*dim(x)[2],dim(x)[3]+print_regions+print_cells+years))
      	output[,(1+print_regions+print_cells+years):dim(output)[2]] <- as.vector(as.matrix(x))
        if(years) {
          yearvec <- c()
          for(year in dimnames(x)[[2]]) yearvec <- c(yearvec,rep(year,dim(x)[1]))
          output[,1] <- yearvec
        }
      	if(print_regions) output[,1+years] <- substring(rep(dimnames(x)[[1]],dim(x)[2]),1,3)
      	if(print_cells) {
          if(file_type %in% c("cs2","cs2b","cs2r")) {
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
        if(any(comment!="")) writeLines(paste0(comment.char,comment,sep=""),zz)
        if(!is.null(metadata))  .writeMetadata(zz,metadata,comment.char,metadata.char)
        write.table(output,zz,sep=",",col.names=header,row.names=FALSE,quote=FALSE)
        close(zz)
        Sys.chmod(file_path, mode)
      }
    }
  } else {
    stop("Input is not in MAgPIE-format!")
  }
  if(!is.null(mode)) Sys.umask(umask)
}
