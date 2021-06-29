readMagpieNCDF4 <- function(fileName) {

  if (!requireNamespace("ncdf4", quietly = TRUE)) stop("The package ncdf4 is required for reading NCDF4 files!")
  ncFile <- ncdf4::nc_open(fileName)

  mv <- getOption("magclass.verbosity")
  on.exit(options(magclass.verbosity = mv)) #nolint
  options(magclass.verbosity = 1) #nolint

  coord <- magclassdata$half_deg[, c("lon", "lat")]

  if (!(max(ncFile$dim$lat$vals) >= max(coord$lat)) | !(min(ncFile$dim$lat$vals) <= min(coord$lat)) |
      !(max(ncFile$dim$lon$vals) >= max(coord$lon)) | !(min(ncFile$dim$lon$vals) <= min(coord$lon))) {

    stop(paste("Only netcdf files with 0.5 degree resolution with extend from", min(coord$lon), max(coord$lon),
               min(coord$lat), max(coord$lat), "are supported.", "Your file has a extend of",
               min(ncFile$dim$lon$vals), max(ncFile$dim$lon$vals), min(ncFile$dim$lat$vals),
               max(ncFile$dim$lat$vals), "."))
  }

  if (is.null(ncFile$dim$time$len)) ncFile$dim$time$len <- 1
  if (is.null(ncFile$dim$time$vals)) ncFile$dim$time$vals <- 1995

  if (length(ncFile$groups) == 1) {
    varNames <- names(ncFile$var)
  } else {
    varNames <- NULL
    for (i in 1:ncFile$nvars) {
      varName <- ncFile$var[[i]]$longname
      groupIndex <- ncFile$var[[i]]$group_index
      groupName <- ncFile$groups[[groupIndex]]$fqgn
      varNames <- c(varNames, paste(groupName, varName, sep = "/"))
      varNames <- gsub("/", ".", varNames)
    }
  }

  # create a single array of all ncdf variables
  ncData <- array(NA, dim = c(ncFile$dim$lon$len, ncFile$dim$lat$len, ncFile$dim$time$len, ncFile$nvars))
  for (i in 1:ncFile$nvars) {
    ncData[, , , i] <- ncdf4::ncvar_get(ncFile, varid = names(ncFile$var)[i])
  }

  # taking out lat and lon from nc file
  lat <- ncFile$dim$lat$vals
  lon <- ncFile$dim$lon$vals
  # coord from magclass data
  coord <- magclassdata$half_deg[, c("lon", "lat")]

  # reorder ncdf array into magpie cellular format (still as array)
  # create emtpy array in magpie cellular format
  mag <- array(NA, dim = c(59199, ncFile$dim$time$len, ncFile$nvars),
               dimnames = list(paste("GLO", 1:59199, sep = "."),
                               paste("y", ncFile$dim$time$vals, sep = ""), varNames))
  # Loop over cells to give mag values taken from ncData. For each cell in mag, we know the exact
  # coordinates (coord). Hence, we can use coord to map coordinates in ncData to cells in mag.
  for (i in 1:ncells(mag)) {
    mag[i, , ] <- ncData[which(coord[i, 1] == lon), which(coord[i, 2] == lat), , ]
  }

  # convert array to magpie object
  return(clean_magpie(as.magpie(mag, temporal = 2)))
}
