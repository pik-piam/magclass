#' mplot
#'
#' Get a quick visualization of the content of most magpie objects.
#'
#' @param px The magpie object to be visualized
#' @param dim3 A character string for filtering the items in the data dimensions to be visualized
#' (by default all are shown).
#' @param global Whether data should be aggregated over regions to global values (by default TRUE).
#' @param dim1 A character string to filter which regions should be shown (by default all, if global is FALSE).
#' @param perYear Is the data perYear? If TRUE, the visualization will visualize the accumulated
#' value per year.
#' @param total Whether the total of all data values should also be visualized (by default TRUE).
#' @author Pascal Sauer, Patrick Rein
#' @importFrom rlang .data
#' @export
mplot <- function(px, dim3 = "", global = TRUE, dim1 = "", perYear = FALSE, total = FALSE) {

  rlang::check_installed("ggplot2")

  originalDimNames <- names(dimnames(px))
  firstDimensionPart <- function(aName) strsplit(aName, ".", fixed = TRUE)[[1]][[1]]
  temporalCategorial <- FALSE

  # Ensure all dimensions have items
  # or are somewhat normalized
  if (is.null(getItems(px, 1))) {
    getItems(px, 1) <- "global"
  }
  if (is.null(getItems(px, 2))) {
    getItems(px, 2) <- "timeless"
    temporalCategorial <- TRUE
  }
  if (is.null(getItems(px, 3))) {
    getItems(px, 3) <- "total"
  }

  # Rewrite names of data dimension to make them symbols
  # Simplify and keep the data dimension name
  px <- px[, , dim3, pmatch = TRUE]
  getNames(px) <- gsub(".", "_", getNames(px), fixed = TRUE)
  dataDimName <- firstDimensionPart(originalDimNames[[3]])
  names(dimnames(px))[[3]] <- dataDimName

  # Retrieve the temporal dimension name and potentially
  # make temporal items symbols
  temporalDimName <- originalDimNames[[2]]
  if (grepl(".", temporalDimName, fixed = TRUE)) {
    # We have subdimensions, convert to symbols
    dimnames(px)[[2]] <- gsub(".", "_", dimnames(px)[[temporalDimName]], fixed = TRUE)
    temporalDimName <- "temporal"
    names(dimnames(px))[[2]] <- temporalDimName
    temporalCategorial <- TRUE
  }

  spatialDimName <- originalDimNames[[1]]
  if (global) {
    # Transform to global values
    px <- dimSums(px, 1)
    spatialDimName <- names(dimnames(px))[[1]]
  } else {
    # Rewrite names of spatial dimension to symbols
    if (grepl(".", spatialDimName, fixed = TRUE)) {
      # We have subdimensions, convert to symbols
      dimnames(px)[[1]] <- gsub(".", "_", dimnames(px)[[spatialDimName]], fixed = TRUE)
      spatialDimName <- "spatial"
      names(dimnames(px))[[1]] <- spatialDimName
    }
  }

  # Calculate and add total
  # If temporal is categorial, we will render stacked bar charts and
  # do not need the total
  if (total && !temporalCategorial) {
    if (ndata(px) > 1) {
      px <- mbind(px, magclass::setNames(dimSums(px, 3), "total"))
    } else {
      getNames(px) <- "total"
    }
  }

  # Convert to data.frame
  if (global) {
    px <- as.data.frame(px, rev = 3)
  } else {
    px <- as.data.frame(px[dim1, , , pmatch = TRUE], rev = 3)
  }

  # Plot
  plot <- ggplot2::ggplot(data = px, ggplot2::aes(x = .data[[temporalDimName]],
                                                  color = .data[[dataDimName]],
                                                  group = .data[[dataDimName]]))

  if (!temporalCategorial) {
    plot <- plot + ggplot2::geom_line(linewidth = 1.5, ggplot2::aes(y = .data[[".value"]]))
  } else {
    plot <- plot + ggplot2::geom_bar(linewidth = 1.5, ggplot2::aes(weight = .data[[".value"]]))
  }

  if (!global) {
    plot <- plot + ggplot2::facet_wrap(~.data[[spatialDimName]])
  }

  print(plot)
}
