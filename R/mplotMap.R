#' mPlotMap
#'
#' Render a simple world map of a coordinate-based (cell) magpie object. Each
#' grid cell is drawn as a colored tile on top of country outlines. If the object
#' contains more than one year or data name, the remaining dimensions are spread
#' across facets, so it is usually best to subset the object to the slice(s) of
#' interest before plotting.
#'
#' @param px The magpie object to be visualized. It must contain spatial
#' coordinates (see \code{\link{hasCoords}}).
#' @return Invisibly returns the ggplot object.
#' @author Patrick Rein
#' @family Display
#' @seealso \code{\link{mplot}}, \code{\link{hasCoords}}
#' @examples
#' \dontrun{
#' a <- maxample("animal")
#' mPlotMap(a[, 1, 1])
#' }
#' @importFrom rlang .data
#' @export
mPlotMap <- function(px) {

  rlang::check_installed("ggplot2")

  if (!hasCoords(px)) {
    stop("mPlotMap requires a magpie object with spatial coordinates ",
         "(see hasCoords). Region-based objects cannot be mapped.")
  }

  df <- as.data.frame(px, rev = 3)
  dimtype <- attr(df, "dimtype")

  # Facet over all temporal and data dimensions that vary; ignore extra spatial
  # subdimensions (e.g. country, cell) and keep only the x/y coordinates.
  facetCandidates <- names(df)[startsWith(dimtype, ".temp") | startsWith(dimtype, ".data")]
  facetCols <- facetCandidates[vapply(df[facetCandidates],
                                      function(col) length(unique(col)) > 1,
                                      logical(1))]
  if (length(facetCols) > 0) {
    df$.label <- do.call(paste, c(df[facetCols], sep = "."))
  }

  plot <- ggplot2::ggplot(df)

  if (requireNamespace("maps", quietly = TRUE)) {
    world <- maps::map("world", plot = FALSE, fill = TRUE)
    world <- data.frame(long = world$x, lat = world$y,
                        group = cumsum(is.na(world$x) & is.na(world$y)))
    plot <- plot +
      ggplot2::geom_polygon(data = world,
                            ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
                            colour = "grey70", fill = "grey95", linewidth = 0.2,
                            inherit.aes = FALSE)
  }

  # Zoom the map to the extent of the data, so a small region does not get lost
  # on a full world map. Pad by at least half a cell (to avoid clipping edge
  # tiles) plus a small context margin around the data.
  res <- guessResolution(px)
  pad <- function(r) max(res / 2, 0.05 * diff(r))
  xr <- range(df$x)
  yr <- range(df$y)
  xpad <- pad(xr)
  ypad <- pad(yr)

  plot <- plot +
    ggplot2::geom_tile(ggplot2::aes(x = .data$x, y = .data$y, fill = .data$.value)) +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::coord_quickmap(xlim = xr + c(-xpad, xpad), ylim = yr + c(-ypad, ypad)) +
    ggplot2::labs(x = "lon", y = "lat", fill = "value") +
    ggplot2::theme_minimal()

  if (length(facetCols) > 0) {
    plot <- plot + ggplot2::facet_wrap(ggplot2::vars(.data$.label))
  }

  invisible(print(plot))
}
