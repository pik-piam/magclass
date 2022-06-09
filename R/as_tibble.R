#' magpie method for tibble::as_tibble
#'
#' @param x A magpie object
#' @inheritParams tibble::as_tibble
#' @return A tibble object
#' @exportS3Method tibble::as_tibble
as_tibble.magpie <- function(x, # nolint
                             ...,
                             .rows = NULL,
                             .name_repair = c("check_unique", "unique", "universal", "minimal"), # nolint
                             rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
  # Turn the magpie object into a data.frame and rename ".value" to "value" - the default column name for values in
  # tibbles.
  y <- as.data.frame(x, rev = 2)
  names(y)[which(colnames(y) == ".value")] <- "value"
  # Turn the data.frame into a tibble
  tibble::as_tibble(x = y,
                    ...,
                    .rows = .rows,
                    .name_repair = .name_repair,
                    rownames = rownames)
}
