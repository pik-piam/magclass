#' magpie_expand
#'
#' Expands a MAgPIE object based on a reference
#'
#' Expansion means here that the dimensions of x are expanded accordingly to
#' ref. Please note that this is really only about expansion. In the case that
#' one dimension of ref is smaller than of x nothing happens with this
#' dimension. At the moment magpie_expand is only internally available in the
#' magclass library
#'
#' You can influence the verbosity of this function by setting the option
#' "magclass.verbosity". By default verbosity is set to 1 which means that
#' only warnings are returned. Setting verbosity to 2 means that warnings
#' as well as additional notes are returned. This is done by
#' options(verbosity.level=2)
#'
#' With version 5 of the package magpie_expand has been updated to a newer version
#' (currently 2.1) and since version 6 this is the only currently supported
#' version. To switch to the old setup you have to install magclass  in a version < 6
#' and set \code{options(magclass_expand_version=1)}.
#'
#' By default expansion is based on the elements in a dimension ignoring the set name of
#' the dimension. To expand based on set names instead of contents (recommended) you can
#' switch \code{options(magclass_setMatching=TRUE)}. Please be careful with this setting
#' as it alters the behavior of magclass objects quite significantly! For more information
#' have a look at \code{vignette("magclass-expansion")}.
#'
#' @param x MAgPIE object that should be expanded
#' @param ref MAgPIE object that serves as a reference
#' @return An expanded version of x.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{as.magpie}}, \code{\link[base]{options}}
#' @examples
#'
#' a <- new.magpie(c("AFR", "CPA"), "y1995", c("m", "n"))
#' b <- new.magpie("GLO", "y1995", c("bla", "blub"))
#' magpie_expand(b, a)
#' options(magclass.verbosity = 2)
#' magpie_expand(b, a)
#' @export
magpie_expand <- function(x, ref) { # nolint

  version <- getOption("magclass_expand_version")
  if (is.null(version)) options("magclass_expand_version" = 2.1) #nolint
  if (!is.null(version) && version != 2.1) stop("Unsupported magclass expand version (", version, ")!")

  setMatching <- isTRUE(getOption("magclass_setMatching"))

  # x: MAgPIE object which should be expanded
  # ref: Reference object defining the structure to which x should be expanded

  # 1.spatial dimension
  # 2.temporal dimension
  # 3.data dimension

  for (i in 1:3) {
    # Remove "GLO" from spatial dimension in case another spatial dimension exists
    if (i == 1) {
      if (dim(ref)[1] == 1 && !is.null(rownames(ref)) && rownames(ref) == "GLO") rownames(ref) <- NULL
      else if (dim(x)[1] == 1 && !is.null(rownames(x)) && rownames(x) == "GLO") rownames(x) <- NULL
    }

    if (is.null(dimnames(ref)[[i]]) && dim(ref)[i] > 1) {
      stop("Inconsistent MAgPIE reference file: more than 1 element in dimension ", i, " but no names given!")
    }

    if (is.null(dimnames(ref)[[i]])) next # Nothing to do if ref has no dimnames

    if (is.null(dimnames(x)[[i]])) {
      if (dim(x)[i] > 1) stop("Inconsistent MAgPIE file: more than 1 element in dimension ", i, " but no names given!")
      if (dim(ref)[i] > 1) { # Expand single element dimension to dimension of ref
         x <- x[rep(1, dim(ref)[i]), dim = i]
      }
      if (!is.null(dimnames(ref)[[i]])) dimnames(x)[[i]] <- dimnames(ref)[[i]]
    } else if (dim(x)[i] == dim(ref)[i] && all(dimnames(x)[[i]] == dimnames(ref)[[i]]) &&
      (!setMatching || names(dimnames(x))[i] == names(dimnames(ref))[i])) {
      # dimension is identical
      next
    } else if (dim(x)[i] == dim(ref)[i] && all(sort(dimnames(x)[[i]]) == sort(dimnames(ref)[[i]])) &&
      (!setMatching || names(dimnames(x))[i] == names(dimnames(ref))[i])) {
      # same length and entries, but different order
      x <- x[dimnames(ref)[[i]], dim = i]
    } else {
      x <- magpie_expand_dim(x, ref, dim = i)
    }
  }
  getSets(x) <- make.unique(getSets(clean_magpie(x, what = "sets")), sep = "")
  return(x)
}
