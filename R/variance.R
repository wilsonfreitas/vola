#' Variance conversion functions
#'
#' Variance is an entity. It has attributes: \code{dib} (days in base) and
#' \code{annualized} (to inform if it is annual or daily volatility).
#' It usually is reported in daily units, but one can use it annualized.
#'
#' @name variance-class
#' @export
as.variance <- function(x, ...) UseMethod("as.variance")

#' @rdname variance-class
#' @export
as.variance.default <- function(x, annualized = FALSE, dib = NULL, ...) {
  if (annualized & is.null(dib)) {
    stop("Annualized data with NULL dib")
  }
  structure(x, annualized = annualized, dib = dib, class = "variance")
}

#' @rdname variance-class
#' @export
as.variance.volatility <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  structure(y^2,
    annualized = attr(x, "annualized"), dib = attr(x, "dib"),
    class = "variance"
  )
}

#' @export
print.variance <- function(x, ...) {
  cat(
    if (attr(x, "annualized")) "Annual" else "Daily",
    "Variance",
    if (attr(x, "annualized")) paste(attr(x, "dib"), "days"),
    "\n"
  )
  y <- unclass(x)
  attributes(y) <- NULL
  print(y)
  invisible(x)
}