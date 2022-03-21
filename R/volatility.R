#' Functions to handle volatility calculations issues
#'
#' @name volatility
#' @aliases volatility
#' @title Functions to handle volatility calculations
#' @author Wilson Freitas \email{wilson.freitas@@gmail.com}
#' @docType package
NULL

#' Volatility conversion functions
#'
#' Volatility is an entity. It has attributes: \code{dib} (days in base) and
#' \code{annualized} (to inform if it is annual or daily volatility).
#' It usually is reported annualized and in percentual units, but one can use
#' it in a daily basis.
#'
#' @name volatility-class
#' @export
as.volatility <- function(x, ...) UseMethod("as.volatility")

#' @rdname volatility-class
#' @export
as.volatility.default <- function(x, annualized = FALSE, dib = NULL, ...) {
  if (annualized & is.null(dib)) {
    stop("Annualized data with NULL dib")
  }
  structure(x, annualized = annualized, dib = dib, class = "volatility")
}

#' @rdname volatility-class
#' @export
as.volatility.variance <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  structure(sqrt(y),
    annualized = attr(x, "annualized"), dib = attr(x, "dib"),
    class = "volatility"
  )
}

#' @export
print.volatility <- function(x, ...) {
  cat(
    if (attr(x, "annualized")) "Annual" else "Daily",
    "Volatility",
    if (attr(x, "annualized")) paste(attr(x, "dib"), "days"),
    "\n"
  )
  p <- 1 # if (attr(x, "annualized")) 100 else 1
  y <- unclass(x) * p
  attributes(y) <- NULL
  print(y)
  invisible(x)
}