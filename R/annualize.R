#' Functions to annualize data
#'
#' Annualize data: volatility and variance
#'
#' @name annualize
#' @export
annualize <- function(x, ...) UseMethod("annualize")

#' @rdname annualize
#' @export
annualize.volatility <- function(x, dib = 252, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  if (attr(x, "annualized")) {
    return(x)
  } else {
    y <- y * sqrt(dib)
  }
  as.volatility(y, annualized = TRUE, dib = dib)
}

#' @rdname annualize
#' @export
annualize.variance <- function(x, dib = 252, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  if (attr(x, "annualized")) {
    return(x)
  } else {
    y <- y * dib
  }
  as.variance(y, annualized = TRUE, dib = dib)
}

#' @rdname annualize
#' @export
daily <- function(x, ...) UseMethod("daily")

#' @rdname annualize
#' @export
daily.volatility <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  if (!attr(x, "annualized")) {
    return(x)
  } else {
    y <- y / (sqrt(attr(x, "dib")))
  }
  as.volatility(y, annualized = FALSE, dib = NULL)
}

#' @rdname annualize
#' @export
daily.variance <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  if (!attr(x, "annualized")) {
    return(x)
  } else {
    y <- y / attr(x, "dib")
  }
  as.variance(y, annualized = FALSE, dib = NULL)
}