#' sin/cos cosinor operations
#'
#' Converts sine and cosine coeeficients to acrophase/amplitude
#'
#' @name sincos
#'
#' @param sin - sine coefficient returned from cosinor model.
#' @param cos - cosine coefficient returned from cosinor model.
#' @param per - period of oscillation (default = 24).
#'
#' @return acrophase
#'
#' @examples
#' \dontrun{ # don't run since internal
#' sincos2acr(0.5, 0.5, per = 24)
#' sincos2amp(0.5, 0.5)
#' }
#' @author Matthew Carlucci
#' @keywords internal
sincos2acr <- function(sin, cos, per = 24) {
    (atan2(sin, cos) / 2 / pi * per + per) %% per
}

#' @rdname sincos
#' @inheritParams sincos2acr
#' @keywords internal
sincos2amp <- function(sin, cos) sqrt(sin^2 + cos^2)
