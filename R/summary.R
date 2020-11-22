#' Summarize a Gaussian mixture model or graphical model
#'
#' This function summarizes a Gaussian mixture model or graphical model.
#'
#' @param object An object of class \code{gmm}, \code{gmbn} or \code{gmdbn}.
#' @param \dots Unused arguments from the generic function.
#'
#' @return If \code{object} is a \code{gmm} object, an integer vector containing
#' the number of variables, mixture components and free parameters.
#'
#' If \code{object} is a \code{gmbn} or \code{gmdbn} object, a list with
#' elements:
#' \item{global}{An integer vector containing the global number of nodes, arcs,
#' mixture components and free parameters (for a \code{gmdbn} object, also the
#' number of \code{gmbn} elements).}
#' \item{local}{For a \code{gmbn} object, an integer matrix containing the local
#' numbers of arcs, mixture components and free parameters. For a \code{gmdbn}
#' object, a list of integer matrices containing these statistics for each
#' \code{gmbn} elements.}
#'
#' @examples
#' data(gmm_body)
#' summ_1 <- summary(gmm_body)
#'
#' data(gmbn_body)
#' summ_2 <- summary(gmbn_body)
#'
#' data(gmdbn_air)
#' summ_3 <- summary(gmdbn_air)
#'
#' @name summary
#' @export

summary.gmm <- function(object, ...) {
  if (!inherits(object, "gmm")) {
    "object is not of class \"gmm\"" %>%
      stop()
  }
  n_var <- object$mu %>%
    nrow()
  n_comp <- object$alpha %>%
    length()
  n_param <- as.integer(n_comp * (0.5 * n_var * (n_var + 3) + 1) - 1)
  c(n_var = n_var, n_comp = n_comp, n_param = n_param) %>%
    return()
}

#' @rdname summary
#' @export

summary.gmbn <- function(object, ...) {
  if (!inherits(object, "gmbn")) {
    "object is not of class \"gmbn\"" %>%
      stop()
  }
  local <- object %>%
    map(summary) %>%
    do.call("cbind", .)
  rownames(local) <- c("n_arcs", "n_comp", "n_param")
  local["n_arcs", ] <- as.integer(local["n_arcs", ] - 1)
  global <- local %>%
    rowSums() %>%
    as.integer() %>%
    c(length(object), .) %>%
    set_names(c("n_nodes", "n_arcs", "n_comp", "n_param"))
  list(global = global, local = local) %>%
    return()
}

#' @rdname summary
#' @export

summary.gmdbn <- function(object, ...) {
  if (!inherits(object, "gmdbn")) {
    "object is not of class \"gmdbn\"" %>%
      stop()
  }

  summ <- object %>%
    map(summary) %>%
    transpose()
  global <- summ$global %>%
    reduce(`+`) %>%
    as.integer() %>%
    c(length(object), .) %>%
    set_names(c("n_gmbn", "n_nodes", "n_arcs", "n_comp", "n_param"))
  list(global = global, local = summ$local) %>%
    return()
}
