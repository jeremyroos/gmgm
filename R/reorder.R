#' Reorder the variables and the mixture components of a Gaussian mixture model
#'
#' This function reorders the variables and the mixture components of a Gaussian
#' mixture model.
#'
#' @param gmm An object of class \code{gmm}.
#' @param var A character vector containing the variables in the desired order.
#' If variables are not specified, they are added after the ordered ones. If
#' \code{NULL} (the default), the variables are not reordered.
#' @param comp An integer vector containing the indexes of the mixture component
#' in the desired order. If components are not specified, they are added after
#' the ordered ones. If \code{NULL} (the default), the components are not
#' reordered.
#'
#' @return The reordered \code{gmm} object.
#'
#' @examples
#' data(gmm_body)
#' gmm_1 <- reorder(gmm_body, var = c("WAIST", "AGE", "FAT", "HEIGHT", "WEIGHT"),
#'                  comp = c(2, 1, 3))
#'
#' @export

reorder <- function(gmm, var = NULL, comp = NULL) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  mu <- gmm$mu
  var_gmm <- mu %>%
    rownames()

  if (!is.null(var)) {
    if (!is.vector(var, "character")) {
      "var is not a character vector" %>%
        stop()
    }

    if (any(duplicated(var))) {
      "var has duplicated elements" %>%
        stop()
    }

    if (any(!(var %in% var_gmm))) {
      "elements of var are not variables of gmm" %>%
        stop()
    }
  }

  alpha <- gmm$alpha
  seq_comp <- alpha %>%
    seq_along()

  if (!is.null(comp)) {
    if (!is.vector(comp, "numeric")) {
      "comp is not a numeric vector" %>%
        stop()
    }

    if (any(duplicated(comp))) {
      "comp has duplicated elements" %>%
        stop()
    }

    if (any(!(comp %in% seq_comp))) {
      "elements of comp are invalid component indexes" %>%
        stop()
    }
  }

  var <- var %>%
    c(setdiff(var_gmm, var))
  comp <- comp %>%
    c(setdiff(seq_comp, comp))
  alpha <- alpha[comp]
  mu <- mu[var, comp, drop = FALSE]
  sigma <- gmm$sigma[comp] %>%
    map(function(sigma) {
      sigma[var, var, drop = FALSE] %>%
        return()
    })
  alpha %>%
    gmm(mu, sigma) %>%
    return()
}
