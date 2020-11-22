#' Rename variables of a Gaussian mixture model
#'
#' This function renames variables of a Gaussian mixture model.
#'
#' @param gmm An object of class \code{gmm}.
#' @param var A character vector containing the renamed variables.
#' @param names A character vector containing the respective new names of the
#' variables.
#'
#' @return The \code{gmm} object after renaming the variables.
#'
#' @seealso \code{\link{add_var}}, \code{\link{remove_var}}
#'
#' @examples
#' data(gmm_body)
#' gmm_1 <- rename_var(gmm_body, "FAT", "BODY_FAT")
#'
#' @export

rename_var <- function(gmm, var, names) {
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

  if (!is.null(names)) {
    if (!is.vector(names, "character")) {
      "names is not a character vector" %>%
        stop()
    }

    if (any(duplicated(names))) {
      "names has duplicated elements" %>%
        stop()
    }
  }

  if (length(var) != length(names)) {
    "var and names have different lengths" %>%
      stop()
  }

  var_gmm[match(var, var_gmm)] <- names
  gmm$alpha %>%
    gmm(mu, gmm$sigma, var = var_gmm) %>%
    return()
}
