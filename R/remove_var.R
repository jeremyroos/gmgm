#' Remove variables from a Gaussian mixture model
#'
#' This function removes variables from a Gaussian mixture model.
#'
#' @param gmm An object of class \code{gmm}.
#' @param var A character vector containing the removed variables.
#'
#' @return The \code{gmm} object after removing the variables.
#'
#' @seealso \code{\link{add_var}}, \code{\link{rename_var}}
#'
#' @examples
#' data(gmm_body)
#' gmm_1 <- remove_var(gmm_body, "FAT")
#'
#' @export

remove_var <- function(gmm, var) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  if (!is.null(var) & !is.vector(var, "character")) {
    "var is not a character vector" %>%
      stop()
  }

  mu <- gmm$mu
  var_gmm <- mu %>%
    rownames() %>%
    setdiff(var)

  if (length(var_gmm) == 0) {
    "var contains all the variables of gmm" %>%
      stop()
  }

  mu <- mu[var_gmm, , drop = FALSE]
  sigma <- gmm$sigma %>%
    map(function(sigma) {
      sigma[var_gmm, var_gmm, drop = FALSE] %>%
        return()
    })
  gmm$alpha %>%
    gmm(mu, sigma) %>%
    return()
}
