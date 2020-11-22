#' Split a mixture component of a Gaussian mixture model
#'
#' This function splits a mixture component of a Gaussian mixture model using
#' the singular value decomposition of the covariance matrix (Zhang
#' \emph{et al.}, 2003).
#'
#' @param gmm An object of class \code{gmm}.
#' @param comp An integer corresponding to the index of the split mixture
#' component.
#' @param n_sub A positive integer corresponding to the number of subcomponents.
#' @param space A numeric value in [0, 1[ corresponding to the space between the
#' subcomponents.
#'
#' @return The \code{gmm} object after splitting the mixture component.
#'
#' @references
#' Zhang, Z., Chen, C., Sun, J. and Chan, K. L. (2003). EM algorithms for
#' Gaussian mixtures with split-and-merge operation. \emph{Pattern Recognition},
#' 36(9):1973--1983.
#'
#' @seealso \code{\link{merge_comp}}
#'
#' @examples
#' data(gmm_body)
#' gmm_1 <- split_comp(gmm_body, n_sub = 3)
#'
#' @export

split_comp <- function(gmm, comp = 1, n_sub = 2, space = 0.5) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  alpha <- gmm$alpha

  if (!is.null(comp) & !is.vector(comp, "numeric")) {
    "comp is not a numeric value" %>%
      stop()
  }

  n_comp <- comp %>%
    length()

  if (n_comp > 0) {
    if (n_comp > 1) {
      "comp is of length greater than 1" %>%
        stop()
    }

    seq_comp <- alpha %>%
      seq_along()

    if (!(comp %in% seq_comp)) {
      "comp is an invalid component index" %>%
        stop()
    }

    if (!is.vector(n_sub, "numeric")) {
      "n_sub is not a numeric value" %>%
        stop()
    }

    if (length(n_sub) != 1) {
      "n_sub is not of length 1" %>%
        stop()
    }

    if (!is.finite(n_sub)) {
      "n_sub is not finite" %>%
        stop()
    }

    if (n_sub <= 0) {
      "n_sub is not positive" %>%
        stop()
    }

    if (round(n_sub) != n_sub) {
      "n_sub is not an integer" %>%
        stop()
    }

    if (n_sub > 1) {
      if (!is.vector(space, "numeric")) {
        "space is not a numeric value" %>%
          stop()
      }

      if (length(space) != 1) {
        "space is not of length 1" %>%
          stop()
      }

      if (is.na(space)) {
        "space is NA" %>%
          stop()
      }

      if (space < 0 | space >= 1) {
        "space is not in [0, 1[" %>%
          stop()
      }

      comp_fix <- seq_comp %>%
        setdiff(comp)
      alpha_fix <- alpha[comp_fix]
      mu <- gmm$mu
      mu_fix <- mu[, comp_fix, drop = FALSE]
      sigma <- gmm$sigma
      sigma_fix <- sigma[comp_fix]
      alpha_split <- alpha[comp] / n_sub

      if (alpha_split == 0) {
        alpha <- alpha_fix / sum(alpha_fix)
        mu <- mu_fix
        sigma <- sigma_fix
      } else {
        alpha_split <- alpha_split %>%
          rep(n_sub)
        sigma_comp <- sigma[[comp]]
        eigen_sigma <- sigma_comp %>%
          eigen(TRUE)
        space_sigma <- space * sqrt(eigen_sigma$values[1]) *
          eigen_sigma$vectors[, 1]
        mu_split <- space_sigma %*%
          t((2 * seq_len(n_sub) - n_sub - 1) * sqrt(3 / (n_sub ^ 2 - 1))) +
          mu[, comp]
        sigma_split <- list(sigma_comp - space_sigma %*% t(space_sigma)) %>%
          rep(n_sub)
        order_comp <- comp_fix %>%
          c(rep(comp, n_sub)) %>%
          order()
        alpha <- alpha_fix %>%
          c(alpha_split) %>%
          .[order_comp]
        mu <- mu_fix %>%
          cbind(mu_split) %>%
          .[, order_comp, drop = FALSE]
        sigma <- sigma_fix %>%
          c(sigma_split) %>%
          .[order_comp]
      }

      gmm <- alpha %>%
        gmm(mu, sigma)
    }
  }

  gmm %>%
    return()
}
