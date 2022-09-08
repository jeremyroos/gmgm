#' Create a Gaussian mixture model
#'
#' This function creates a Gaussian mixture model as an object of S3 class
#' \code{gmm}. A Gaussian mixture model is a weighted sum of multivariate
#' Gaussian distributions:
#' \deqn{p(x) = \sum_{i = 1}^M \alpha_i \mathcal{N}(x | \mu_i, \Sigma_i)}
#' where \eqn{\alpha_i} is the \eqn{i}th mixture proportion such that
#' \eqn{\alpha_i > 0} and \eqn{\sum_{i = 1}^M \alpha_i = 1}, \eqn{\mu_i} the
#' mean vector and \eqn{\Sigma_i} the covariance matrix of the \eqn{i}th mixture
#' component (Bilmes, 1998). Since conditional distributions can be derived from
#' joint distributions, the \code{gmm} class is also used to work with
#' conditional Gaussian mixture models (see function \code{\link{conditional}}
#' to explicit their parameters). Finally, note that a one-component Gaussian
#' mixture model can be created with function \code{\link{add_var}} (by passing
#' \code{NULL} as argument \code{gmm}), which allows to quickly initialize a
#' \code{gmm} object that can be passed to a learning function.
#'
#' @param alpha A positive numeric vector containing the mixture proportions. If
#' the sum of these proportions is not 1, a normalization is performed by
#' dividing them by this sum.
#' @param mu A numeric matrix containing the mean vectors bound by column.
#' @param sigma A list containing the covariance matrices.
#' @param var A character vector containing the variable names (by default the
#' row names of \code{mu}).
#'
#' @return A list of class \code{gmm} containing the elements \code{alpha},
#' \code{mu} and \code{sigma} passed as arguments (completed with the variable
#' names passed as argument \code{var}).
#'
#' @references
#' Bilmes, J. A. (1998). A Gentle Tutorial of the EM Algorithm and its
#' Application to Parameter Estimation for Gaussian Mixture and Hidden Markov
#' Models. Technical report, International Computer Science Institute.
#'
#' @seealso \code{\link{gmbn}}, \code{\link{gmdbn}}
#'
#' @examples
#' gmm_1 <- gmm(alpha = c(0.2, 0.5, 0.3),
#'              mu = matrix(c(109, 91, 44, 160, 41, 99, 87, 27, 173, 40, 86, 65,
#'                            35, 161, 40),
#'                          nrow = 5),
#'              sigma = list(matrix(c(208, 240, 32, 17, -6, 240, 378, 40, 55, -38,
#'                                    32, 40, 15, -2, 1, 17, 55, -2, 47, -13, -6,
#'                                    -38, 1, -13, 127),
#'                                  nrow = 5),
#'                           matrix(c(242, 270, 82, 10, 49, 270, 363, 83, 44, 19,
#'                                    82, 83, 38, -2, 15, 10, 44, -2, 45, -7, 49,
#'                                    19, 15, -7, 137),
#'                                  nrow = 5),
#'                           matrix(c(109, 102, 41, 11, 29, 102, 128, 34, 38, 10,
#'                                    41, 34, 36, -9, 16, 11, 38, -9, 56, -5, 29,
#'                                    10, 16, -5, 138),
#'                                  nrow = 5)),
#'              var = c("WAIST", "WEIGHT", "FAT", "HEIGHT", "AGE"))
#'
#' @export

gmm <- function(alpha, mu, sigma, var = rownames(mu)) {
  if (!is.vector(alpha, "numeric")) {
    "alpha is not a numeric vector" %>%
      stop()
  }

  n_comp <- alpha %>%
    length()

  if (n_comp == 0) {
    "alpha is empty" %>%
      stop()
  }

  if (any(!is.finite(alpha))) {
    "alpha has non-finite elements" %>%
      stop()
  }

  if (any(alpha <= 0)) {
    "alpha has non-positive elements" %>%
      stop()
  }

  if (n_comp == 1) {
    alpha <- 1
  } else {
    alpha <- alpha %>%
      unname()
    sum_alpha <- alpha %>%
      sum()

    if (abs(sum_alpha - 1) > .Machine$double.eps) {
      alpha <- alpha / sum_alpha

      if (any(alpha <= 0)) {
        "alpha has non-positive elements" %>%
          stop()
      }
    }
  }

  if (!is.matrix(mu)) {
    "mu is not a numeric matrix" %>%
      stop()
  }

  n_var <- mu %>%
    nrow()

  if (n_var == 0) {
    "mu has no row" %>%
      stop()
  }

  if (ncol(mu) != n_comp) {
    "alpha and mu have incompatible sizes" %>%
      stop()
  }

  if (!is.numeric(mu)) {
    "mu is not numeric" %>%
      stop()
  }

  if (any(!is.finite(mu))) {
    "mu has non-finite elements" %>%
      stop()
  }

  if (!inherits(sigma, "list")) {
    "sigma is not a list of numeric matrices" %>%
      stop()
  }

  if (length(sigma) != n_comp) {
    "alpha and sigma have different lengths" %>%
      stop()
  }

  sigma %>%
    iwalk(function(sigma, comp) {
      if (!is.matrix(sigma)) {
        "sigma[[" %>%
          str_c(comp, "]] is not a numeric matrix") %>%
          stop()
      }

      if (nrow(sigma) != n_var | ncol(sigma) != n_var) {
        "mu and sigma[[" %>%
          str_c(comp, "]] have incompatible sizes") %>%
          stop()
      }

      if (!is.numeric(sigma)) {
        "sigma[[" %>%
          str_c(comp, "]] is not numeric") %>%
          stop()
      }

      if (any(!is.finite(sigma))) {
        "sigma[[" %>%
          str_c(comp, "]] has non-finite elements") %>%
          stop()
      }

      if (!isSymmetric(unname(sigma))) {
        "sigma[[" %>%
          str_c(comp, "]] is not symmetric") %>%
          stop()
      }

      if (inherits(tryCatch(chol(sigma), error = function(e) {return(e)}),
                   "error")) {
        "sigma[[" %>%
          str_c(comp, "]] is not positive definite") %>%
          stop()
      }
    })

  if (!is.vector(var, "character")) {
    "var is not a character vector" %>%
        stop()
  }

  if (length(var) != n_var) {
    "mu and var have incompatible sizes" %>%
      stop()
  }

  if (any(duplicated(var))) {
    "var has duplicated elements" %>%
      stop()
  }

  if (any(is.na(var))) {
    "var has NA elements" %>%
      stop()
  }

  if (any(!str_detect(var,
                      "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$"))) {
    "var contains invalid variable names" %>%
      stop()
  }

  mu <- mu %>%
    as.numeric() %>%
    matrix(n_var, dimnames = list(var))
  dimnames_sigma <- var %>%
    list(var)
  sigma <- sigma %>%
    map(function(sigma) {
      sigma %>%
        as.numeric() %>%
        matrix(n_var, dimnames = dimnames_sigma) %>%
        return()
    }) %>%
    unname()
  gmm <- list(alpha = alpha, mu = mu, sigma = sigma)
  class(gmm) <- "gmm"
  gmm %>%
    return()
}
