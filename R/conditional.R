#' Explicit the parameters of a conditional Gaussian mixture model
#'
#' This function explicits the parameters of a conditional Gaussian mixture
#' model (Sun \emph{et al.}, 2006).
#'
#' @param gmm An object of class \code{gmm}.
#' @param y A character vector containing the dependent variables (by default
#' the first variable of \code{gmm}).
#'
#' @return A list with elements:
#' \item{alpha}{A numeric vector containing the mixture proportions.}
#' \item{mu_x}{A numeric matrix containing the marginal mean vectors of the
#' explanatory variables bound by column.}
#' \item{sigma_x}{A list containing the marginal covariance matrices of the
#' explanatory variables.}
#' \item{coeff}{A list containing the regression coefficient matrices of
#' the dependent variables on the explanatory variables.}
#' \item{sigma_c}{A list containing the conditional covariance matrices.}
#'
#' @references
#' Sun, S., Zhang, C. and Yu, G. (2006). A Bayesian Network Approach
#' to Traffic Flow Forecasting. \emph{IEEE Transactions on Intelligent
#' Transportation Systems}, 7(1):124--132.
#'
#' @examples
#' data(gmm_body)
#' cond <- conditional(gmm_body)
#'
#' @export

conditional <- function(gmm, y = rownames(gmm$mu)[1]) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  mu <- gmm$mu
  var_gmm <- mu %>%
    rownames()

  if (!is.vector(y, "character")) {
    "y is not a character vector" %>%
      stop()
  }

  if (length(y) == 0) {
    "y is empty" %>%
      stop()
  }

  if (any(!(y %in% var_gmm))) {
    "elements of y are not variables of gmm" %>%
      stop()
  }

  x <- var_gmm %>%
    setdiff(y)
  mu_x <- mu[x, , drop = FALSE]
  sigma <- gmm$sigma
  sigma_x <- sigma %>%
    map(function(sigma) {
      sigma[x, x, drop = FALSE] %>%
        return()
    })
  alpha <- gmm$alpha
  seq_comp <- alpha %>%
    seq_along()
  t_mu <- mu %>%
    t()

  if (length(x) == 0) {
    coeff <- seq_comp %>%
      map(function(comp) {
        coeff <- t_mu[comp, , drop = FALSE]
        rownames(coeff) <- "(Intercept)"
        coeff %>%
          return()
      })
    sigma_c <- sigma
  } else {
    t_mu_x <- t_mu[, x, drop = FALSE]
    y <- var_gmm %>%
      intersect(y)
    t_mu_y <- t_mu[, y, drop = FALSE]
    sigma_y <- sigma %>%
      map(function(sigma) {
        sigma[y, y, drop = FALSE] %>%
          return()
      })
    sigma_yx <- sigma %>%
      map(function(sigma) {
        sigma[y, x, drop = FALSE] %>%
          return()
      })
    sigma_xy <- sigma %>%
      map(function(sigma) {
        sigma[x, y, drop = FALSE] %>%
          return()
      })
    id_x <- x %>%
      length() %>%
      diag()
    res_cond <- seq_comp %>%
      map(function(comp) {
        inv_chol_sigma_x <- sigma_x[[comp]] %>%
          chol() %>%
          backsolve(id_x)
        s_coeff_x <- inv_chol_sigma_x %>%
          t() %*% sigma_xy[[comp]]
        coeff_x <- inv_chol_sigma_x %*% s_coeff_x
        coeff <- rbind(t_mu_y[comp, , drop = FALSE] -
                         t_mu_x[comp, , drop = FALSE] %*% coeff_x,
                       coeff_x)
        rownames(coeff) <- "(Intercept)" %>%
          c(x)
        sigma_c <- sigma_y[[comp]] - crossprod(s_coeff_x)

        if (inherits(tryCatch(chol(sigma_c), error = function(e) {return(e)}),
                     "error")) {
          id_y <- y %>%
            length() %>%
            diag()
          sigma_c <- sigma_c - last(eigen(sigma_c, TRUE, TRUE)$values) * id_y
          sigma_c_corr <- sigma_c
          base <- .Machine$double.base
          exp <- .Machine$double.min.exp - 1

          while (inherits(tryCatch(chol(sigma_c_corr),
                                   error = function(e) {return(e)}),
                          "error")) {
            exp <- exp + 1
            sigma_c_corr <- sigma_c + base ^ exp * id_y
          }

          sigma_c <- sigma_c_corr
        }

        list(coeff = coeff, sigma_c = sigma_c) %>%
          return()
      }) %>%
      transpose()
    coeff <- res_cond$coeff
    sigma_c <- res_cond$sigma_c
  }

  list(alpha = alpha, mu_x = mu_x, sigma_x = sigma_x, coeff = coeff,
       sigma_c = sigma_c) %>%
    return()
}
