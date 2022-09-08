#' Compute the Akaike Information Criterion (AIC) of a Gaussian mixture model or
#' graphical model
#'
#' This function computes the Akaike Information Criterion (AIC) of a Gaussian
#' mixture model or graphical model:
#' \deqn{AIC = logLik - n_{par}}
#' where \eqn{logLik} is the log-likelihood and \eqn{n_{par}} the number of free
#' parameters.
#'
#' @param object An object of class \code{gmm}, \code{gmbn} or \code{gmdbn}.
#' @param data A data frame containing the data used to compute the AIC. Its
#' columns must explicitly be named after the variables (or nodes) of
#' \code{object}. If \code{object} is a \code{gmm} object, a numeric matrix can
#' be passed.
#' @param y A character vector containing the dependent variables if a
#' conditional AIC is computed. If \code{NULL} (the default), the joint AIC is
#' computed.
#' @param regul A positive numeric value corresponding to the regularization
#' constant if a penalty term is added for Bayesian regularization. If
#' \code{NULL}, no penalty term is added. If a conditional AIC is computed, this
#' argument is ignored.
#' @param \dots Unused arguments from the generic function.
#' @param col_seq A character vector containing the column names of \code{data}
#' that describe the observation sequence. If \code{NULL} (the default), all the
#' observations belong to a single sequence. If \code{object} is a temporal
#' \code{gmbn} or \code{gmdbn} object, the observations of a same sequence must
#' be ordered such that the \eqn{t}th one is related to time slice \eqn{t}
#' (note that the sequences can have different lengths). If \code{object} is a
#' non-temporal \code{gmbn} object, this argument is ignored.
#'
#' @return If \code{object} is a \code{gmm} object, a numeric value
#' corresponding to the AIC.
#'
#' If \code{object} is a \code{gmbn} or \code{gmdbn} object, a list with
#' elements:
#' \item{global}{A numeric value corresponding to the global AIC.}
#' \item{local}{For a \code{gmbn} object, a numeric vector containing the local
#' conditional AICs. For a \code{gmdbn} object, a list of numeric vectors
#' containing these values for each \code{gmbn} element.}
#'
#' @seealso \code{\link{BIC}}, \code{\link{logLik}}
#'
#' @examples
#' data(gmm_body, data_body)
#' aic_1 <- AIC(gmm_body, data_body)
#' aic_2 <- AIC(gmm_body, data_body, y = "WAIST")
#'
#' data(gmbn_body, data_body)
#' aic_3 <- AIC(gmbn_body, data_body)
#'
#' data(gmdbn_air, data_air)
#' aic_4 <- AIC(gmdbn_air, data_air, col_seq = "DATE")
#'
#' @name AIC
#' @export

AIC.gmm <- function(object, data, y = NULL, regul = 0.01, ...) {
  return(logLik.gmm(object, data, y = y, regul = regul) -
           summary.gmm(object)[["n_param"]])
}

#' @rdname AIC
#' @export

AIC.gmbn <- function(object, data, col_seq = NULL, ...) {
  loglik <- object %>%
    logLik.gmbn(data, col_seq = col_seq)
  summ <- object %>%
    summary.gmbn()
  global <- loglik$global - summ$global[["n_param"]]
  local <- loglik$local - summ$local["n_param", ]
  list(global = global, local = local) %>%
    return()
}

#' @rdname AIC
#' @export

AIC.gmdbn <- function(object, data, col_seq = NULL, ...) {
  loglik <- object %>%
    logLik.gmdbn(data, col_seq = col_seq)
  summ <- object %>%
    summary.gmdbn()
  global <- loglik$global - summ$global[["n_param"]]
  local <- loglik$local %>%
    map2(summ$local, function(loglik, summ) {
      return(loglik - summ["n_param", ])
    })
  list(global = global, local = local) %>%
    return()
}
