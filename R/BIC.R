#' Compute the Bayesian Information Criterion (BIC) of a Gaussian mixture model
#' or graphical model
#'
#' This function computes the Bayesian Information Criterion (BIC) of a Gaussian
#' mixture model or graphical model:
#' \ifelse{html}{\out{<p style="text-align:center;"><i>BIC</i> = <i>logLik</i>
#' &minus; <sup>log(<i>n<sub>obs</sub></i>)</sup> &frasl; <sub>2</sub>
#' <i>n<sub>par</sub></i></p>}}{\deqn{BIC = logLik - \frac{\log(n_{obs})}{2}
#' n_{par}}}
#' where \eqn{logLik} is the log-likelihood,
#' \ifelse{html}{\out{<i>n<sub>obs</sub></i>}}{\eqn{n_{obs}}} the number of
#' observations in the data and
#' \ifelse{html}{\out{<i>n<sub>par</sub></i>}}{\eqn{n_{par}}} the number of free
#' parameters.
#'
#' @param object An object of class \code{gmm}, \code{gmbn} or \code{gmdbn}.
#' @param data A data frame containing the data used to compute the BIC. Its
#' columns must explicitly be named after the variables (or nodes) of
#' \code{object}. If \code{object} is a \code{gmm} object, a numeric matrix can
#' be passed.
#' @param y A character vector containing the dependent variables if a
#' conditional BIC is computed. If \code{NULL} (the default), the joint BIC is
#' computed.
#' @param regul A positive numeric value corresponding to the regularization
#' constant if a penalty term is added for Bayesian regularization. If
#' \code{NULL} (the default) no penalty term is added. If a conditional BIC is
#' computed, this argument is ignored.
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
#' corresponding to the BIC.
#'
#' If \code{object} is a \code{gmbn} or \code{gmdbn} object, a list with
#' elements:
#' \item{global}{A numeric value corresponding to the global BIC.}
#' \item{local}{For a \code{gmbn} object, a numeric vector containing the local
#' conditional BICs. For a \code{gmdbn} object, a list of numeric vectors
#' containing these values for each \code{gmbn} element.}
#'
#' @seealso \code{\link{AIC}}, \code{\link{logLik}}
#'
#' @examples
#' data(gmm_body, data_body)
#' bic_1 <- BIC(gmm_body, data_body)
#' bic_2 <- BIC(gmm_body, data_body, y = "WAIST")
#'
#' data(gmbn_body, data_body)
#' bic_3 <- BIC(gmbn_body, data_body)
#'
#' data(gmdbn_air, data_air)
#' bic_4 <- BIC(gmdbn_air, data_air, col_seq = "DATE")
#'
#' @name BIC
#' @export

BIC.gmm <- function(object, data, y = NULL, regul = NULL, ...) {
  return(logLik.gmm(object, data, y = y, regul = regul) -
           0.5 * log(nrow(data)) * summary.gmm(object)[["n_param"]])
}

#' @rdname BIC
#' @export

BIC.gmbn <- function(object, data, col_seq = NULL, ...) {
  loglik <- object %>%
    logLik.gmbn(data, col_seq = col_seq)
  summ <- object %>%
    summary.gmbn()
  if (length(col_seq) == 0) {
    col_n <- "n"
  } else {
    col_n <- col_seq %>%
      sort() %>%
      last() %>%
      str_c("_n")
  }
  max_lag <- object %>%
    structure() %>%
    .$arcs %>%
    .$lag %>%
    max(0)
  log_n_obs <- data %>%
    group_by_at(col_seq) %>%
    summarise(!!col_n := n(), .groups = "drop") %>%
    mutate_at(col_n, ~ pmax(. - max_lag, 0)) %>%
    .[[col_n]] %>%
    sum() %>%
    log()
  global <- loglik$global - 0.5 * log_n_obs * summ$global[["n_param"]]
  local <- loglik$local - 0.5 * log_n_obs * summ$local["n_param", ]
  list(global = global, local = local) %>%
    return()
}

#' @rdname BIC
#' @export

BIC.gmdbn <- function(object, data, col_seq = NULL, ...) {
  loglik <- object %>%
    logLik.gmdbn(data, col_seq = col_seq)
  summ <- object %>%
    summary.gmdbn()
  times_gmbn <- object %>%
    names() %>%
    str_remove("b_") %>%
    as.numeric()
  prefix <- col_seq

  if (length(col_seq) > 0) {
    prefix <- prefix %>%
      sort() %>%
      last() %>%
      str_c("_")
  }

  col_n <- prefix %>%
   str_c("n")
  col_time <- prefix %>%
    str_c("time")
  col_diff <- prefix %>%
    str_c("diff")
  n_obs_gmbn <- data %>%
    group_by_at(col_seq) %>%
    summarise(!!col_n := n(), .groups = "drop") %>%
    crossing(tibble(!!col_time := times_gmbn - 1,
                    !!col_diff := c(diff(times_gmbn), Inf))) %>%
    mutate_at(col_n, ~ pmin(!!sym(col_diff), pmax(0, . - !!sym(col_time)))) %>%
    group_by_at(col_time) %>%
    summarise_at(col_n, sum)
  local <- loglik$local %>%
    list(summ$local, n_obs_gmbn[[col_n]]) %>%
    pmap(function(loglik, summ, n_obs) {
      return(loglik - 0.5 * log(n_obs) * summ["n_param", ])
    })
  global <- local %>%
    unlist() %>%
    sum()
  list(global = global, local = local) %>%
    return()
}
