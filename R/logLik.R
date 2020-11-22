#' Compute the log-likelihood of a Gaussian mixture model or graphical model
#'
#' This function computes the log-likelihood of a Gaussian mixture model or
#' graphical model.
#'
#' @param object An object of class \code{gmm}, \code{gmbn} or \code{gmdbn}.
#' @param data A data frame containing the data used to compute the
#' log-likelihood. Its columns must explicitly be named after the variables (or
#' nodes) of \code{object}. If \code{object} is a \code{gmm} object, a numeric
#' matrix can be passed.
#' @param y A character vector containing the dependent variables if a
#' conditional log-likelihood is computed. If \code{NULL} (the default), the
#' joint log-likelihood is computed.
#' @param regul A positive numeric value corresponding to the regularization
#' constant if a penalty term is added for Bayesian regularization. If
#' \code{NULL} (the default) no penalty term is added. If a conditional
#' log-likelihood is computed, this argument is ignored.
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
#' corresponding to the log-likelihood.
#'
#' If \code{object} is a \code{gmbn} or \code{gmdbn} object, a list with
#' elements:
#' \item{global}{A numeric value corresponding to the global log-likelihood.}
#' \item{local}{For a \code{gmbn} object, a numeric vector containing the local
#' conditional log-likelihoods. For a \code{gmdbn} object, a list of numeric
#' vectors containing these values for each \code{gmbn} element.}
#'
#' @seealso \code{\link{AIC}}, \code{\link{BIC}}
#'
#' @examples
#' data(gmm_body, data_body)
#' loglik_1 <- logLik(gmm_body, data_body)
#' loglik_2 <- logLik(gmm_body, data_body, y = "WAIST")
#'
#' data(gmbn_body, data_body)
#' loglik_3 <- logLik(gmbn_body, data_body)
#'
#' data(gmdbn_air, data_air)
#' loglik_4 <- logLik(gmdbn_air, data_air, col_seq = "DATE")
#'
#' @name logLik
#' @export

logLik.gmm <- function(object, data, y = NULL, regul = NULL, ...) {
  loglik <- object %>%
    density(data, y = y, log = TRUE) %>%
    sum()

  if (is.null(y) & !is.null(regul)) {
    if (!is.vector(regul, "numeric")) {
      "regul is not a numeric value" %>%
        stop()
    }

    if (length(regul) != 1) {
      "regul is not of length 1" %>%
        stop()
    }

    if (!is.finite(regul)) {
      "regul is not finite" %>%
        stop()
    }

    if (regul <= 0) {
      "regul is not positive" %>%
        stop()
    }

    loglik <- object$sigma %>%
      map_dbl(function(sigma) {
        chol_sigma <- sigma %>%
          chol()
        return(- sum(log(diag(chol_sigma))) -
                 0.5 * regul * sum(diag(chol2inv(chol_sigma))))
      }) %>%
      sum() + loglik
  }

  loglik %>%
    return()
}

#' @rdname logLik
#' @export

logLik.gmbn <- function(object, data, col_seq = NULL, ...) {
  if (!inherits(object, "gmbn")) {
    "object is not of class \"gmbn\"" %>%
      stop()
  }

  if (!is.data.frame(data)) {
    "data is not a data frame" %>%
      stop()
  }

  data <- data %>%
    ungroup()
  col_data <- data %>%
    colnames()

  if (any(duplicated(col_data))) {
    "data has duplicated column names" %>%
      stop()
  }

  struct <- object %>%
    structure()
  nodes <- struct$nodes

  if (any(!(nodes %in% col_data))) {
    "nodes of object are not column names of data" %>%
      stop()
  }

  if (!is.null(col_seq)) {
    if (!is.vector(col_seq, "character")) {
      "col_seq is not a character vector" %>%
        stop()
    }

    col_seq <- col_seq %>%
      unique()

    if (any(!str_detect(col_seq,
                        "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$"))) {
      "col_seq contains invalid column names" %>%
        stop()
    }

    if (any(!(col_seq %in% col_data))) {
      "elements of col_seq are not column names of data" %>%
        stop()
    }

    if (any(!(map_chr(data[col_seq], mode) %in%
              c("numeric", "character", "logical")))) {
      "columns of data[col_seq] have invalid types" %>%
        stop()
    }

    if (any(str_remove(col_seq, "\\.[1-9][0-9]*$") %in% nodes)) {
      "col_seq contains nodes (or instantiations of nodes) of object" %>%
        stop()
    }
  }

  arcs_lag <- struct$arcs %>%
    filter(lag > 0) %>%
    distinct(from, lag) %>%
    mutate(from_lag = str_c(from, ".", lag))
  data <- data %>%
    select_at(c(col_seq, nodes))
  data <- arcs_lag %>%
    group_by(lag) %>%
    group_map(function(arcs, lag) {
      from <- arcs$from
      data %>%
        group_by_at(col_seq) %>%
        mutate_at(from, list(~ lag(., lag$lag))) %>%
        ungroup() %>%
        select_at(from) %>%
        set_names(arcs$from_lag) %>%
        return()
    }) %>%
    bind_cols(data)

  if (nrow(arcs_lag) > 0) {
    data <- data %>%
      group_by_at(col_seq) %>%
      slice(- seq_len(max(arcs_lag$lag))) %>%
      ungroup()
  }

  data <- data %>%
    select_at(c(nodes, arcs_lag$from_lag)) %>%
    as.matrix()
  local <- object %>%
    imap_dbl(function(gmm, node) {
      gmm %>%
        logLik.gmm(data, y = node) %>%
        return()
    })
  global <- local %>%
    sum()
  list(global = global, local = local) %>%
    return()
}

#' @rdname logLik
#' @export

logLik.gmdbn <- function(object, data, col_seq = NULL, ...) {
  if (!inherits(object, "gmdbn")) {
    "object is not of class \"gmdbn\"" %>%
      stop()
  }

  if (!is.data.frame(data)) {
    "data is not a data frame" %>%
      stop()
  }

  data <- data %>%
    ungroup()
  col_data <- data %>%
    colnames()

  if (any(duplicated(col_data))) {
    "data has duplicated column names" %>%
      stop()
  }

  if (!is.null(col_seq)) {
    if (!is.vector(col_seq, "character")) {
      "col_seq is not a character vector" %>%
        stop()
    }

    col_seq <- col_seq %>%
      unique()

    if (any(!str_detect(col_seq,
                        "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$"))) {
      "col_seq contains invalid column names" %>%
        stop()
    }

    if (any(!(col_seq %in% col_data))) {
      "elements of col_seq are not column names of data" %>%
        stop()
    }

    if (any(!(map_chr(data[col_seq], mode) %in%
              c("numeric", "character", "logical")))) {
      "columns of data[col_seq] have invalid types" %>%
        stop()
    }
  }

  arcs <- object %>%
    structure() %>%
    .$arcs
  n_gmbn <- object %>%
    length()
  times_gmbn <- object %>%
    names() %>%
    str_remove("b_") %>%
    as.numeric()
  local <- object %>%
    list(arcs, seq_len(n_gmbn)) %>%
    pmap(function(gmbn, arcs, i_gmbn) {
      time_start <- times_gmbn[i_gmbn] - max(arcs$lag, 0)
      data <- data %>%
        group_by_at(col_seq)

      if (i_gmbn < n_gmbn) {
        data <- data %>%
          slice(time_start:(times_gmbn[i_gmbn + 1] - 1))
      } else if (time_start > 1) {
        data <- data %>%
          slice(- seq_len(time_start - 1))
      }

      data <- data %>%
        ungroup()
      gmbn %>%
        logLik.gmbn(data, col_seq = col_seq) %>%
        .$local %>%
        return()
    })
  global <- local %>%
    unlist() %>%
    sum()
  list(global = global, local = local) %>%
    return()
}
