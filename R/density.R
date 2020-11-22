#' Compute densities of a Gaussian mixture model
#'
#' This function computes densities of a Gaussian mixture model.
#'
#' @param gmm An object of class \code{gmm}.
#' @param data A data frame or numeric matrix containing the observations whose
#' densities are computed. Its columns must explicitly be named after the
#' variables of \code{gmm}.
#' @param y A character vector containing the dependent variables if conditional
#' densities are computed. If \code{NULL} (the default), joint densities are
#' computed.
#' @param log A logical value indicating whether the densities are returned as
#' log-densities.
#'
#' @return A numeric vector containing the (log-)densities.
#'
#' @seealso \code{\link{expectation}}, \code{\link{sampling}}
#'
#' @examples
#' data(gmm_body, data_body)
#' dens_1 <- density(gmm_body, data_body, log = TRUE)
#' dens_2 <- density(gmm_body, data_body, y = "WAIST", log = TRUE)
#'
#' @export

density <- function(gmm, data, y = NULL, log = FALSE) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  is_data_frame <- data %>%
    is.data.frame()

  if (!is_data_frame & !is.matrix(data)) {
    "data is not a data frame or numeric matrix" %>%
      stop()
  }

  col_data <- data %>%
    colnames()

  if (any(duplicated(col_data))) {
    "data has duplicated column names" %>%
      stop()
  }

  mu <- gmm$mu
  var_gmm <- mu %>%
    rownames()

  if (any(!(var_gmm %in% col_data))) {
    "variables of gmm are not column names of data" %>%
      stop()
  }

  data <- data[, var_gmm, drop = FALSE]

  if (is_data_frame) {
    data <- data %>%
      as.matrix()
  }

  n_obs <- data %>%
    nrow()

  if (!is.numeric(data)) {
    if (n_obs == 0) {
      mode(data) <- "numeric"
    } else {
      "data is not numeric" %>%
        stop()
    }
  }

  is_y <- !is.null(y)

  if (is_y) {
    if (!is.vector(y, "character")) {
      "y is not a character vector" %>%
        stop()
    }

    y <- y %>%
      unique()
    n_y <- y %>%
      length()

    if (n_y == 0) {
      "y is empty" %>%
        stop()
    }

    if (any(!(y %in% var_gmm))) {
      "elements of y are not variables of gmm" %>%
        stop()
    }
  }

  if (!is.vector(log, "logical")) {
    "log is not a logical value" %>%
      stop()
  }

  if (length(log) != 1) {
    "log is not of length 1" %>%
      stop()
  }

  if (is.na(log)) {
    "log is NA" %>%
      stop()
  }

  if (n_obs == 0) {
    log_dens <- numeric()
  } else {
    n_var_gmm <- var_gmm %>%
      length()
    id <- n_var_gmm %>%
      diag()
    alpha <- gmm$alpha
    sigma <- gmm$sigma
    log_dens_comp <- alpha %>%
      seq_along() %>%
      map(function(comp) {
        c_data <- data - matrix(mu[, comp], n_obs, n_var_gmm, TRUE)
        chol_sigma <- sigma[[comp]] %>%
          chol()
        return(log(alpha[comp]) - sum(log(diag(chol_sigma))) -
                 0.5 *
                 (n_var_gmm * log(2 * pi) +
                    rowSums((c_data %*% backsolve(chol_sigma, id)) ^ 2)))
      }) %>%
      do.call("cbind", .)
    max_log_dens_comp <- log_dens_comp %>%
      apply(1, max)
    s_log_dens <- exp(log_dens_comp - max_log_dens_comp) %>%
      rowSums() %>%
      log()
    s_log_dens[max_log_dens_comp == - Inf] <- 0
    log_dens <- s_log_dens + max_log_dens_comp

    if (is_y) {
      if (n_y < n_var_gmm) {
        log_dens <- log_dens -
          density(remove_var(gmm, y), data[, setdiff(var_gmm, y), drop = FALSE],
                  log = TRUE)
      }
    }
  }

  if (log) {
    log_dens %>%
      return()
  } else {
    log_dens %>%
      exp() %>%
      return()
  }
}
