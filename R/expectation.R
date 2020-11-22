#' Compute expectations of a Gaussian mixture model
#'
#' This function computes expectations of a Gaussian mixture model.
#'
#' @param gmm An object of class \code{gmm}.
#' @param data_x A data frame or numeric matrix containing observations of the
#' explanatory variables if conditional expectations are computed. Its columns
#' must explicitly be named after the explanatory variables. If \code{NULL} (the
#' default), the joint expectation is computed in a one-row matrix.
#'
#' @return A numeric matrix containing the expectations.
#'
#' @seealso \code{\link{density}}, \code{\link{sampling}}
#'
#' @examples
#' data(gmm_body, data_body)
#' expect_1 <- expectation(gmm_body)
#' expect_2 <- expectation(gmm_body,
#'                         data_body[, c("WEIGHT", "FAT", "HEIGHT", "AGE")])
#'
#' @export

expectation <- function(gmm, data_x = NULL) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  var_gmm <- gmm$mu %>%
    rownames()

  if (is.null(data_x)) {
    data_x <- numeric() %>%
      matrix(1, 0)
    x <- NULL
    y <- var_gmm
    n_y <- y %>%
      length()
  } else {
    is_data_frame <- data_x %>%
      is.data.frame()

    if (!is_data_frame & !is.matrix(data_x)) {
      "data_x is not a data frame or numeric matrix" %>%
        stop()
    }

    col_data_x <- data_x %>%
      colnames()

    if (length(col_data_x) < ncol(data_x)) {
      "the columns of data_x have no names" %>%
        stop()
    }

    if (any(duplicated(col_data_x))) {
      "data_x has duplicated column names" %>%
        stop()
    }

    y <- var_gmm %>%
      setdiff(col_data_x)
    n_y <- y %>%
      length()

    if (n_y == 0) {
      "the column names of data_x contain all the variables of gmm" %>%
        stop()
    }

    x <- var_gmm %>%
      setdiff(y)

    if (!is.null(col_data_x)) {
      data_x <- data_x[, x, drop = FALSE]
    }

    if (is_data_frame) {
      data_x <- data_x %>%
        as.matrix()
    }

    if (!is.numeric(data_x)) {
      if (length(data_x) == 0) {
        mode(data_x) <- "numeric"
      } else {
        "data_x is not numeric" %>%
          stop()
      }
    }
  }

  n_obs <- data_x %>%
    nrow()

  if (n_obs == 0) {
    expect <- numeric() %>%
      matrix(0, n_y, dimnames = list(NULL, y))
  } else {
    cond_gmm <- gmm %>%
      conditional(y)
    n_x <- x %>%
      length()
    alpha <- gmm$alpha
    n_comp <- alpha %>%
      length()
    seq_comp <- n_comp %>%
      seq_len()

    if (n_x == 0) {
      alpha_c <- alpha %>%
        matrix(n_obs, n_comp, TRUE)
    } else {
      mu_x <- cond_gmm$mu_x
      sigma_x <- cond_gmm$sigma_x
      id_x <- n_x %>%
        diag()
      log_dens_comp_x <- seq_comp %>%
        map(function(comp) {
          c_data_x <- data_x - matrix(mu_x[, comp], n_obs, n_x, TRUE)
          chol_sigma_x <- sigma_x[[comp]] %>%
            chol()
          return(log(alpha[comp]) - sum(log(diag(chol_sigma_x))) -
                   0.5 *
                   (n_x * log(2 * pi) +
                      rowSums((c_data_x %*% backsolve(chol_sigma_x, id_x))
                              ^ 2)))
        }) %>%
        do.call("cbind", .)
      max_log_dens_comp_x <- log_dens_comp_x %>%
        apply(1, max)
      s_log_dens_comp_x <- log_dens_comp_x - max_log_dens_comp_x
      s_log_dens_comp_x[max_log_dens_comp_x == - Inf, ] <- 0
      s_log_dens_x <- s_log_dens_comp_x %>%
        exp() %>%
        rowSums() %>%
        log()
      alpha_c <- exp(s_log_dens_comp_x - s_log_dens_x)
    }

    data_x_1 <- 1 %>%
      cbind(data_x, deparse.level = 0)
    mu_c <- cond_gmm$coeff %>%
      map(function(coeff) {
        return(data_x_1 %*% coeff)
      })
    expect <- seq_comp %>%
      map(function(comp) {
        return(alpha_c[, comp] * mu_c[[comp]])
      }) %>%
      reduce(`+`)
    rownames(expect) <- data_x %>%
      rownames()
  }

  expect %>%
    return()
}
