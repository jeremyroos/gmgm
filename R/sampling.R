#' Sample a Gaussian mixture model
#'
#' This function samples a Gaussian mixture model.
#'
#' @param gmm An object of class \code{gmm}.
#' @param data_x A data frame or numeric matrix containing observations of the
#' explanatory variables if conditional sampling is performed. Its columns must
#' explicitly be named after the explanatory variables. If \code{NULL} (the
#' default), joint sampling is performed.
#' @param n A non-negative integer corresponding to the number of samples. If
#' conditional sampling is performed, this argument is ignored.
#'
#' @return A numeric matrix containing the samples.
#'
#' @seealso \code{\link{density}}, \code{\link{expectation}}
#'
#' @examples
#' set.seed(0)
#' data(gmm_body, data_body)
#' sampl_1 <- sampling(gmm_body, n = 500)
#' sampl_2 <- sampling(gmm_body,
#'                     data_body[, c("WEIGHT", "FAT", "HEIGHT", "AGE")])
#'
#' @export

sampling <- function(gmm, data_x = NULL, n = 1) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  var_gmm <- gmm$mu %>%
    rownames()

  if (is.null(data_x)) {
    if (!is.vector(n, "numeric")) {
      "n is not a numeric value" %>%
        stop()
    }

    if (length(n) != 1) {
      "n is not of length 1" %>%
        stop()
    }

    if (!is.finite(n)) {
      "n is not finite" %>%
        stop()
    }

    if (n < 0) {
      "n is negative" %>%
        stop()
    }

    if (round(n) != n) {
      "n is not an integer" %>%
        stop()
    }

    data_x <- numeric() %>%
      matrix(n, 0)
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

    n <- data_x %>%
      nrow()
  }

  if (n == 0) {
    sampl <- numeric() %>%
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
        matrix(n, n_comp, TRUE)
    } else {
      mu_x <- cond_gmm$mu_x
      sigma_x <- cond_gmm$sigma_x
      id_x <- n_x %>%
        diag()
      log_dens_comp_x <- seq_comp %>%
        map(function(comp) {
          c_data_x <- data_x - matrix(mu_x[, comp], n, n_x, TRUE)
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
    cumsum_alpha_c <- alpha_c %*%
      upper.tri(matrix(nrow = n_comp, ncol = n_comp))
    comp_sampl <- rowSums(cumsum_alpha_c < runif(n))
    is_comp_sampl <- !is.na(comp_sampl)
    sigma_c <- cond_gmm$sigma_c
    sampl <- seq_comp %>%
      map(function(comp) {
        sampl_comp <- comp_sampl == comp & is_comp_sampl
        return(matrix(rnorm(sum(sampl_comp) * n_y), ncol = n_y) %*%
                 chol(sigma_c[[comp]]) +
                 mu_c[[comp]][sampl_comp, , drop = FALSE])
      }) %>%
      do.call("rbind", .) %>%
      .[row_number(comp_sampl), , drop = FALSE]
    rownames(sampl) <- data_x %>%
      rownames()
  }

  sampl %>%
    return()
}
