#' Add variables to a Gaussian mixture model
#'
#' This function adds variables to a Gaussian mixture model.
#'
#' @param gmm An object of class \code{gmm}. If \code{NULL}, a \code{gmm} object
#' is created with the added variables and one mixture component.
#' @param var A character vector containing the added variables, or a data frame
#' or numeric matrix whose columns are named after the added variables. In the
#' first case, for each mixture component, the marginal mean vector of the added
#' variables is 0 and the marginal covariance matrix the identity matrix. In the
#' second case, these mean vector and covariance matrix are computed from the
#' data (after removing the rows that contain missing values).
#'
#' @return The \code{gmm} object after adding the variables.
#'
#' @seealso \code{\link{remove_var}}, \code{\link{rename_var}}
#'
#' @examples
#' data(gmm_body, data_body)
#' gmm_1 <- add_var(gmm_body, "GENDER")
#' gmm_2 <- add_var(gmm_body, data_body[, "GENDER"])
#'
#' @export

add_var <- function(gmm, var) {
  if (is.null(gmm)) {
    gmm <- list(alpha = 1, mu = matrix(numeric(), 0, 1),
                sigma = list(matrix(numeric(), 0, 0)))
  } else if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  mu <- gmm$mu
  var_gmm <- mu %>%
    rownames()

  if (is.null(var) | is.vector(var, "character")) {
    var <- var %>%
      setdiff(var_gmm)
    n_obs <- 0
  } else {
    data <- var
    is_data_frame <- data %>%
      is.data.frame()

    if (!is_data_frame & !is.matrix(data)) {
      "data is not a character vector, data frame or numeric matrix" %>%
        stop()
    }

    var <- data %>%
      colnames()

    if (length(var) < ncol(data)) {
      "the columns of var have no names" %>%
        stop()
    }

    if (any(duplicated(var))) {
      "var has duplicated column names" %>%
        stop()
    }

    var <- var %>%
      setdiff(var_gmm)
    data <- data[, var, drop = FALSE]

    if (is_data_frame) {
      data <- data %>%
        as.matrix()
    }

    data <- data %>%
      na.omit()
    n_obs <- data %>%
      nrow()
  }

  n_var <- var %>%
    length()

  if (n_obs == 0 | n_var == 0) {
    mu_add <- 0 %>%
      rep(n_var)
    sigma_add <- n_var %>%
      diag()
  } else {
    if (!is.numeric(data)) {
      "var is not numeric" %>%
        stop()
    }

    mu_add <- data %>%
      colMeans()

    if (n_obs == 1) {
      sigma_add <- n_var %>%
        diag()
    } else {
      sigma_add <- tcrossprod(t(data) - mu_add) / n_obs

      if (inherits(tryCatch(chol(sigma_add), error = function(e) {return(e)}),
                   "error")) {
        id_var <- n_var %>%
          diag()
        sigma_add <- sigma_add -
          last(eigen(sigma_add, TRUE, TRUE)$values) * id_var
        sigma_add_corr <- sigma_add
        base <- .Machine$double.base
        exp <- .Machine$double.min.exp - 1

        while (inherits(tryCatch(chol(sigma_add_corr),
                                 error = function(e) {return(e)}),
                        "error")) {
          exp <- exp + 1
          sigma_add_corr <- sigma_add + base ^ exp * id_var
        }

        sigma_add <- sigma_add_corr
      }
    }
  }

  alpha <- gmm$alpha
  n_var_gmm <- var_gmm %>%
    length()
  mu <- mu %>%
    rbind(matrix(mu_add, n_var, length(alpha)))
  sigma_add_1 <- 0 %>%
    matrix(n_var_gmm, n_var)
  sigma_add_2 <- sigma_add_1 %>%
    t() %>%
    cbind(sigma_add)
  sigma <- gmm$sigma %>%
    map(function(sigma) {
      sigma %>%
        cbind(sigma_add_1) %>%
        rbind(sigma_add_2) %>%
        return()
    })
  alpha %>%
    gmm(mu, sigma, var = c(var_gmm, var)) %>%
    return()
}
