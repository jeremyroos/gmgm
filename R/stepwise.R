#' Select the explanatory variables, the number of mixture components and
#' estimate the parameters of a conditional Gaussian mixture model
#'
#' This function selects the explanatory variables, the number of mixture
#' components and estimates the parameters a conditional Gaussian mixture model
#' using a stepwise algorithm. At the first iteration, the SMEM algorithm is
#' performed to update the number of components and the parameters of the
#' initial model. Then each following iteration consists in adding or removing a
#' candidate explanatory variable, before re-estimating the model with the SMEM
#' algorithm. The selected add or remove operation is the one that maximizes a
#' conditional scoring function (after the re-estimation process). The stepwise
#' algorithm stops if none of the candidate operations improves the score.
#'
#' @param gmm An initial object of class \code{gmm}.
#' @param data A data frame or numeric matrix containing the data used in the
#' stepwise algorithm. Its columns must explicitly be named after the variables
#' of \code{gmm} and the candidate explanatory variables, and must not contain
#' missing values.
#' @param y A character vector containing the dependent variables (by default
#' the first variable of \code{gmm}).
#' @param x_cand A character vector containing the candidate explanatory
#' variables for addition or removal (by default all the column names of
#' \code{data} except \code{y}). If variables already in \code{gmm} are not
#' candidates, they cannot be removed.
#' @param score A character string (\code{"aic"}, \code{"bic"} or
#' \code{"loglik"}) corresponding to the scoring function.
#' @param add A logical value indicating whether add operations are allowed (if
#' \code{FALSE}, no variable can be added).
#' @param remove A logical value indicating whether remove operations are
#' allowed (if \code{FALSE}, no variable can be removed).
#' @param min_x A non-negative integer corresponding to the minimum number of
#' explanatory variables.
#' @param max_x A non-negative integer corresponding to the maximum number of
#' explanatory variables.
#' @param max_iter_step A non-negative integer corresponding to the maximum
#' number of iterations.
#' @param verbose A logical value indicating whether iterations in progress
#' are displayed.
#' @param \dots Additional arguments passed to function \code{\link{smem}}.
#'
#' @return A list with elements:
#' \item{gmm}{The final \code{gmm} object.}
#' \item{posterior}{A numeric matrix containing the posterior probabilities for
#' each observation.}
#' \item{seq_score}{A numeric vector containing the sequence of scores measured
#' initially and after each iteration.}
#' \item{seq_oper}{A character vector containing the sequence of add and remove
#' operations performed at each iteration.}
#'
#' @seealso \code{\link{em}}, \code{\link{smem}}
#'
#' @examples
#' \donttest{
#' data(data_body)
#' gmm_1 <- add_var(NULL, "WAIST")
#' res_step <- stepwise(gmm_1, data_body, verbose = TRUE, max_comp = 3,
#'                      max_rank = 1, regul = 0.01, max_iter_em = 100)}
#'
#' @export

stepwise <- function(gmm, data, y = rownames(gmm$mu)[1],
                     x_cand = setdiff(colnames(data), y), score = "bic",
                     add = TRUE, remove = TRUE, min_x = 0, max_x = Inf,
                     max_iter_step = Inf, verbose = FALSE, ...) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  if (!is.vector(y, "character")) {
    "y is not a character vector" %>%
      stop()
  }

  if (!is.vector(score, "character")) {
    "score is not a character string" %>%
      stop()
  }

  if (length(score) != 1) {
    "score is not of length 1" %>%
      stop()
  }

  score <- score %>%
    str_to_lower()

  if (score == "aic") {
    seq_score <- gmm %>%
      AIC.gmm(data, y = y)
  } else if (score == "bic") {
    seq_score <- gmm %>%
      BIC.gmm(data, y = y)
  } else if (score == "loglik") {
    seq_score <- gmm %>%
      logLik.gmm(data, y = y)
  } else {
    "score is not \"aic\", \"bic\" or \"loglik\"" %>%
      stop()
  }

  if (!is.vector(min_x, "numeric")) {
    "min_x is not a numeric value" %>%
      stop()
  }

  if (length(min_x) != 1) {
    "min_x is not of length 1" %>%
      stop()
  }

  if (!is.finite(min_x)) {
    "min_x is not finite" %>%
      stop()
  }

  if (round(min_x) != min_x) {
    "min_x is not an integer" %>%
      stop()
  }

  if (!is.vector(max_x, "numeric")) {
    "max_x is not a numeric value" %>%
      stop()
  }

  if (length(max_x) != 1) {
    "max_x is not of length 1" %>%
      stop()
  }

  if (is.na(max_x)) {
    "max_x is NA" %>%
      stop()
  }

  if (round(max_x) != max_x) {
    "max_x is not an integer" %>%
      stop()
  }

  if (!between(min_x, 0, max_x)) {
    "min_x is not in [0, max_x]" %>%
      stop()
  }

  mu <- gmm$mu
  y <- y %>%
    unique()
  n_x <- mu %>%
    nrow() - length(y)

  if (!between(n_x, min_x, max_x)) {
    "the number of explanatory variables of gmm is not in [min_x, max_x]" %>%
      stop()
  }

  if (!is.vector(max_iter_step, "numeric")) {
    "max_iter_step is not a numeric value" %>%
      stop()
  }

  if (length(max_iter_step) != 1) {
    "max_iter_step is not of length 1" %>%
      stop()
  }

  if (is.na(max_iter_step)) {
    "max_iter_step is NA" %>%
      stop()
  }

  if (max_iter_step < 0) {
    "max_iter_step is negative" %>%
      stop()
  }

  if (round(max_iter_step) != max_iter_step) {
    "max_iter_step is not an integer" %>%
      stop()
  }

  if (!is.vector(verbose, "logical")) {
    "verbose is not a logical value" %>%
      stop()
  }

  if (length(verbose) != 1) {
    "verbose is not of length 1" %>%
      stop()
  }

  if (is.na(verbose)) {
    "verbose is NA" %>%
      stop()
  }

  if (verbose) {
    "iter 0    " %>%
      str_c(score, " = ", seq_score, "\n") %>%
      cat()
  }

  if (max_iter_step == 0) {
    posterior <- gmm %>%
      em(data, max_iter_em = 0) %>%
      .$posterior
    seq_oper <- character()
  } else {
    if (!is.null(x_cand)) {
      if (!is.vector(x_cand, "character")) {
        "x_cand is not a character vector" %>%
          stop()
      }

      if (any(!(x_cand %in% colnames(data)))) {
        "elements of x_cand are not column names of data" %>%
          stop()
      }

      if (any(x_cand %in% y)) {
        "x_cand contains elements of y" %>%
          stop()
      }
    }

    if (!is.vector(add, "logical")) {
      "add is not a logical value" %>%
        stop()
    }

    if (length(add) != 1) {
      "add is not of length 1" %>%
        stop()
    }

    if (is.na(add)) {
      "add is NA" %>%
        stop()
    }

    if (!is.vector(remove, "logical")) {
      "remove is not a logical value" %>%
        stop()
    }

    if (length(remove) != 1) {
      "remove is not of length 1" %>%
        stop()
    }

    if (is.na(remove)) {
      "remove is NA" %>%
        stop()
    }

    if (verbose) {
      "iter 1" %>%
        cat()
    }

    res_smem <- gmm %>%
      smem(gmm = ., data = data, y = y, score = score, verbose = FALSE, ...)
    gmm <- res_smem$gmm
    posterior <- res_smem$posterior
    v_score <- res_smem$seq_score %>%
      last()
    seq_score <- seq_score %>%
      c(v_score)
    oper <- "SMEM"
    seq_oper <- "SMEM"
    var_gmm <- mu %>%
      rownames()
    iter <- 1

    while (oper != "none" & iter < max_iter_step) {
      iter <- iter + 1

      if (verbose) {
        "    " %>%
          str_c(score, " = ", v_score, "    oper = ", oper, "\niter ", iter) %>%
          cat()
      }

      gmm_old <- gmm
      var_gmm_old <- var_gmm
      n_x_old <- n_x
      oper <- "none"

      if (add & n_x_old < max_x) {
        for (var in setdiff(x_cand, var_gmm_old)) {
          res_add <- gmm_old %>%
            add_var(data[, var, drop = FALSE]) %>%
            smem(gmm = ., data = data, y = y, score = score, verbose = FALSE,
                 ...)
          v_score_add <- res_add$seq_score %>%
            last()

          if (v_score_add > v_score) {
            gmm <- res_add$gmm
            var_gmm <- var_gmm_old %>%
              c(var)
            n_x <- n_x_old + 1
            posterior <- res_add$posterior
            v_score <- v_score_add
            oper <- "add " %>%
              str_c(var, " + SMEM")
          }
        }
      }

      if (remove & n_x_old > min_x) {
        for (var in intersect(x_cand, var_gmm_old)) {
          res_remove <- gmm_old %>%
            remove_var(var) %>%
            smem(gmm = ., data = data, y = y, score = score, verbose = FALSE,
                 ...)
          v_score_remove <- res_remove$seq_score %>%
            last()

          if (v_score_remove >= v_score) {
            gmm <- res_remove$gmm
            var_gmm <- var_gmm_old %>%
              setdiff(var)
            n_x <- n_x_old - 1
            posterior <- res_remove$posterior
            v_score <- v_score_remove
            oper <- "remove " %>%
              str_c(var, " + SMEM")
          }
        }
      }

      seq_score <- seq_score %>%
        c(v_score)
      seq_oper <- seq_oper %>%
        c(oper)
    }

    if (verbose) {
      "    " %>%
        str_c(score, " = ", v_score, "    oper = ", oper, "\n") %>%
        cat()
    }
  }

  list(gmm = gmm, posterior = posterior, seq_score = seq_score,
       seq_oper = seq_oper) %>%
    return()
}
