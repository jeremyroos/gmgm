#' Learn the parameters of a Gaussian mixture graphical model with incomplete
#' data
#'
#' This function learns the parameters of a Gaussian mixture graphical model
#' with incomplete data using the parametric EM algorithm. At each iteration,
#' inference (smoothing inference for a dynamic Bayesian network) is performed
#' to complete the data given the current estimate of the parameters (E step).
#' The completed data are then used to update the parameters (M step), and so
#' on. Each iteration is guaranteed to increase the log-likelihood until
#' convergence to a local maximum (Koller and Friedman, 2009). In practice, due
#' to the sampling process inherent in particle-based inference, it may happen
#' that the monotonic increase no longer occurs when approaching the local
#' maximum, resulting in an earlier termination of the algorithm.
#'
#' @param gmgm An object of class \code{gmbn} (non-temporal) or \code{gmdbn}.
#' @param data A data frame containing the data used for learning. Its columns
#' must explicitly be named after nodes of \code{gmgm} and can contain missing
#' values (columns with no value can be removed).
#' @param nodes A character vector containing the nodes whose local conditional
#' models are learned (by default all the nodes of \code{gmgm}). If \code{gmgm}
#' is a \code{gmdbn} object, the same nodes are learned for each of its
#' \code{gmbn} elements. This constraint can be overcome by passing a list of
#' character vectors named after some of these elements (\code{b_1}, \dots) and
#' containing learned nodes specific to them.
#' @param col_seq A character vector containing the column names of \code{data}
#' that describe the observation sequence. If \code{NULL} (the default), all the
#' observations belong to a single sequence. If \code{gmgm} is a \code{gmdbn}
#' object, the observations of a same sequence must be ordered such that the
#' \eqn{t}th one is related to time slice \eqn{t} (note that the sequences can
#' have different lengths). If \code{gmgm} is a \code{gmbn} object, this
#' argument is ignored.
#' @param n_part A positive integer corresponding to the number of particles
#' generated for each observation (if \code{gmgm} is a \code{gmbn} object) or
#' observation sequence (if \code{gmgm} is a \code{gmdbn} object) during
#' inference.
#' @param max_part_sim An integer greater than or equal to \code{n_part}
#' corresponding to the maximum number of particles that can be processed
#' simultaneously during inference. This argument is used to prevent memory
#' overflow, dividing \code{data} into smaller subsets that are handle
#' sequentially.
#' @param min_ess A numeric value in [0, 1] corresponding to the minimum ESS
#' (expressed as a proportion of \code{n_part}) under which the renewal step of
#' sequential importance resampling is performed. If \code{1} (the default),
#' this step is performed at each time slice. If \code{gmgm} is a \code{gmbn}
#' object, this argument is ignored.
#' @param max_iter_pem A non-negative integer corresponding to the maximum
#' number of iterations.
#' @param verbose A logical value indicating whether iterations in progress
#' are displayed.
#' @param \dots Additional arguments passed to function \code{\link{em}}.
#'
#' @return A list with elements:
#' \item{gmgm}{The final \code{gmbn} or \code{gmdbn} object (with the highest
#' log-likelihood).}
#' \item{data}{A data frame (tibble) containing the complete data used to learn
#' the final \code{gmbn} or \code{gmdbn} object.}
#' \item{seq_loglik}{A numeric matrix containing the sequence of log-likelihoods
#' measured after the E and M steps of each iteration.}
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{param_learn}}, \code{\link{struct_em}},
#' \code{\link{struct_learn}}
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' data(data_body)
#' data_1 <- data_body
#' data_1$GENDER[sample.int(2148, 430)] <- NA
#' data_1$AGE[sample.int(2148, 430)] <- NA
#' data_1$HEIGHT[sample.int(2148, 430)] <- NA
#' data_1$WEIGHT[sample.int(2148, 430)] <- NA
#' data_1$FAT[sample.int(2148, 430)] <- NA
#' data_1$WAIST[sample.int(2148, 430)] <- NA
#' data_1$GLYCO[sample.int(2148, 430)] <- NA
#' gmbn_1 <- gmbn(
#'   AGE = split_comp(add_var(NULL, data_1[, "AGE"]), n_sub = 3),
#'   FAT = split_comp(add_var(NULL,
#'                            data_1[, c("FAT", "GENDER", "HEIGHT", "WEIGHT")]),
#'                    n_sub = 2),
#'   GENDER = split_comp(add_var(NULL, data_1[, "GENDER"]), n_sub = 2),
#'   GLYCO = split_comp(add_var(NULL, data_1[, c("GLYCO", "AGE", "WAIST")]),
#'                      n_sub = 2),
#'   HEIGHT = split_comp(add_var(NULL, data_1[, c("HEIGHT", "GENDER")])),
#'   WAIST = split_comp(add_var(NULL,
#'                              data_1[, c("WAIST", "AGE", "FAT", "HEIGHT",
#'                                         "WEIGHT")]),
#'                      n_sub = 3),
#'   WEIGHT = split_comp(add_var(NULL, data_1[, c("WEIGHT", "HEIGHT")]), n_sub = 2)
#' )
#' res_learn_1 <- param_em(gmbn_1, data_1, verbose = TRUE, regul = 0.01,
#'                         max_iter_em = 100)
#'
#' library(dplyr)
#' set.seed(0)
#' data(data_air)
#' data_2 <- data_air
#' data_2$NO2[sample.int(7680, 1536)] <- NA
#' data_2$O3[sample.int(7680, 1536)] <- NA
#' data_2$TEMP[sample.int(7680, 1536)] <- NA
#' data_2$WIND[sample.int(7680, 1536)] <- NA
#' data_3 <- data_2 %>%
#'   group_by(DATE) %>%
#'   mutate(NO2.1 = lag(NO2), O3.1 = lag(O3), TEMP.1 = lag(TEMP),
#'          WIND.1 = lag(WIND)) %>%
#'   ungroup()
#' gmdbn_1 <- gmdbn(
#'   b_2 = gmbn(
#'     NO2 = split_comp(add_var(NULL, data_3[, c("NO2", "NO2.1", "WIND")]),
#'                      n_sub = 3),
#'     O3 = split_comp(add_var(NULL,
#'                             data_3[, c("O3", "NO2", "NO2.1", "O3.1", "TEMP",
#'                                        "TEMP.1")]),
#'                     n_sub = 3),
#'     TEMP = split_comp(add_var(NULL, data_3[, c("TEMP", "TEMP.1")]), n_sub = 3),
#'     WIND = split_comp(add_var(NULL, data_3[, c("WIND", "WIND.1")]), n_sub = 3)
#'   ),
#'   b_13 = gmbn(
#'     NO2 = split_comp(add_var(NULL, data_3[, c("NO2", "NO2.1", "WIND")]),
#'                      n_sub = 3),
#'     O3 = split_comp(add_var(NULL,
#'                             data_3[, c("O3", "O3.1", "TEMP", "TEMP.1",
#'                                        "WIND")]),
#'                     n_sub = 3),
#'     TEMP = split_comp(add_var(NULL, data_3[, c("TEMP", "TEMP.1")]), n_sub = 3),
#'     WIND = split_comp(add_var(NULL, data_3[, c("WIND", "WIND.1")]), n_sub = 3)
#'   )
#' )
#' res_learn_2 <- param_em(gmdbn_1, data_2, col_seq = "DATE", verbose = TRUE,
#'                         regul = 0.01, max_iter_em = 100)}
#'
#' @export

param_em <- function(gmgm, data, nodes = structure(gmgm)$nodes, col_seq = NULL,
                     n_part = 1000, max_part_sim = 1e06, min_ess = 1,
                     max_iter_pem = Inf, verbose = FALSE, ...) {
  is_gmbn <- gmgm %>%
    inherits("gmbn")

  if (!is_gmbn & !inherits(gmgm, "gmdbn")) {
    "gmgm is not of class \"gmbn\" or \"gmdbn\"" %>%
      stop()
  }

  if (!is.vector(max_iter_pem, "numeric")) {
    "max_iter_pem is not a numeric value" %>%
      stop()
  }

  if (length(max_iter_pem) != 1) {
    "max_iter_pem is not of length 1" %>%
      stop()
  }

  if (is.na(max_iter_pem)) {
    "max_iter_pem is not finite" %>%
      stop()
  }

  if (max_iter_pem < 0) {
    "max_iter_pem is negative" %>%
      stop()
  }

  if (round(max_iter_pem) != max_iter_pem) {
    "max_iter_pem is not an integer" %>%
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

  data_compl <- NULL
  seq_loglik <- numeric() %>%
    matrix(2, 0) %>%
    list()
  loglik <- - Inf
  diff_loglik <- Inf
  iter <- 0

  while (diff_loglik > 0 & iter < max_iter_pem) {
    iter <- iter + 1

    if (verbose) {
      "iter " %>%
        str_c(iter) %>%
        cat()
    }

    gmgm_old <- gmgm
    data_compl_old <- data_compl
    loglik_old <- loglik

    if (is_gmbn) {
      data_compl <- gmgm %>%
        inference(data, n_part = n_part, max_part_sim = max_part_sim)
    } else {
      data_compl <- gmgm %>%
        smoothing(data, col_seq = col_seq, n_part = n_part,
                  max_part_sim = max_part_sim, min_ess = min_ess)
    }

    res_learn <- gmgm %>%
      param_learn(gmgm = ., data = data_compl, nodes = nodes, col_seq = col_seq,
                  verbose = FALSE, ...)
    gmgm <- res_learn$gmgm
    evol_loglik <- res_learn$evol_loglik$global
    seq_loglik <- seq_loglik %>%
      c(list(evol_loglik))
    loglik <- evol_loglik["new"]
    diff_loglik <- loglik - loglik_old

    if (verbose) {
      "    loglik_e = " %>%
        str_c(evol_loglik["old"], "    loglik_m = ", loglik, "\n") %>%
        cat()
    }
  }

  if (diff_loglik < 0) {
    gmgm <- gmgm_old
    data_compl <- data_compl_old
  }

  seq_loglik <- seq_loglik %>%
    do.call("cbind", .)
  rownames(seq_loglik) <- c("E", "M")
  list(gmgm = gmgm, data = data_compl, seq_loglik = seq_loglik) %>%
    return()
}
