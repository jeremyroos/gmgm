#' Learn the parameters of a Gaussian mixture graphical model
#'
#' This function learns the parameters of a Gaussian mixture graphical model.
#' Using the local decomposability of the log-likelihood, this task consists in
#' learning each local conditional model independently with the EM algorithm
#' (Koller and Friedman, 2009).
#'
#' @param gmgm An initial object of class \code{gmbn} or \code{gmdbn}.
#' @param data A data frame containing the data used for learning. Its columns
#' must explicitly be named after the nodes of \code{gmgm} and must not contain
#' missing values.
#' @param nodes A character vector containing the nodes whose local conditional
#' models are learned (by default all the nodes of \code{gmgm}). If \code{gmgm}
#' is a \code{gmdbn} object, the same nodes are learned for each of its
#' \code{gmbn} elements. This constraint can be overcome by passing a list of
#' character vectors named after some of these elements (\code{b_1}, \dots) and
#' containing learned nodes specific to them.
#' @param col_seq A character vector containing the column names of \code{data}
#' that describe the observation sequence. If \code{NULL} (the default), all the
#' observations belong to a single sequence. If \code{gmgm} is a temporal
#' \code{gmbn} or \code{gmdbn} object, the observations of a same sequence must
#' be ordered such that the \eqn{t}th one is related to time slice \eqn{t}
#' (note that the sequences can have different lengths). If \code{gmgm} is a
#' non-temporal \code{gmbn} object, this argument is ignored.
#' @param verbose A logical value indicating whether learned nodes in progress
#' are displayed.
#' @param \dots Additional arguments passed to function \code{\link{em}}.
#'
#' @return A list with elements:
#' \item{gmgm}{The final \code{gmbn} or \code{gmdbn} object.}
#' \item{evol_loglik}{A list with elements:}
#' \item{}{\code{global}\verb{  }A numeric vector containing the global
#' log-likelihood before and after learning.}
#' \item{}{\code{local}\verb{  }For a \code{gmbn} object, a numeric matrix
#' containing the local conditional log-likelihoods before and after learning.
#' For a \code{gmdbn} object, a list of numeric matrices containing these values
#' for each \code{gmbn} element.}
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{param_em}}, \code{\link{struct_em}},
#' \code{\link{struct_learn}}
#'
#' @examples
#' \donttest{
#' data(data_body)
#' gmbn_1 <- gmbn(
#'   AGE = split_comp(add_var(NULL, data_body[, "AGE"]), n_sub = 3),
#'   FAT = split_comp(add_var(NULL,
#'                            data_body[, c("FAT", "GENDER", "HEIGHT", "WEIGHT")]),
#'                    n_sub = 2),
#'   GENDER = split_comp(add_var(NULL, data_body[, "GENDER"]), n_sub = 2),
#'   GLYCO = split_comp(add_var(NULL, data_body[, c("GLYCO", "AGE", "WAIST")]),
#'                      n_sub = 2),
#'   HEIGHT = split_comp(add_var(NULL, data_body[, c("HEIGHT", "GENDER")])),
#'   WAIST = split_comp(add_var(NULL,
#'                              data_body[, c("WAIST", "AGE", "FAT", "HEIGHT",
#'                                            "WEIGHT")]),
#'                      n_sub = 3),
#'   WEIGHT = split_comp(add_var(NULL, data_body[, c("WEIGHT", "HEIGHT")]),
#'                       n_sub = 2)
#' )
#' res_learn_1 <- param_learn(gmbn_1, data_body, verbose = TRUE)
#'
#' library(dplyr)
#' data(data_air)
#' data <- data_air %>%
#'   group_by(DATE) %>%
#'   mutate(NO2.1 = lag(NO2), O3.1 = lag(O3), TEMP.1 = lag(TEMP),
#'          WIND.1 = lag(WIND)) %>%
#'   ungroup()
#' gmdbn_1 <- gmdbn(
#'   b_2 = gmbn(
#'     NO2 = split_comp(add_var(NULL, data[, c("NO2", "NO2.1", "WIND")]),
#'                      n_sub = 3),
#'     O3 = split_comp(add_var(NULL,
#'                             data[, c("O3", "NO2", "NO2.1", "O3.1", "TEMP",
#'                                      "TEMP.1")]),
#'                     n_sub = 3),
#'     TEMP = split_comp(add_var(NULL, data[, c("TEMP", "TEMP.1")]), n_sub = 3),
#'     WIND = split_comp(add_var(NULL, data[, c("WIND", "WIND.1")]), n_sub = 3)
#'   ),
#'   b_13 = gmbn(
#'     NO2 = split_comp(add_var(NULL, data[, c("NO2", "NO2.1", "WIND")]),
#'                      n_sub = 3),
#'     O3 = split_comp(add_var(NULL,
#'                             data[, c("O3", "O3.1", "TEMP", "TEMP.1", "WIND")]),
#'                     n_sub = 3),
#'     TEMP = split_comp(add_var(NULL, data[, c("TEMP", "TEMP.1")]), n_sub = 3),
#'     WIND = split_comp(add_var(NULL, data[, c("WIND", "WIND.1")]), n_sub = 3)
#'   )
#' )
#' res_learn_2 <- param_learn(gmdbn_1, data_air, col_seq = "DATE", verbose = TRUE)}
#'
#' @export

param_learn <- function(gmgm, data, nodes = structure(gmgm)$nodes,
                        col_seq = NULL, verbose = FALSE, ...) {
  gmgm %>%
    struct_learn(gmgm = ., data = data, nodes = nodes, arcs_cand = NULL,
                 col_seq = col_seq, score = "loglik", verbose = verbose,
                 add = TRUE, remove = TRUE, min_x = 0, max_x = Inf,
                 max_iter_step = 1, split = TRUE, merge = TRUE, min_comp = 1,
                 max_comp = Inf, space = 0.5, max_rank = 1, max_iter_smem = 1,
                 ...) %>%
    set_names("gmgm", "evol_loglik") %>%
    return()
}
