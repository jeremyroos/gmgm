#' Create a Gaussian mixture dynamic Bayesian network
#'
#' This function creates a Gaussian mixture dynamic Bayesian network as an
#' object of S3 class \code{gmdbn}. Assuming that the system evolves over time
#' (possibly non-stationary) and denoting by \eqn{X^{(t)}} its state at time
#' slice \eqn{t}, a dynamic Bayesian network is a probabilistic graphical model
#' that encodes the joint distribution over any finite time sequence:
#' \deqn{p(X^{(1)}, \dots , X^{(T)}) = p(X^{(1)})
#' \prod_{t = 2}^T p(X^{(t)} | X^{(t - 1)}, \dots , X^{(1)})}
#' It is defined by a sequence of transition models
#' \eqn{\mathcal{B}_1, \mathcal{B}_2, \dots , \mathcal{B}_N} associated with
#' transition time slices \eqn{t_1 = 1 < t_2 < \dots < t_N}, where:
#' \itemize{
#' \item \verb{}\eqn{\mathcal{B}_1} is a Bayesian network that encodes the
#' distribution \eqn{p(X^{(t)})} for \eqn{1 \le t \le t_2 - 1}, assuming that
#' the states at these time slices do not depend on previous states;
#' \item for each \eqn{i \ge 2}, \eqn{\mathcal{B}_i} is a (\eqn{k_i + 1})-slice
#' temporal Bayesian network (where \eqn{k_i < t_i}) that encodes the transition
#' distribution \eqn{p(X^{(t)} | X^{(t - 1)}, \dots , X^{(t - k_i)})} for
#' \eqn{t_i \le t \le t_{i + 1} - 1} (or \eqn{t \ge t_i} if \eqn{i = N}),
#' assuming that the states at these time slices only depend on the \eqn{k_i}
#' previous states (Hourbracq \emph{et al.}, 2017).
#' }
#' In a Gaussian mixture dynamic Bayesian network, these transition models are
#' Gaussian mixture Bayesian networks (Roos \emph{et al.}, 2017).
#'
#' @param \dots Objects of class \code{gmbn} corresponding to the transition
#' models. Each \code{gmbn} object must be named with the prefix \code{b_}
#' followed by its associated transition time slice (e.g. a transition model
#' whose transition time slice is 8 is represented by the \code{gmbn} object
#' \code{b_8}). If the first \code{gmbn} object (chronologically) is associated
#' with a transition time slice \eqn{t \ge 2} (i.e. \code{b_1} is not
#' specified), it is duplicated to create transition models associated with
#' \eqn{1, \dots , t - 1} (removing the arcs whose time lags exceed the maximum
#' temporal depths of these models).
#'
#' @return A list of class \code{gmdbn} containing the \code{gmbn} objects
#' passed as arguments.
#'
#' @references
#' Hourbracq, M., Wuillemin, P.-H., Gonzales, C. and Baumard, P. (2017).
#' Learning and Selection of Dynamic Bayesian Networks for Non-Stationary
#' Processes in Real Time. \emph{In Proceedings of the 30th International
#' Flairs Conference}, pages 742--747, Marco Island, FL, USA.
#'
#' Roos, J., Bonnevay, S. and Gavin, G. (2017). Dynamic Bayesian Networks with
#' Gaussian Mixture Models for Short-Term Passenger Flow Forecasting. \emph{In
#' Proceedings of the 12th International Conference on Intelligent Systems and
#' Knowledge Engineering}, Nanjing, China.
#'
#' @seealso \code{\link{gmbn}}, \code{\link{gmm}}
#'
#' @examples
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
#'
#' @export

gmdbn <- function(...) {
  gmdbn <- list(...)

  if (length(gmdbn) == 0) {
    "no argument is passed" %>%
      stop()
  }

  names_gmbn <- gmdbn %>%
    names()

  if (length(names_gmbn) < length(gmdbn)) {
    "arguments have no names" %>%
      stop()
  }

  if (any(duplicated(names_gmbn))) {
    "arguments have the same name" %>%
      stop()
  }

  if (any(!str_detect(names_gmbn, "^b_[1-9][0-9]*$"))) {
    "arguments have invalid names" %>%
      stop()
  }

  times_gmbn <- names_gmbn %>%
    str_remove("b_") %>%
    as.numeric()
  order_times <- times_gmbn %>%
    order()
  gmdbn <- gmdbn[order_times]
  struct <- gmdbn %>%
    imap(function(gmbn, name_gmbn) {
      if (!inherits(gmbn, "gmbn")) {
        name_gmbn %>%
          str_c(" is not of class \"gmbn\"") %>%
          stop()
      }

      gmbn %>%
        structure() %>%
        return()
    })
  struct_1 <- struct[[1]]
  nodes_1 <- struct_1$nodes
  arcs_1 <- struct_1$arcs
  names_gmbn <- names_gmbn[order_times]
  name_1 <- names_gmbn[1]
  times_gmbn <- times_gmbn[order_times]
  time_1 <- times_gmbn[1]

  if (max(arcs_1$lag, 0) >= time_1) {
    "arcs of " %>%
      str_c(name_1, " have too high lags") %>%
      stop()
  }

  struct[- 1] %>%
    list(names_gmbn[- 1], times_gmbn[- 1]) %>%
    pwalk(function(struct, name_gmbn, time_gmbn) {
      if (!setequal(struct$nodes, nodes_1)) {
        name_1 %>%
          str_c(" and ", name_gmbn, " have different nodes") %>%
          stop()
      }

      if (max(struct$arcs$lag, 0) >= time_gmbn) {
        "arcs of " %>%
          str_c(name_gmbn, " have too high lags") %>%
          stop()
      }
    })

  if (time_1 > 1) {
    gmbn_1 <- gmdbn[[1]]
    times_init <- seq_len(time_1 - 1)
    gmdbn <- times_init %>%
      map(function(time) {
        arcs <- arcs_1 %>%
          filter(lag >= time)
        gmbn_1 %>%
          remove_arcs(arcs) %>%
          return()
      }) %>%
      set_names(str_c("b_", times_init)) %>%
      c(gmdbn)
  }

  class(gmdbn) <- "gmdbn"
  gmdbn %>%
    return()
}
