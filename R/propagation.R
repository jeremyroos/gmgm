#' Propagate particles forward in time
#'
#' This function propagates particles forward in time. Assuming that the
#' particles have been propagated to a given time slice \eqn{t}, the aim is to
#' propagate them to a later time slice \eqn{t + k} according to the Gaussian
#' mixture graphical model and to the evidence collected over time. At first, a
#' renewal step is performed if the effective sample size (ESS) is below a given
#' threshold (Doucet and Johansen, 2009). This step consists in randomly
#' selecting new particles among the old ones proportionately to their current
#' weights. Upon receiving the data (the evidence) of \eqn{t + 1}, each particle
#' is used to generate samples for the unknown values. Its weight is then
#' updated to the likelihood for the observed values. The higher this
#' likelihood, the more likely the particle is selected at the next renewal step
#' for propagation to \eqn{t + 2}, and so on (Koller and Friedman, 2009).
#'
#' @param part A data frame containing the particles propagated to time slice
#' \eqn{t}, as obtained from function \code{\link{particles}} or
#' \code{\link{propagation}}.
#' @param gmgm An object of class \code{gmbn} or \code{gmdbn}. For a
#' \code{gmdbn} object, the \code{gmbn} elements used for propagation are
#' selected according to the temporal depth of the particles, assuming that the
#' particles contain all the samples since the first time slice (this depth is
#' thus considered as the current time slice).
#' @param evid A data frame containing the evidence of time slices
#' \eqn{t + 1, \dots , t + k}. Its columns must explicitly be named after nodes
#' of \code{gmgm} and can contain missing values (columns with no value can be
#' removed). If \code{NULL} (the default), no evidence is taken into account.
#' @param col_seq A character vector containing the column names of \code{part}
#' and \code{evid} that describe the observation sequence. If \code{NULL} (the
#' default), all the particles and observations belong to a single sequence. In
#' \code{evid}, the observations of a same sequence must be ordered such that
#' the \eqn{k}th one is related to time slice \eqn{t + k} (note that the
#' sequences can have different lengths).
#' @param col_weight A character string corresponding to the column name of
#' \code{part} that describes the particle weight.
#' @param n_times A non-negative integer corresponding to the number of time
#' slices \eqn{k} over which the particles are propagated.
#' @param min_ess A numeric value in [0, 1] corresponding to the minimum ESS
#' (expressed as a proportion of the number of particles) under which the
#' renewal step is performed. If \code{1} (the default), this step is performed
#' at each time slice.
#'
#' @return A data frame (tibble) containing the particles supplemented with the
#' samples of time slices \eqn{t + 1, \dots , t + k}.
#'
#' @references
#' Doucet, A. and Johansen, A. M. (2009). A Tutorial on Particle Filtering and
#' Smoothing: Fifteen years later. \emph{Handbook of nonlinear filtering},
#' 12:656--704.
#'
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{aggregation}}, \code{\link{particles}}
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' set.seed(0)
#' data(gmdbn_air, data_air)
#' evid <- data_air %>%
#'   group_by(DATE) %>%
#'   slice(1:3) %>%
#'   ungroup()
#' evid$NO2[sample.int(150, 30)] <- NA
#' evid$O3[sample.int(150, 30)] <- NA
#' evid$TEMP[sample.int(150, 30)] <- NA
#' evid$WIND[sample.int(150, 30)] <- NA
#' part <- particles(data.frame(DATE = unique(evid$DATE))) %>%
#'   propagation(gmdbn_air, evid, col_seq = "DATE", n_times = 3)}
#'
#' @export

propagation <- function(part, gmgm, evid = NULL, col_seq = NULL,
                        col_weight = "weight", n_times = 1, min_ess = 1) {
  if (!is.data.frame(part)) {
    "part is not a data frame" %>%
      stop()
  }

  part <- part %>%
    as_tibble()
  n_part <- part %>%
    nrow()

  if (n_part == 0) {
    "part has no row" %>%
      stop()
  }

  col_part <- part %>%
    colnames()

  if (any(duplicated(col_part))) {
    "part has duplicated column names" %>%
      stop()
  }

  struct <- gmgm %>%
    structure()
  nodes <- struct$nodes

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

    if (any(str_remove(col_seq, "\\.[1-9][0-9]*$") %in% nodes)) {
      "col_seq contains nodes (or instantiations of nodes) of gmgm" %>%
        stop()
    }

    if (any(!(col_seq %in% col_part))) {
      "elements of col_seq are not column names of part" %>%
        stop()
    }
  }

  seq <- part %>%
    select(all_of(col_seq))

  if (any(!(map_chr(seq, mode) %in% c("numeric", "character", "logical")))) {
    "columns of part[col_seq] have invalid types" %>%
      stop()
  }

  if (!is.vector(col_weight, "character")) {
    "col_weight is not a character string" %>%
      stop()
  }

  if (length(col_weight) != 1) {
    "col_weight is not of length 1" %>%
      stop()
  }

  if (!str_detect(col_weight,
                  "^(\\.([A-Za-z_\\.]|$)|[A-Za-z])[A-Za-z0-9_\\.]*$")) {
    "col_weight is an invalid column name" %>%
      stop()
  }

  if (!(col_weight %in% col_part)) {
    "col_weight is not a column name of part" %>%
      stop()
  }

  if (!is.numeric(part[[col_weight]])) {
    "part[[col_weight]] is not numeric" %>%
      stop()
  }

  if (str_remove(col_weight, "\\.[1-9][0-9]*$") %in% nodes) {
    "col_weight is a node (or an instantiation of a node) of gmgm" %>%
      stop()
  }

  if (col_weight %in% col_seq) {
    "col_weight is in col_seq" %>%
      stop()
  }

  if (!is.vector(n_times, "numeric")) {
    "n_times is not a numeric value" %>%
      stop()
  }

  if (length(n_times) != 1) {
    "n_times is not of length 1" %>%
      stop()
  }

  if (!is.finite(n_times)) {
    "n_times is not finite" %>%
      stop()
  }

  if (n_times < 0) {
    "n_times is negative" %>%
      stop()
  }

  if (round(n_times) != n_times) {
    "n_times is not an integer" %>%
      stop()
  }

  if (n_times > 0) {
    n_nodes <- nodes %>%
      length()
    nodes_0 <- numeric() %>%
      matrix(0, n_nodes, dimnames = list(NULL, nodes)) %>%
      as_tibble()

    if (is.null(evid)) {
      evid <- seq %>%
        slice(0) %>%
        bind_cols(nodes_0)
    } else {
      if (!is.data.frame(evid)) {
        "evid is not a data frame" %>%
          stop()
      }

      evid <- evid %>%
        ungroup()
      col_evid <- evid %>%
        colnames()

      if (any(duplicated(col_evid))) {
        "evid has duplicated column names" %>%
          stop()
      }

      if (any(!(col_seq %in% col_evid))) {
        "elements of col_seq are not column names of evid" %>%
          stop()
      }

      seq_evid <- evid %>%
        select(all_of(col_seq))

      if (any(!map2_lgl(seq, seq_evid,
                        function(seq, seq_evid) {
                          return(identical(class(seq), class(seq_evid)) |
                                 (is.numeric(seq) & is.numeric(seq_evid)) |
                                 ((is.character(seq) | is.factor(seq)) &
                                  (is.character(seq_evid) |
                                   is.factor(seq_evid))))
                        }))) {
        "columns of part[col_seq] and evid[col_seq] have incompatible types" %>%
          stop()
      }

      nodes_evid <- evid %>%
        select(any_of(nodes))

      if (any(map_chr(nodes_evid, mode) != "numeric")) {
        "columns of evid related to nodes are not numeric" %>%
          stop()
      }

      evid <- seq_evid %>%
        bind_cols(nodes_evid) %>%
        bind_rows(nodes_0)
    }

    if (!is.vector(min_ess, "numeric")) {
      "min_ess is not a numeric value" %>%
        stop()
    }

    if (length(min_ess) != 1) {
      "min_ess is not of length 1" %>%
        stop()
    }

    if (is.na(min_ess)) {
      "min_ess is NA" %>%
        stop()
    }

    if (min_ess < 0 || min_ess > 1) {
      "min_ess is not in [0, 1]" %>%
        stop()
    }

    col_part_split <- col_part %>%
      str_split_fixed("\\.(?=[1-9][0-9]*$)", 2)
    col_part_from <- col_part_split[, 1]
    col_in_nodes <- col_part_from %in% nodes
    col_part_lag <- col_part_split[, 2][col_in_nodes] %>%
      as.numeric() %>%
      replace_na(0) + 1
    arcs <- struct$arcs
    is_gmbn <- gmgm %>%
      inherits("gmbn")

    if (is_gmbn) {
      gmbn <- gmgm
      arcs_gmbn <- arcs %>%
        mutate(from_lag = str_c(from, ".", lag))
      from_lag <- arcs_gmbn %>%
        filter(lag > 0) %>%
        .$from_lag
      arcs_0 <- arcs_gmbn %>%
        filter(lag == 0) %>%
        select(from, to)
    } else {
      time <- col_part_lag %>%
        max(0)
      times_gmbn <- gmgm %>%
        names() %>%
        str_remove("b_") %>%
        as.numeric() %>%
        c(Inf)
      time_gmbn <- time + 1
      i_gmbn <- time_gmbn %>%
        findInterval(times_gmbn)
    }

    eps <- .Machine$double.eps

    for (time_prop in seq_len(n_times)) {
      if (!is_gmbn) {
        time <- time + 1

        if (time == time_gmbn) {
          gmbn <- gmgm[[i_gmbn]]
          arcs_gmbn <- arcs[[i_gmbn]] %>%
            mutate(from_lag = str_c(from, ".", lag))
          from_lag <- arcs_gmbn %>%
            filter(lag > 0) %>%
            .$from_lag
          arcs_0 <- arcs_gmbn %>%
            filter(lag == 0) %>%
            select(from, to)
          i_gmbn <- i_gmbn + 1
          time_gmbn <- times_gmbn[i_gmbn]
        }
      }

      if (time_prop > 1) {
        col_part <- col_part %>%
          c(nodes)
        col_part_from <- col_part_from %>%
          c(nodes)
        col_in_nodes <- col_in_nodes %>%
          c(rep(TRUE, n_nodes))
        col_part_lag <- c(col_part_lag + 1, rep(1, n_nodes))
      }

      col_part[col_in_nodes] <- col_part_from[col_in_nodes] %>%
        str_c(".", col_part_lag)

      if (any(!(from_lag %in% col_part))) {
        "columns of part related to nodes are missing" %>%
          stop()
      }

      part <- part %>%
        set_names(col_part) %>%
        group_by(across(col_seq)) %>%
        group_map(function(part, seq) {
          if (sum(part[[col_weight]] ^ 2) - 1 / (min_ess * nrow(part)) > eps) {
            part <- part %>%
              sample_frac(replace = TRUE, weight = !!sym(col_weight)) %>%
              mutate(across(col_weight, ~ 1))
          }

          part %>%
            return()
        }, .keep = TRUE) %>%
        bind_rows() %>%
        mutate(across(col_weight, log))
      part_prop <- part %>%
        select(all_of(c(col_seq, from_lag)))
      part_miss <- part_prop %>%
        group_by(across(col_seq)) %>%
        mutate(across(from_lag, ~ any(. != .[1]))) %>%
        ungroup() %>%
        select(all_of(from_lag)) %>%
        as.matrix()
      part_prop <- part_prop %>%
        select(all_of(from_lag)) %>%
        as.matrix()
      evid_time <- evid %>%
        group_by(across(col_seq)) %>%
        slice(time_prop) %>%
        ungroup()

      if (length(col_seq) == 0) {
        if (nrow(evid_time) == 0) {
          evid_time <- evid_time %>%
            add_row()
        }

        evid_prop <- evid_time %>%
          uncount(n_part)
      } else {
        evid_prop <- part %>%
          left_join(evid_time, by = col_seq) %>%
          select(all_of(nodes))
      }

      evid_prop <- evid_prop %>%
        as.matrix()
      part_miss <- part_miss %>%
        cbind(is.na(evid_prop))
      nodes_miss <- part_miss %>%
        colnames() %>%
        .[colSums(part_miss) > 0]
      nodes_pend <- arcs_gmbn %>%
        filter(from_lag %in% nodes_miss) %>%
        .$to %>%
        union(intersect(nodes_miss, nodes))
      part_prop <- part_prop %>%
        cbind(evid_prop[, setdiff(nodes, nodes_pend), drop = FALSE])
      arcs_pend <- arcs_0 %>%
        filter(to %in% nodes_pend)
      log_weights <- 0 %>%
        rep(n_part)

      while (length(nodes_pend) > 0) {
        arcs_pend <- arcs_pend %>%
          filter(from %in% nodes_pend)
        to_pend <- arcs_pend$to %>%
          unique()
        nodes_prop <- nodes_pend %>%
          setdiff(to_pend)
        nodes_pend <- to_pend
        res_prop <- gmbn[nodes_prop] %>%
          imap(function(gmm, node) {
            evid_prop <- evid_prop[, node, drop = FALSE]
            miss <- part_miss[, node]
            evid_prop[miss, ] <- gmm %>%
              sampling(part_prop[miss, , drop = FALSE])
            par_miss <- part_miss[, rownames(gmm$mu)[- 1], drop = FALSE] %>%
              rowSums() > 0
            obs <- !miss & par_miss
            part_evid_obs <- part_prop[obs, , drop = FALSE] %>%
              cbind(evid_prop[obs, , drop = FALSE])
            log_weights[obs] <- gmm %>%
              density(part_evid_obs, y = node, log = TRUE)
            list(evid_prop = evid_prop, log_weights = log_weights) %>%
              return()
          }) %>%
          transpose()
        part_prop <- part_prop %>%
          cbind(do.call("cbind", res_prop$evid_prop))
        part <- part %>%
          mutate(across(col_weight, ~ . + reduce(res_prop$log_weights, `+`)))
      }

      part <- part %>%
        group_by(across(col_seq)) %>%
        mutate(across(col_weight, ~ . - max(.))) %>%
        mutate(across(col_weight, exp)) %>%
        mutate(across(col_weight, ~ . / sum(.))) %>%
        ungroup() %>%
        bind_cols(as_tibble(part_prop[, nodes, drop = FALSE]))
    }
  }

  part %>%
    return()
}
