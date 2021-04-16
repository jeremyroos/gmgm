#' Perform predictive inference in a Gaussian mixture dynamic Bayesian network
#'
#' This function performs predictive inference in a Gaussian mixture dynamic
#' Bayesian network. For a sequence of \eqn{T} time slices, this task consists
#' in defining a time horizon \eqn{h} such that at each time slice \eqn{t}
#' (for \ifelse{html}{\out{0 &le; <i>t</i> &le; <i>t</i> &minus;
#' <i>h</i>}}{\eqn{0 \le t \le T - h}}), the state of the system at \eqn{t + h}
#' is estimated given all the data (the evidence) collected up to \eqn{t}.
#' Although the states at \ifelse{html}{\out{<i>t</i> + 1, &hellip; ,
#' <i>t</i> + <i>h</i>}}{\eqn{t + 1, \dots , t + h}} are observed in the future,
#' some information about them can be known a priori (such as contextual
#' information or features controlled by the user). This "predicted" evidence
#' can be taken into account when propagating the particles from \eqn{t} to
#' \eqn{t + h} in order to improve the predictions. Predictive inference is
#' performed by sequential importance resampling, which is a particle-based
#' approximate method (Koller and Friedman, 2009).
#'
#' @param gmdbn An object of class \code{gmdbn}.
#' @param evid A data frame containing the evidence. Its columns must explicitly
#' be named after nodes of \code{gmdbn} and can contain missing values (columns
#' with no value can be removed).
#' @param evid_pred A data frame containing the "predicted" evidence. Its
#' columns must explicitly be named after nodes of \code{gmdbn} and can contain
#' missing values (columns with no value can be removed).
#' @param nodes A character vector containing the inferred nodes (by default all
#' the nodes of \code{gmdbn}).
#' @param col_seq A character vector containing the column names of \code{evid}
#' and \code{evid_pred} that describe the observation sequence. If \code{NULL}
#' (the default), all the observations belong to a single sequence. The
#' observations of a same sequence must be ordered such that the \eqn{t}th one
#' is related to time slice \eqn{t} (note that the sequences can have different
#' lengths).
#' @param horizon A positive integer vector containing the time horizons for
#' which predictive inference is performed.
#' @param n_part A positive integer corresponding to the number of particles
#' generated for each observation sequence.
#' @param max_part_sim An integer greater than or equal to \code{n_part}
#' corresponding to the maximum number of particles that can be processed
#' simultaneously. This argument is used to prevent memory overflow, dividing
#' \code{evid} into smaller subsets that are handled sequentially.
#' @param min_ess A numeric value in [0, 1] corresponding to the minimum ESS
#' (expressed as a proportion of \code{n_part}) under which the renewal step of
#' sequential importance resampling is performed. If \code{1} (the default),
#' this step is performed at each time slice.
#' @param verbose A logical value indicating whether subsets of \code{evid} and
#' time slices in progress are displayed.
#'
#' @return If \code{horizon} has one element, a data frame with a structure
#' similar to \code{evid} containing the predicted values of the inferred
#' nodes and their observation sequences (if \code{col_seq} is not \code{NULL}).
#' If \code{horizon} has two or more elements, a list of data frames (tibbles)
#' containing these values for each time horizon.
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{filtering}}, \code{\link{inference}},
#' \code{\link{smoothing}}
#'
#' @examples
#' \donttest{
#' set.seed(0)
#' data(gmdbn_air, data_air)
#' evid <- data_air
#' evid$NO2[sample.int(7680, 1536)] <- NA
#' evid$O3[sample.int(7680, 1536)] <- NA
#' pred <- prediction(gmdbn_air, evid, evid[, c("DATE", "TEMP", "WIND")],
#'                    nodes = c("NO2", "O3"), col_seq = "DATE",
#'                    horizon = c(1, 2), verbose = TRUE)}
#'
#' @export

prediction <- function(gmdbn, evid, evid_pred = NULL, nodes = names(gmdbn$b_1),
                       col_seq = NULL, horizon = 1, n_part = 1000,
                       max_part_sim = 1e06, min_ess = 1, verbose = FALSE) {
  if (!inherits(gmdbn, "gmdbn")) {
    "gmdbn is not of class \"gmdbn\"" %>%
      stop()
  }

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

  struct <- gmdbn %>%
    structure()
  nodes_gmdbn <- struct$nodes
  is_col_seq <- !is.null(col_seq)

  if (is_col_seq) {
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
      "col_seq contains nodes (or instantiations of nodes) of gmdbn" %>%
        stop()
    }

    if (any(!(col_seq %in% col_evid))) {
      "elements of col_seq are not column names of evid" %>%
        stop()
    }
  }

  seq <- evid %>%
    select(all_of(col_seq)) %>%
    as_tibble()

  if (any(!(map_chr(seq, mode) %in% c("numeric", "character", "logical")))) {
    "columns of evid[col_seq] have invalid types" %>%
      stop()
  }

  if (is.null(evid_pred)) {
    evid_pred <- seq %>%
      slice(0)
    col_evid_pred <- col_seq
  } else {
    if (!is.data.frame(evid_pred)) {
      "evid_pred is not a data frame" %>%
        stop()
    }

    evid_pred <- evid_pred %>%
      ungroup()
    col_evid_pred <- evid_pred %>%
      colnames()

    if (any(duplicated(col_evid_pred))) {
      "evid_pred has duplicated column names" %>%
        stop()
    }

    if (is_col_seq) {
      if (any(!(col_seq %in% col_evid_pred))) {
        "elements of col_seq are not column names of evid_pred" %>%
          stop()
      }

      if (any(!map2_lgl(seq, evid_pred[col_seq],
                        function(seq, seq_pred) {
                          return(identical(class(seq), class(seq_pred)) |
                                 (is.numeric(seq) & is.numeric(seq_pred)) |
                                 ((is.character(seq) | is.factor(seq)) &
                                  (is.character(seq_pred) |
                                   is.factor(seq_pred))))
                        }))) {
        "columns of evid[col_seq] and evid_pred[col_seq] have incompatible types" %>%
          stop()
      }
    }
  }

  if (!is.vector(horizon, "numeric")) {
    "horizon is not a numeric vector" %>%
      stop()
  }

  horizon <- horizon %>%
    unique()
  n_hor <- horizon %>%
    length()

  if (n_hor == 0) {
    "horizon is empty" %>%
      stop()
  }

  if (any(!is.finite(horizon))) {
    "horizon has non-finite elements" %>%
      stop()
  }

  if (any(horizon <= 0)) {
    "horizon has non-positive elements" %>%
      stop()
  }

  if (any(round(horizon) != horizon)) {
    "horizon has non-integer elements" %>%
      stop()
  }

  col_evid_prop <- col_seq %>%
    c(nodes_gmdbn)
  prefix <- col_evid_prop %>%
    sort() %>%
    last() %>%
    str_c("_")
  col_time <- prefix %>%
    str_c("time")
  seq_time <- seq %>%
    group_by(across(col_seq)) %>%
    mutate(!!col_time := seq_len(n())) %>%
    ungroup()
  col_seq_time <- col_seq %>%
    c(col_time)
  horizon <- horizon %>%
    sort()

  if (length(gmdbn) == 1) {
    evid_pred <- evid_pred %>%
      select(any_of(col_evid_prop)) %>%
      group_by(across(col_seq)) %>%
      mutate(!!col_time := seq_len(n())) %>%
      ungroup() %>%
      left_join(seq_time, ., by = col_seq_time)
    infer <- seq %>%
      bind_cols(inference(gmdbn$b_1, evid_pred, nodes = nodes, n_part = n_part,
                          max_part_sim = max_part_sim, verbose = verbose))
    pred <- horizon %>%
      map(function(horizon) {
        infer %>%
          group_by(across(col_seq)) %>%
          mutate(across(nodes, ~ lag(lead(., horizon - 1), horizon - 1))) %>%
          ungroup() %>%
          return()
      })

    if (n_hor == 1) {
      pred <- pred[[1]]
    } else {
      pred <- pred %>%
        set_names(str_c("hor_", horizon))
    }
  } else {
    if (!is.vector(nodes, "character")) {
      "nodes is not a character vector" %>%
        stop()
    }

    nodes <- nodes %>%
      unique()
    n_nodes <- nodes %>%
      length()

    if (n_nodes == 0) {
      "nodes is empty" %>%
        stop()
    }

    if (any(!(nodes %in% nodes_gmdbn))) {
      "elements of nodes are not nodes of gmdbn" %>%
        stop()
    }

    nodes <- nodes %>%
      sort()
    col_n <- prefix %>%
      str_c("n")
    min_hor <- horizon[1]
    n_times_seq <- seq %>%
      group_by(across(col_seq)) %>%
      summarise(!!col_n := n(), .groups = "drop")

    if (max(n_times_seq[[col_n]], 0) < min_hor) {
      nodes_0 <- numeric() %>%
        matrix(0, n_nodes, dimnames = list(NULL, nodes)) %>%
        as_tibble()
      pred <- seq %>%
        bind_rows(nodes_0)

      if (n_hor > 1) {
        pred <- pred %>%
          list() %>%
          rep(n_hor) %>%
          set_names(str_c("hor_", horizon))
      }
    } else {
      if (!is.vector(n_part, "numeric")) {
        "n_part is not a numeric value" %>%
          stop()
      }

      if (length(n_part) != 1) {
        "n_part is not of length 1" %>%
          stop()
      }

      if (!is.finite(n_part)) {
        "n_part is not finite" %>%
          stop()
      }

      if (n_part <= 0) {
        "n_part is not positive" %>%
          stop()
      }

      if (round(n_part) != n_part) {
        "n_part is not an integer" %>%
          stop()
      }

      if (!is.vector(max_part_sim, "numeric")) {
        "max_part_sim is not a numeric value" %>%
          stop()
      }

      if (length(max_part_sim) != 1) {
        "max_part_sim is not of length 1" %>%
          stop()
      }

      if (is.na(max_part_sim)) {
        "max_part_sim is NA" %>%
          stop()
      }

      if (max_part_sim < n_part) {
        "max_part_sim is lower than n_part" %>%
          stop()
      }

      if (round(max_part_sim) != max_part_sim) {
        "max_part_sim is not an integer" %>%
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

      n_nodes_gmdbn <- nodes_gmdbn %>%
        length()
      arcs <- struct$arcs %>%
        bind_rows()

      if (n_nodes < n_nodes_gmdbn) {
        nodes_obs_evid <- evid %>%
          select(any_of(nodes_gmdbn) & where(~ !any(is.na(.)))) %>%
          colnames()
        nodes_obs <- evid_pred %>%
          select(any_of(col_evid_prop)) %>%
          group_by(across(col_seq)) %>%
          mutate(!!col_time := seq_len(n())) %>%
          ungroup() %>%
          left_join(seq_time, ., by = col_seq_time) %>%
          select(any_of(nodes_gmdbn) & where(~ !any(is.na(.)))) %>%
          colnames() %>%
          intersect(nodes_obs_evid)
        blanket <- nodes
        blanket_miss <- blanket[!(blanket %in% nodes_obs)]

        while (length(blanket_miss) > 0) {
          child <- arcs %>%
            filter(from %in% blanket_miss) %>%
            .$to
          copar <- arcs %>%
            filter(to %in% c(blanket_miss, child)) %>%
            .$from
          blanket_add <- child %>%
            c(copar) %>%
            setdiff(blanket)
          blanket <- blanket %>%
            c(blanket_add)
          blanket_miss <- blanket_add[!(blanket_add %in% nodes_obs)]
        }

        gmdbn <- gmdbn %>%
          remove_nodes(setdiff(nodes_gmdbn, blanket))
        nodes_gmdbn <- gmdbn$b_1 %>%
          names()
        n_nodes_gmdbn <- nodes_gmdbn %>%
          length()
        col_evid_prop <- col_seq %>%
          c(nodes_gmdbn)
        arcs <- arcs %>%
          filter(from %in% blanket, to %in% blanket)
      }

      col_sub <- prefix %>%
        str_c("sub")
      col_weight <- prefix %>%
        str_c("weight")
      col_prop <- col_seq %>%
        c(col_weight)
      n_prop <- arcs$lag %>%
        max(0) - 1

      if (n_prop >= 0) {
        col_prop <- col_prop %>%
          c(str_c(rep(str_c(nodes_gmdbn, "."), n_prop),
                  rep(rev(seq_len(n_prop)), each = length(nodes_gmdbn))),
            nodes_gmdbn)
      }

      evid <- evid %>%
        select(any_of(col_evid_prop))
      evid_pred <- evid_pred %>%
        select(any_of(col_evid_prop))
      times_gmbn <- gmdbn %>%
        names() %>%
        str_remove("b_") %>%
        as.numeric() %>%
        c(Inf)
      time_gmbn <- 1
      i_gmbn <- 1
      seq_time_hor <- horizon %>%
        last() %>%
        seq_len()
      names_hor <- "hor_" %>%
        str_c(horizon)
      list_pred <- list()
      n_sub <- (nrow(n_times_seq) * n_part - 1) %/% max_part_sim + 1
      pred <- n_times_seq %>%
        mutate(!!col_sub := ntile(!!sym(col_n), n_sub)) %>%
        group_by(across(col_sub)) %>%
        group_map(function(n_times_seq, sub) {
          if (verbose) {
            verb <- "subset " %>%
              str_c(sub[[col_sub]], " / ", n_sub)
            "\n" %>%
              c(verb, "\n", rep("-", str_length(verb)), "\n") %>%
              str_c(collapse = "") %>%
              cat()
          }

          seq <- n_times_seq %>%
            select(all_of(col_seq))

          if (n_sub > 1) {
            evid <- seq %>%
              inner_join(evid, by = col_seq)
            evid_pred <- seq %>%
              inner_join(evid_pred, by = col_seq)
          }

          part <- seq %>%
            particles(col_weight = col_weight, n_part = n_part)
          max_time <- n_times_seq[[col_n]] %>%
            max(0) - min_hor

          for (time in 0:max_time) {
            if (verbose) {
              "time " %>%
                str_c(time, " / ", max_time, "\n") %>%
                cat()
            }

            if (time > 0) {
              evid_time <- evid %>%
                group_by(across(col_seq)) %>%
                slice(time) %>%
                ungroup()

              if (is_col_seq) {
                part <- part %>%
                  semi_join(evid_time, by = col_seq)
              }

              part <- part %>%
                select(any_of(col_prop))

              if (time == time_gmbn) {
                gmbn <- gmdbn[[i_gmbn]]
                i_gmbn <- i_gmbn + 1
                time_gmbn <- times_gmbn[i_gmbn]
              }

              part <- part %>%
                propagation(gmbn, evid_time, col_seq = col_seq,
                            col_weight = col_weight, min_ess = min_ess)
              gmbn_pred <- gmbn
            }

            part_pred <- part
            i_gmbn_pred <- i_gmbn
            time_gmbn_pred <- time_gmbn
            pred_time <- list()

            for (time_hor in seq_time_hor) {
              time_pred <- time + time_hor
              evid_time_pred <- evid_pred %>%
                group_by(across(col_seq)) %>%
                slice(time_pred) %>%
                ungroup()

              if (time_pred == time_gmbn_pred) {
                gmbn_pred <- gmdbn[[i_gmbn_pred]]
                i_gmbn_pred <- i_gmbn_pred + 1
                time_gmbn_pred <- times_gmbn[i_gmbn_pred]
              }

              part_pred <- part_pred %>%
                propagation(gmbn_pred, evid_time_pred, col_seq = col_seq,
                            col_weight = col_weight, min_ess = min_ess)

              if (time_hor %in% horizon) {
                pred_time_hor <- part_pred %>%
                  aggregation(nodes, col_seq = col_seq, col_weight = col_weight)
                pred_time <- pred_time %>%
                  c(list(pred_time_hor))
              }
            }

            pred_time <- pred_time %>%
              set_names(names_hor)
            list_pred <- list_pred %>%
              c(list(pred_time))
          }

          list_pred %>%
            return()
        })
      col_pred <- col_seq %>%
        c(nodes)
      pred <- pred %>%
        flatten() %>%
        transpose() %>%
        map2(horizon, function(pred, horizon) {
          pred %>%
            bind_rows() %>%
            group_by(across(col_seq)) %>%
            mutate(!!col_time := seq_len(n())) %>%
            ungroup() %>%
            left_join(seq_time, ., by = col_seq_time) %>%
            group_by(across(col_seq)) %>%
            mutate(across(nodes, ~ lag(., horizon - 1))) %>%
            ungroup() %>%
            select(all_of(col_pred)) %>%
            return()
        })

      if (n_hor == 1) {
        pred <- pred[[1]]
      }
    }
  }

  pred %>%
    return()
}
