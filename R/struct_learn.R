#' Learn the structure and the parameters of a Gaussian mixture graphical model
#'
#' This function learns the (graphical and mixture) structure and the parameters
#' of a Gaussian mixture graphical model. Using the local decomposability of the
#' scoring function, this task consists in learning each local conditional model
#' independently with the stepwise algorithm (Koller and Friedman, 2009). Note
#' that some candidate arcs may be discarded to avoid that the global graphical
#' structure contains cycles. To limit recourse to this action, the learning
#' process is performed sequentially. The more arcs of a local model are likely
#' to be part of cycles (considering the worst case where all the candidate arcs
#' are selected), the later this local model is processed. By gradually taking
#' into account the local structures learned over time, the number of possible
#' cycles decreases and, with it, the number of candidate arcs to discard.
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
#' @param arcs_cand A data frame containing the candidate arcs for addition or
#' removal (by default all possible non-temporal arcs). The column \code{from}
#' describes the start node, the column \code{to} the end node and the column
#' \code{lag} the time lag between them. Missing values in \code{from} or
#' \code{to} are interpreted as "all possible nodes", which allows to quickly
#' define large set of arcs that share common attributes. Missing values in
#' \code{lag} are replaced by 0. If \code{gmgm} is a \code{gmdbn} object, the
#' same candidate arcs are used for each of its \code{gmbn} elements. This
#' constraint can be overcome by passing a list of data frames named after some
#' of these elements (\code{b_1}, \dots) and containing candidate arcs specific
#' to them. If arcs already in \code{gmgm} are not candidates, they cannot be
#' removed. Therefore, setting \code{arcs_cand} to \code{NULL} is equivalent to
#' learning only the mixture structure (and the parameters) of the model.
#' @param col_seq A character vector containing the column names of \code{data}
#' that describe the observation sequence. If \code{NULL} (the default), all the
#' observations belong to a single sequence. If \code{gmgm} is a temporal
#' \code{gmbn} or \code{gmdbn} object, the observations of a same sequence must
#' be ordered such that the \eqn{t}th one is related to time slice \eqn{t} (note
#' that the sequences can have different lengths). If \code{gmgm} is a
#' non-temporal \code{gmbn} object, this argument is ignored.
#' @param score A character string (\code{"aic"}, \code{"bic"} or
#' \code{"loglik"}) corresponding to the scoring function.
#' @param verbose A logical value indicating whether learned nodes in progress
#' are displayed.
#' @param \dots Additional arguments passed to function \code{\link{stepwise}}.
#'
#' @return A list with elements:
#' \item{gmgm}{The final \code{gmbn} or \code{gmdbn} object.}
#' \item{evol_score}{A list with elements:}
#' \item{}{\code{global}\verb{  }A numeric vector containing the global score
#' before and after learning.}
#' \item{}{\code{local}\verb{  }For a \code{gmbn} object, a numeric matrix
#' containing the local conditional scores before and after learning. For a
#' \code{gmdbn} object, a list of numeric matrices containing these values for
#' each \code{gmbn} element.}
#'
#' @references
#' Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
#' Principles and Techniques}. The MIT Press.
#'
#' @seealso \code{\link{param_em}}, \code{\link{param_learn}},
#' \code{\link{struct_em}}
#'
#' @examples
#' \donttest{
#' data(data_body)
#' gmbn_1 <- add_nodes(NULL,
#'                     c("AGE", "FAT", "GENDER", "GLYCO", "HEIGHT", "WAIST",
#'                       "WEIGHT"))
#' arcs_cand_1 <- data.frame(from = c("AGE", "GENDER", "HEIGHT", "WEIGHT", NA,
#'                                    "AGE", "GENDER", "AGE", "FAT", "GENDER",
#'                                    "HEIGHT", "WEIGHT", "AGE", "GENDER",
#'                                    "HEIGHT"),
#'                           to = c("FAT", "FAT", "FAT", "FAT", "GLYCO", "HEIGHT",
#'                                  "HEIGHT", "WAIST", "WAIST", "WAIST", "WAIST",
#'                                  "WAIST", "WEIGHT", "WEIGHT", "WEIGHT"))
#' res_learn_1 <- struct_learn(gmbn_1, data_body, arcs_cand = arcs_cand_1,
#'                             verbose = TRUE, max_comp = 3, max_rank = 1,
#'                             regul = 0.01, max_iter_em = 100)
#'
#' data(data_air)
#' gmdbn_1 <- gmdbn(b_2 = add_nodes(NULL, c("NO2", "O3", "TEMP", "WIND")),
#'                  b_13 = add_nodes(NULL, c("NO2", "O3", "TEMP", "WIND")))
#' arcs_cand_2 <- data.frame(from = c("NO2", "NO2", "NO2", "O3", "TEMP", "TEMP",
#'                                    "WIND", "WIND"),
#'                           to = c("NO2", "O3", "O3", "O3", NA, NA, NA, NA),
#'                           lag = c(1, 0, 1, 1, 0, 1, 0, 1))
#' res_learn_2 <- struct_learn(gmdbn_1, data_air, arcs_cand = arcs_cand_2,
#'                             col_seq = "DATE", verbose = TRUE, max_comp = 3,
#'                             max_rank = 1, regul = 0.01, max_iter_em = 100)}
#'
#' @export

struct_learn <- function(gmgm, data, nodes = structure(gmgm)$nodes,
                         arcs_cand = tibble(lag = 0), col_seq = NULL,
                         score = "bic", verbose = FALSE, ...) {
  if (!is.data.frame(data)) {
    "data is not a data frame" %>%
      stop()
  }

  data <- data %>%
    ungroup()
  col_data <- data %>%
    colnames()

  if (any(duplicated(col_data))) {
    "data has duplicated column names" %>%
      stop()
  }

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

    if (any(!(col_seq %in% col_data))) {
      "elements of col_seq are not column names of data" %>%
        stop()
    }

    if (any(!(map_chr(data[col_seq], mode) %in%
              c("numeric", "character", "logical")))) {
      "columns of data[col_seq] have invalid types" %>%
        stop()
    }
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

  struct <- gmgm %>%
    structure()
  arcs <- struct$arcs

  if (inherits(gmgm, "gmbn")) {
    nodes_gmgm <- struct$nodes

    if (any(!(nodes_gmgm %in% col_data))) {
      "nodes of gmgm are not column names of data" %>%
        stop()
    }

    if (any(str_remove(col_seq, "\\.[1-9][0-9]*$") %in% nodes_gmgm)) {
      "col_seq contains nodes of gmgm" %>%
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
      f_score <- AIC.gmm
    } else if (score == "bic") {
      f_score <- BIC.gmm
    } else if (score == "loglik") {
      f_score <- logLik.gmm
    } else {
      "score is not \"aic\", \"bic\" or \"loglik\"" %>%
        stop()
    }

    if (!is.null(nodes)) {
      if (!is.vector(nodes, "character")) {
        "nodes is not a character vector" %>%
          stop()
      }

      if (any(!(nodes %in% nodes_gmgm))) {
        "elements of nodes are not nodes of gmgm" %>%
          stop()
      }
    }

    max_lag <- arcs$lag %>%
      max(0)

    if (is.null(arcs_cand)) {
      arcs_cand <- tibble(from = character(), to = character(), lag = numeric())
    } else {
      if (!is.data.frame(arcs_cand)) {
        "arcs_cand is not a data frame" %>%
          stop()
      }

      arcs_cand <- arcs_cand %>%
        ungroup()
      col_arcs_cand <- arcs_cand %>%
        colnames()

      if (any(duplicated(col_arcs_cand))) {
        "arcs_cand has duplicated column names" %>%
          stop()
      }

      if (!any(c("from", "to", "lag") %in% col_arcs_cand)) {
        "arcs_cand has no column from, to or lag" %>%
          stop()
      }

      if ("from" %in% col_arcs_cand) {
        from_cand <- arcs_cand$from
        from_cand <- from_cand[!is.na(from_cand)]

        if (!is.character(from_cand)) {
          if (length(from_cand) == 0 | is.factor(from_cand)) {
            arcs_cand <- arcs_cand %>%
              mutate(from = as.character(from))
            from_cand <- from_cand %>%
              as.character()
          } else {
            "arcs_cand[[\"from\"]] is not character" %>%
              stop()
          }
        }

        if (any(!(from_cand %in% nodes_gmgm))) {
          "elements of arcs_cand[[\"from\"]] are not nodes of gmgm" %>%
            stop()
        }
      }

      if ("to" %in% col_arcs_cand) {
        to_cand <- arcs_cand$to
        to_cand <- to_cand[!is.na(to_cand)]

        if (!is.character(to_cand)) {
          if (length(to_cand) == 0 | is.factor(to_cand)) {
            arcs_cand <- arcs_cand %>%
              mutate(to = as.character(to))
            to_cand <- to_cand %>%
              as.character()
          } else {
            "arcs_cand[[\"to\"]] is not character" %>%
              stop()
          }
        }

        if (any(!(to_cand %in% nodes_gmgm))) {
          "elements of arcs_cand[[\"to\"]] are not nodes of gmgm" %>%
            stop()
        }
      }

      if ("lag" %in% col_arcs_cand) {
        lag_cand <- arcs_cand$lag
        lag_cand <- lag_cand[!is.na(lag_cand)]

        if (!is.numeric(lag_cand)) {
          if (length(lag_cand) == 0) {
            arcs_cand <- arcs_cand %>%
              mutate(lag = as.numeric(lag))
            lag_cand <- lag_cand %>%
              as.numeric()
          } else {
            "arcs_cand[[\"lag\"]] is not numeric" %>%
              stop()
          }
        }

        if (any(is.infinite(lag_cand))) {
          "arcs_cand[[\"lag\"]] has infinite elements" %>%
            stop()
        }

        if (any(lag_cand < 0)) {
          "arcs_cand[[\"lag\"]] has negative elements" %>%
            stop()
        }

        if (any(round(lag_cand) != lag_cand)) {
          "arcs_cand[[\"lag\"]] has non-integer elements" %>%
            stop()
        }

        max_lag <- max_lag %>%
          max(lag_cand)
      }

      arcs_cand <- tibble(from = character(), to = character(),
                          lag = numeric()) %>%
        bind_rows(arcs_cand) %>%
        select(from, to, lag) %>%
        mutate(lag = replace_na(lag, 0))
      arcs_cand_from_na <- arcs_cand %>%
        filter(is.na(from)) %>%
        select(to, lag) %>%
        crossing(from = nodes_gmgm)
      arcs_cand <- arcs_cand %>%
        filter(!is.na(from)) %>%
        bind_rows(arcs_cand_from_na)
      arcs_cand_to_na <- arcs_cand %>%
        filter(is.na(to)) %>%
        select(from, lag) %>%
        crossing(to = nodes_gmgm)
      arcs_cand <- arcs_cand %>%
        filter(!is.na(to)) %>%
        bind_rows(arcs_cand_to_na) %>%
        filter(to %in% nodes, from != to | lag > 0) %>%
        distinct(from, to, lag)
    }

    arcs_mov <- arcs_cand %>%
      setdiff(arcs) %>%
      mutate(movable = TRUE)
    arcs_max <- arcs %>%
      mutate(movable = FALSE) %>%
      bind_rows(arcs_mov)
    arcs_max_lag <- arcs_max %>%
      filter(lag > 0) %>%
      distinct(from, lag) %>%
      mutate(from_lag = str_c(from, ".", lag))
    data <- data %>%
      select(all_of(c(col_seq, nodes_gmgm)))
    data <- arcs_max_lag %>%
      group_by(lag) %>%
      group_map(function(arcs, lag) {
        from <- arcs$from
        data %>%
          group_by(across(col_seq)) %>%
          mutate(across(from, ~ lag(., lag$lag))) %>%
          ungroup() %>%
          select(all_of(from)) %>%
          set_names(arcs$from_lag) %>%
          return()
      }) %>%
      bind_cols(data)

    if (max_lag > 0) {
      data <- data %>%
        group_by(across(col_seq)) %>%
        slice(- seq_len(max_lag)) %>%
        ungroup()
    }

    data <- data %>%
      select(all_of(c(nodes_gmgm, arcs_max_lag$from_lag))) %>%
      as.matrix()
    nodes_learn <- character()
    nodes_pend <- nodes_gmgm
    arcs_0_learn <- tibble(from = character(), to = character())
    arcs_cycles <- arcs_max %>%
      filter(lag == 0) %>%
      select(from, to, movable)
    list_gmm_score <- list()

    while (length(nodes_pend) > 0) {
      arcs_0_learn <- arcs_0_learn %>%
        inner_join(arcs_cycles, by = c("from", "to")) %>%
        mutate(movable = FALSE)
      arcs_cycles <- arcs_cycles %>%
        filter(!(to %in% nodes_learn)) %>%
        bind_rows(arcs_0_learn)
      n_arcs_cycles <- arcs_cycles %>%
        nrow()
      n_arcs_cycles_old <- Inf

      while (n_arcs_cycles < n_arcs_cycles_old) {
        n_arcs_cycles_old <- n_arcs_cycles
        arcs_cycles <- arcs_cycles %>%
          filter(from %in% to, to %in% from)
        n_arcs_cycles <- arcs_cycles %>%
          nrow()
      }

      arcs_cycles_learn <- arcs_cycles
      nodes_learn <- nodes_pend %>%
        sort()
      nodes_pend <- character()

      while (n_arcs_cycles > 0 & length(nodes_learn) > 1) {
        node_pend <- arcs_cycles_learn %>%
          filter(movable) %>%
          group_by(to) %>%
          summarise(n = n(), .groups = "drop") %>%
          filter(n == max(n)) %>%
          slice(n()) %>%
          .$to
        nodes_learn <- nodes_learn %>%
          setdiff(node_pend)
        nodes_pend <- nodes_pend %>%
          c(node_pend)
        arcs_cycles_learn <- arcs_cycles_learn %>%
          filter(to != node_pend | !movable)
        n_arcs_cycles <- arcs_cycles_learn %>%
          nrow()

        while (n_arcs_cycles < n_arcs_cycles_old) {
          n_arcs_cycles_old <- n_arcs_cycles
          arcs_cycles_learn <- arcs_cycles_learn %>%
            filter(from %in% to, to %in% from)
          n_arcs_cycles <- arcs_cycles_learn %>%
            nrow()
        }
      }

      arcs_cycles_learn <- arcs_cycles_learn %>%
        filter(movable) %>%
        select(from, to) %>%
        mutate(lag = 0)
      arcs_cand_learn <- arcs_cand %>%
        filter(to %in% nodes_learn) %>%
        setdiff(arcs_cycles_learn) %>%
        mutate(from_lag = if_else(lag == 0, from, str_c(from, ".", lag))) %>%
        select(from_lag, to)
      res_learn <- gmgm[nodes_learn] %>%
        imap(function(gmm, node) {
          if (verbose) {
            "node " %>%
              str_c(node) %>%
              cat()
          }

          if (node %in% nodes) {
            par_cand <- arcs_cand_learn %>%
              filter(to == node) %>%
              .$from_lag
            res_step <- gmm %>%
              stepwise(gmm = ., data = data, y = node, x_cand = par_cand,
                       score = score, verbose = FALSE, ...)
            gmm <- res_step$gmm
            seq_score <- res_step$seq_score
            evol_score <- seq_score[c(1, length(seq_score))]
          } else {
            evol_score <- gmm %>%
              f_score(data, y = node) %>%
              rep(2)
          }

          if (verbose) {
            "    " %>%
              str_c(score, "_old = ", evol_score[1], "    ", score, "_new = ",
                    evol_score[2], "\n") %>%
              cat()
          }

          arcs <- gmm$mu %>%
            rownames() %>%
            .[- 1] %>%
            str_split_fixed("\\.(?=[1-9][0-9]*$)", 2)
          colnames(arcs) <- c("from", "lag")
          arcs_0 <- arcs %>%
            as_tibble() %>%
            filter(lag == "") %>%
            select(from)
          list(gmm_score = list(gmm = gmm, evol_score = evol_score),
               arcs_0 = arcs_0) %>%
            return()
        }) %>%
        transpose()
      list_gmm_score <- list_gmm_score %>%
        c(res_learn$gmm_score)
      arcs_0_learn <- res_learn$arcs_0 %>%
        bind_rows(.id = "to")
    }

    list_gmm_score <- list_gmm_score %>%
      transpose()
    gmgm <- list_gmm_score$gmm %>%
      do.call("gmbn", .)
    local <- list_gmm_score$evol_score[nodes_gmgm] %>%
      do.call("cbind", .)
    rownames(local) <- c("old", "new")
    global <- local %>%
      rowSums()
  } else {
    n_gmbn <- gmgm %>%
      length()
    names_gmbn <- gmgm %>%
      names()

    if (inherits(nodes, "list")) {
      names_nodes <- nodes %>%
        names()
      n_nodes <- names_nodes %>%
        length()

      if (n_nodes < length(nodes)) {
        "the elements of nodes have no names" %>%
          stop()
      }

      if (any(duplicated(names_nodes))) {
        "nodes has duplicated element names" %>%
          stop()
      }

      if (any(!(names_nodes %in% names_gmbn))) {
        "element names of nodes are not element names of gmgm" %>%
          stop()
      }

      nodes <- NULL %>%
        list() %>%
        rep(n_gmbn - n_nodes) %>%
        set_names(setdiff(names_gmbn, names_nodes)) %>%
        c(nodes) %>%
        .[names_gmbn]
    } else {
      nodes <- nodes %>%
        list() %>%
        rep(n_gmbn)
    }

    if (inherits(arcs_cand, "list")) {
      names_arcs_cand <- arcs_cand %>%
        names()
      n_arcs_cand <- names_arcs_cand %>%
        length()

      if (n_arcs_cand < length(arcs_cand)) {
        "the elements of arcs_cand have no names" %>%
          stop()
      }

      if (any(duplicated(names_arcs_cand))) {
        "arcs_cand has duplicated element names" %>%
          stop()
      }

      if (any(!(names_arcs_cand %in% names_gmbn))) {
        "element names of arcs_cand are not element names of gmgm" %>%
          stop()
      }

      arcs_cand <- NULL %>%
        list() %>%
        rep(n_gmbn - n_arcs_cand) %>%
        set_names(setdiff(names_gmbn, names_arcs_cand)) %>%
        c(arcs_cand) %>%
        .[names_gmbn]
    } else {
      arcs_cand <- arcs_cand %>%
        list() %>%
        rep(n_gmbn)
    }

    times_gmbn <- names_gmbn %>%
      str_remove("b_") %>%
      as.numeric()
    res_learn <- gmgm %>%
      list(arcs, nodes, arcs_cand, names_gmbn, seq_len(n_gmbn)) %>%
      pmap(function(gmbn, arcs, nodes, arcs_cand, name_gmbn, i_gmbn) {
        time_gmbn <- times_gmbn[i_gmbn]

        if (verbose) {
          verb <- "model b_" %>%
            str_c(time_gmbn)
          "\n" %>%
            c(verb, "\n", rep("-", str_length(verb)), "\n") %>%
            str_c(collapse = "") %>%
            cat()
        }

        max_lag <- arcs$lag %>%
          max(0)

        if (!is.null(arcs_cand)) {
          if (!is.data.frame(arcs_cand)) {
            "arcs_cand[[\"" %>%
              str_c(name_gmbn, "\"]] is not a data frame") %>%
              stop()
          }

          arcs_cand <- arcs_cand %>%
            ungroup()
          col_arcs_cand <- arcs_cand %>%
            colnames()

          if (any(duplicated(col_arcs_cand))) {
            "arcs_cand[[\"" %>%
              str_c(name_gmbn, "\"]] has duplicated column names") %>%
              stop()
          }

          if ("lag" %in% col_arcs_cand) {
            lag_cand <- arcs_cand$lag
            lag_cand <- lag_cand[!is.na(lag_cand)]

            if (length(lag_cand) > 0) {
              if (!is.numeric(lag_cand)) {
                "arcs_cand[[\"" %>%
                  str_c(name_gmbn, "\"]][[\"lag\"]] is not numeric") %>%
                  stop()
              }

              if (any(is.infinite(lag_cand))) {
                "arcs_cand[[\"" %>%
                  str_c(name_gmbn, "\"]][[\"lag\"]] has infinite elements") %>%
                  stop()
              }

              if (any(lag_cand < 0)) {
                "arcs_cand[[\"" %>%
                  str_c(name_gmbn, "\"]][[\"lag\"]] has negative elements") %>%
                  stop()
              }

              if (any(round(lag_cand) != lag_cand)) {
                "arcs_cand[[\"" %>%
                  str_c(name_gmbn,
                        "\"]][[\"lag\"]] has non-integer elements") %>%
                  stop()
              }

              arcs_cand <- arcs_cand %>%
                filter(is.na(lag) | lag < time_gmbn)
              max_lag <- max_lag %>%
                max(lag_cand) %>%
                min(time_gmbn - 1)
            }
          }
        }

        time_start <- time_gmbn - max_lag
        data <- data %>%
          group_by(across(col_seq))

        if (i_gmbn < n_gmbn) {
          data <- data %>%
            slice(time_start:(times_gmbn[i_gmbn + 1] - 1))
        } else if (time_start > 1) {
          data <- data %>%
            slice(- seq_len(time_start - 1))
        }

        data <- data %>%
          ungroup()
        gmbn %>%
          struct_learn(gmgm = ., data = data, nodes = nodes,
                       arcs_cand = arcs_cand, col_seq = col_seq, score = score,
                       verbose = verbose, ...) %>%
          return()
      }) %>%
      transpose()
    gmgm <- res_learn$gmgm %>%
      do.call("gmdbn", .)
    evol_score <- res_learn$evol_score %>%
      transpose()
    global <- evol_score$global %>%
      reduce(`+`)
    local <- evol_score$local
  }

  list(gmgm = gmgm, evol_score = list(global = global, local = local)) %>%
    return()
}
