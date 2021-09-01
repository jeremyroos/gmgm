#' Extract the minimal sub-Gaussian mixture graphical model required to infer a
#' subset of nodes
#'
#' This function extracts the minimal sub-Gaussian mixture graphical model
#' required to infer a subset of nodes (i.e. the sub-model relevant to these
#' nodes). The nodes that do not contribute to inference are removed, which
#' includes those that are d-separated from the inferred ones by the nodes whose
#' values are observed, as well as the barren nodes (Druzdzel and Suermondt,
#' 1994).
#'
#' @param gmgm An object of class \code{gmbn} or \code{gmdbn}.
#' @param nodes A character vector containing the inferred nodes.
#' @param nodes_obs A character vector containing the nodes whose values are
#' observed.
#' @param nodes_miss A character vector containing the nodes whose values are
#' missing. Note that if a node is neither in \code{nodes_obs} nor in
#' \code{nodes_miss}, its observability is considered uncertain or varying.
#' Thus, it is not treated as an observed node, nor can it be removed as a
#' barren node.
#'
#' @return The \code{gmbn} or \code{gmdbn} object relevant to the subset of
#' nodes.
#'
#' @references
#' Druzdzel, M. J. and Suermondt, H. J. (1994). Relevance in Probabilistic
#' Models: "Backyards" in a "Small World". \emph{In Working Notes of the AAAI
#' 1994 Fall Symposium Series: Relevance}, pages 60--63, New Orleans, LA, USA.
#'
#' @seealso \code{\link{add_arcs}}, \code{\link{add_nodes}},
#' \code{\link{remove_arcs}}, \code{\link{remove_nodes}},
#' \code{\link{rename_nodes}}
#'
#' @examples
#' data(gmbn_body)
#' gmbn_1 <- relevant(gmbn_body, "AGE",
#'                    nodes_obs = c("FAT", "HEIGHT", "WEIGHT"),
#'                    nodes_miss = "GLYCO")
#'
#' data(gmdbn_air)
#' gmdbn_1 <- do.call("gmdbn", gmdbn_air[c("b_1", "b_2")])
#' gmdbn_2 <- relevant(gmdbn_1, "O3", nodes_obs = "NO2")
#'
#' @export

relevant <- function(gmgm, nodes, nodes_obs = NULL, nodes_miss = NULL) {
  if (!is.vector(nodes, "character")) {
    "nodes is not a character vector" %>%
      stop()
  }

  nodes <- nodes %>%
    unique()

  if (length(nodes) == 0) {
    "nodes is empty" %>%
      stop()
  }

  struct <- gmgm %>%
    structure()
  nodes_gmgm <- struct$nodes

  if (any(!(nodes %in% nodes_gmgm))) {
    "elements of nodes are not nodes of gmgm" %>%
      stop()
  }

  if (!is.null(nodes_obs)) {
    if (!is.vector(nodes_obs, "character")) {
      "nodes_obs is not a character vector" %>%
        stop()
    }

    if (any(!(nodes_obs %in% nodes_gmgm))) {
      "elements of nodes_obs are not nodes of gmgm" %>%
        stop()
    }
  }

  if (!is.null(nodes_miss)) {
    if (!is.vector(nodes_miss, "character")) {
      "nodes_miss is not a character vector" %>%
        stop()
    }

    if (any(!(nodes_miss %in% nodes_gmgm))) {
      "elements of nodes_miss are not nodes of gmgm" %>%
        stop()
    }

    if (any(nodes_miss %in% nodes_obs)) {
      "elements of nodes_miss are in nodes_obs" %>%
        stop()
    }
  }

  nodes_min <- nodes_gmgm %>%
    setdiff(nodes_miss) %>%
    union(nodes)
  arcs <- struct$arcs

  if (inherits(gmgm, "gmbn")) {
    arcs <- arcs %>%
      list()
  }

  n_arcs_old <- Inf
  bound_add <- nodes
  bound <- nodes
  nodes_rel <- arcs %>%
    map(function(arcs) {
      n_arcs <- arcs %>%
        nrow()

      while(n_arcs < n_arcs_old) {
        n_arcs_old <- n_arcs
        arcs <- arcs %>%
          filter(to %in% c(nodes_min, arcs$from))
        n_arcs <- arcs %>%
          nrow()
      }

      while (length(bound_add) > 0) {
        bound_miss <- bound_add %>%
          setdiff(nodes_obs)
        children <- arcs %>%
          filter(from %in% bound_miss) %>%
          .$to
        parents <- arcs %>%
          filter(to %in% c(bound_miss, children)) %>%
          .$from
        bound_add <- children %>%
          c(parents) %>%
          setdiff(bound)
        bound <- bound %>%
          c(bound_add)
      }

      bound %>%
        return()
    }) %>%
    unlist()
  gmgm %>%
    remove_nodes(setdiff(nodes_gmgm, nodes_rel)) %>%
    return()
}
