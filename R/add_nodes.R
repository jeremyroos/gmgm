#' Add nodes to a Gaussian mixture graphical model
#'
#' This function adds nodes to a Gaussian mixture graphical model. If this model
#' is a dynamic Bayesian network, the nodes are added to each of its transition
#' models. For each added node, a one-component univariate Gaussian mixture
#' model is created with mean 0 and variance 1.
#'
#' @param gmgm An object of class \code{gmbn} or \code{gmdbn}. If \code{NULL}, a
#' \code{gmbn} object is created with the added nodes.
#' @param nodes A character vector containing the added nodes.
#'
#' @return The \code{gmbn} or \code{gmdbn} object after adding the nodes.
#'
#' @seealso \code{\link{add_arcs}}, \code{\link{remove_arcs}},
#' \code{\link{remove_nodes}}, \code{\link{rename_nodes}}
#'
#' @examples
#' data(gmbn_body)
#' gmbn_1 <- add_nodes(gmbn_body, c("CHOL", "TRIGLY"))
#'
#' data(gmdbn_air)
#' gmdbn_1 <- add_nodes(gmdbn_air, "PM10")
#'
#' @export

add_nodes <- function(gmgm, nodes) {
  if (is.null(gmgm) | inherits(gmgm, "gmbn")) {
    if (!is.null(nodes) & !is.vector(nodes, "character")) {
      "nodes is not a character vector" %>%
        stop()
    }

    nodes <- nodes %>%
      setdiff(names(gmgm))
    gmgm <- nodes %>%
      map(function(node) {
        add_var(NULL, node) %>%
          return()
      }) %>%
      set_names(nodes) %>%
      c(gmgm) %>%
      do.call("gmbn", .)
  } else {
    if (!inherits(gmgm, "gmdbn")) {
      "gmgm is not of class \"gmbn\" or \"gmdbn\"" %>%
        stop()
    }

    gmgm <- gmgm %>%
      map(add_nodes, nodes) %>%
      do.call("gmdbn", .)
  }

  gmgm %>%
    return()
}
