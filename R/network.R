#' Display the graphical structure of a Gaussian mixture Bayesian network
#'
#' This function displays the graphical structure of a Gaussian mixture
#' Bayesian network.
#'
#' @param gmbn An object of class \code{gmbn}.
#'
#' @return A \code{visNetwork} object displaying the graphical structure.
#'
#' @examples
#' data(gmbn_body)
#' network(gmbn_body)
#'
#' data(gmdbn_air)
#' network(gmdbn_air$b_2)
#'
#' @export

network <- function(gmbn) {
  if (!inherits(gmbn, "gmbn")) {
    "gmbn is not of class \"gmbn\"" %>%
      stop()
  }

  struct <- gmbn %>%
    structure()
  nodes <- struct$nodes
  arcs <- struct$arcs

  if (max(arcs$lag, 0) > 0) {
    nodes <- nodes %>%
      str_c(" (t)")
    arcs <- arcs %>%
      mutate(from = if_else(lag == 0, str_c(from, " (t)"),
                            str_c(from, " (t - ", lag, ")")),
             to = str_c(to, " (t)"))
  }

  arcs %>%
    filter(lag > 0) %>%
    distinct(id = from) %>%
    mutate(group = "past") %>%
    bind_rows(tibble(id = nodes, group = "present")) %>%
    mutate(label = id) %>%
    visNetwork(arcs) %>%
    visNodes(shape = "ellipse", font = list(color = "black")) %>%
    visGroups(groupname = "present",
              color = list(background = "white", border = "black",
                           highlight = list(background = "white",
                                            border = "black"))) %>%
    visGroups(groupname = "past",
              color = list(background = "#d9d9d9", border = "black",
                           highlight = list(background = "#d9d9d9",
                                            border = "black"))) %>%
    visEdges(arrows = "to") %>%
    return()
}
