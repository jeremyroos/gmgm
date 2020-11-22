#' Merge mixture components of a Gaussian mixture model
#'
#' This function merges mixture components of a Gaussian mixture model (Zhang
#' \emph{et al.}, 2003).
#'
#' @param gmm An object of class \code{gmm}.
#' @param comp An integer vector containing the indexes of the merged mixture
#' components (by default all the components of \code{gmm}).
#'
#' @return The \code{gmm} object after merging the mixture components.
#'
#' @references
#' Zhang, Z., Chen, C., Sun, J. and Chan, K. L. (2003). EM algorithms for
#' Gaussian mixtures with split-and-merge operation. \emph{Pattern Recognition},
#' 36(9):1973--1983.
#'
#' @seealso \code{\link{split_comp}}
#'
#' @examples
#' data(gmm_body)
#' gmm_1 <- merge_comp(gmm_body, c(1, 2))
#'
#' @export

merge_comp <- function(gmm, comp = seq_along(gmm$alpha)) {
  if (!inherits(gmm, "gmm")) {
    "gmm is not of class \"gmm\"" %>%
      stop()
  }

  alpha <- gmm$alpha

  if (!is.null(comp) & !is.vector(comp, "numeric")) {
    "comp is not numeric" %>%
      stop()
  }

  if (length(comp) > 0) {
    comp <- comp %>%
      unique()
    seq_comp <- alpha %>%
      seq_along()

    if (any(!(comp %in% seq_comp))) {
      "elements of comp are invalid component indexes" %>%
        stop()
    }

    alpha_comp <- alpha[comp]
    alpha_merge <- alpha_comp %>%
      sum()
    weights <- alpha_comp / alpha_merge
    mu <- gmm$mu
    mu_comp <- mu[, comp, drop = FALSE]
    mu_merge <- mu_comp %*% weights
    sigma <- gmm$sigma
    sigma_comp <- sigma[comp]
    sigma_merge <- comp %>%
      seq_along() %>%
      map(function(i_comp) {
        mu_i_comp <- mu_comp[, i_comp]
        return(weights[i_comp] *
                 (sigma_comp[[i_comp]] + mu_i_comp %*% t(mu_i_comp)))
      }) %>%
      reduce(`+`) - mu_merge %*% t(mu_merge)
    comp_fix <- seq_comp %>%
      setdiff(comp)
    order_comp <- comp_fix %>%
      c(min(comp)) %>%
      order()
    alpha <- alpha[comp_fix] %>%
      c(alpha_merge) %>%
      .[order_comp]
    mu <- mu[, comp_fix, drop = FALSE] %>%
      cbind(mu_merge, deparse.level = 0) %>%
      .[, order_comp, drop = FALSE]
    sigma <- sigma[comp_fix] %>%
      c(list(sigma_merge)) %>%
      .[order_comp]
    gmm <- alpha %>%
      gmm(mu, sigma)
  }

  gmm %>%
    return()
}
