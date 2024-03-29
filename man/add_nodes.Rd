% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_nodes.R
\name{add_nodes}
\alias{add_nodes}
\title{Add nodes to a Gaussian mixture graphical model}
\usage{
add_nodes(gmgm, nodes)
}
\arguments{
\item{gmgm}{An object of class \code{gmbn} or \code{gmdbn}. If \code{NULL}, a
\code{gmbn} object is created with the added nodes.}

\item{nodes}{A character vector containing the added nodes.}
}
\value{
The \code{gmbn} or \code{gmdbn} object after adding the nodes.
}
\description{
This function adds nodes to a Gaussian mixture graphical model. If this model
is a dynamic Bayesian network, the nodes are added to each of its transition
models. For each added node, a one-component univariate Gaussian mixture
model is created with mean 0 and variance 1.
}
\examples{
data(gmbn_body)
gmbn_1 <- add_nodes(gmbn_body, c("CHOL", "TRIGLY"))

data(gmdbn_air)
gmdbn_1 <- add_nodes(gmdbn_air, "PM10")

}
\seealso{
\code{\link{add_arcs}}, \code{\link{relevant}},
\code{\link{remove_arcs}}, \code{\link{remove_nodes}},
\code{\link{rename_nodes}}
}
