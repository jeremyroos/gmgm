% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_nodes.R
\name{remove_nodes}
\alias{remove_nodes}
\title{Remove nodes from a Gaussian mixture graphical model}
\usage{
remove_nodes(gmgm, nodes)
}
\arguments{
\item{gmgm}{An object of class \code{gmbn} or \code{gmdbn}.}

\item{nodes}{A character vector containing the removed nodes.}
}
\value{
The \code{gmbn} or \code{gmdbn} object after removing the nodes.
}
\description{
This function removes nodes from a Gaussian mixture graphical model. If this
model is a dynamic Bayesian network, the nodes are removed from each of its
transition models.
}
\examples{
data(gmbn_body)
gmbn_1 <- remove_nodes(gmbn_body, c("FAT", "GLYCO"))

data(gmdbn_air)
gmdbn_1 <- remove_nodes(gmdbn_air, "TEMP")

}
\seealso{
\code{\link{add_arcs}}, \code{\link{add_nodes}},
\code{\link{relevant}}, \code{\link{remove_arcs}}, \code{\link{rename_nodes}}
}
