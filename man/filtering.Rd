% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filtering.R
\name{filtering}
\alias{filtering}
\title{Perform filtering inference in a Gaussian mixture dynamic Bayesian network}
\usage{
filtering(
  gmdbn,
  evid,
  nodes = names(gmdbn$b_1),
  col_seq = NULL,
  lag = 0,
  n_part = 1000,
  max_part_sim = 1e+06,
  min_ess = 1,
  verbose = FALSE
)
}
\arguments{
\item{gmdbn}{An object of class \code{gmdbn}.}

\item{evid}{A data frame containing the evidence. Its columns must explicitly
be named after nodes of \code{gmdbn} and can contain missing values (columns
with no value can be removed).}

\item{nodes}{A character vector containing the inferred nodes (by default all
the nodes of \code{gmdbn}).}

\item{col_seq}{A character vector containing the column names of \code{evid}
that describe the observation sequence. If \code{NULL} (the default), all the
observations belong to a single sequence. The observations of a same sequence
must be ordered such that the \eqn{t}th one is related to time slice \eqn{t}
(note that the sequences can have different lengths).}

\item{lag}{A non-negative integer vector containing the time lags for which
fixed-lag smoothing inference is performed. If \code{0} (the default),
filtering inference is performed.}

\item{n_part}{A positive integer corresponding to the number of particles
generated for each observation sequence.}

\item{max_part_sim}{An integer greater than or equal to \code{n_part}
corresponding to the maximum number of particles that can be processed
simultaneously. This argument is used to prevent memory overflow, dividing
\code{evid} into smaller subsets that are handled sequentially.}

\item{min_ess}{A numeric value in [0, 1] corresponding to the minimum ESS
(expressed as a proportion of \code{n_part}) under which the renewal step of
sequential importance resampling is performed. If \code{1} (the default),
this step is performed at each time slice.}

\item{verbose}{A logical value indicating whether subsets of \code{evid} and
time slices in progress are displayed.}
}
\value{
If \code{lag} has one element, a data frame (tibble) with a structure
similar to \code{evid} containing the estimated values of the inferred
nodes and their observation sequences (if \code{col_seq} is not \code{NULL}).
If \code{lag} has two or more elements, a list of data frames (tibbles)
containing these values for each time lag.
}
\description{
This function performs filtering inference in a Gaussian mixture dynamic
Bayesian network. For a sequence of \eqn{T} time slices, this task consists
in estimating the state of the system at each time slice \eqn{t} (for
\eqn{1 \le t \le T}) given all the data (the evidence) collected up to
\eqn{t}. This function is also designed to perform fixed-lag smoothing
inference, which consists in defining a time lag \eqn{l} such that at each
time slice \eqn{t} (for \eqn{l + 1 \le t \le T}), the state at \eqn{t - l} is
estimated given the evidence collected up to \eqn{t} (Murphy, 2002).
Filtering and fixed-lag smoothing inference are performed by sequential
importance resampling, which is a particle-based approximate method (Koller
and Friedman, 2009).
}
\examples{
\donttest{
set.seed(0)
data(gmdbn_air, data_air)
evid <- data_air
evid$NO2[sample.int(7680, 1536)] <- NA
evid$O3[sample.int(7680, 1536)] <- NA
evid$TEMP[sample.int(7680, 1536)] <- NA
evid$WIND[sample.int(7680, 1536)] <- NA
filt <- filtering(gmdbn_air, evid, col_seq = "DATE", lag = c(0, 1),
                  verbose = TRUE)}

}
\references{
Koller, D. and Friedman, N. (2009). \emph{Probabilistic Graphical Models:
Principles and Techniques}. The MIT Press.

Murphy, K. (2002). \emph{Dynamic Bayesian Networks: Representation, Inference
and Learning}. PhD thesis, University of California.
}
\seealso{
\code{\link{inference}}, \code{\link{prediction}},
\code{\link{smoothing}}
}
