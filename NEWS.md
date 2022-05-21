## gmgm (development version)

* Fixed formatting and typing errors in the documentation.
* Added slightly more details to the main documentation page.

## gmgm 1.1.0

* Created function `relevant`, which extracts the minimal sub-Gaussian mixture graphical model required to infer a subset of nodes.
* Changed the default values of several arguments to increase the speed of learning functions and make them easier to use:
  * `regul = NULL` → `regul = 0.01` (functions `AIC`, `BIC`, `logLik`, `em`)
  * `max_iter_em = Inf` → `max_iter_em = 100` (function `em`)
  * `max_iter_smem = Inf` → `max_iter_smem = 10` (function `smem`)
  * `max_iter_step = Inf` → `max_iter_step = 10` (function `stepwise`)
  * `max_iter_pem = Inf` → `max_iter_pem = 5` (functions `param_em`, `struct_em`)
  * `max_iter_sem = Inf` → `max_iter_sem = 5` (function `struct_em`)

## gmgm 1.0.2

* Replaced dplyr's scoped verbs (`_all`, `_at`, `_if`), which have been superseded by the use of function `across`.
* Modified function `gmm` to coerce the elements of the `gmm` object to type double.
* Provided a more detailed example in the README file.

## gmgm 1.0.1

* Removed a unit test of function `em` that failed with some BLAS/LAPACK implementations.

## gmgm 1.0.0

* Initial release.
