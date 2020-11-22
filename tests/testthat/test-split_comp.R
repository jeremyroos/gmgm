gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.2, 0.2, 0.6),
              mu = matrix(c(- 0.4253254, 1.311809, 0.4253254, 2.688191, 1, 3),
                          2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(0.8190983, 0.7072949, 0.7072949, 1.5263932),
                                  2, dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(0.8190983, 0.7072949, 0.7072949, 1.5263932),
                                  2, dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.1333333, 0.1333333, 0.1333333, 0.6),
              mu = matrix(c(- 0.5209151, 1.1571417, 0, 2, 0.5209151, 2.8428583,
                            1, 3),
                          2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(0.8190983, 0.7072949, 0.7072949, 1.5263932),
                                  2, dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(0.8190983, 0.7072949, 0.7072949, 1.5263932),
                                  2, dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(0.8190983, 0.7072949, 0.7072949, 1.5263932),
                                  2, dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(2.225074e-308, 1 - 2.225074e-308),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_4) <- "gmm"
gmm_5 <- list(alpha = 1,
              mu = matrix(c(1, 3), dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_5) <- "gmm"
gmm_6 <- list(alpha = c(0.2, 0.2, 0.6),
              mu = matrix(c(0, 2, 0, 2, 1, 3), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_6) <- "gmm"

test_that("split a mixture component into two subcomponents", {
  expect_equal(split_comp(gmm_1), gmm_2, tolerance = 0.01)
})

test_that("split a mixture component into more than two subcomponents", {
  expect_equal(split_comp(gmm_1, n_sub = 3), gmm_3, tolerance = 0.01)
})

test_that("split a mixture component into one subcomponent", {
  expect_equal(split_comp(gmm_1, n_sub = 1), gmm_1)
})

test_that("split a mixture component into zero-proportion subcomponents", {
  expect_equal(split_comp(gmm_4, n_sub = 1e100), gmm_5)
})

test_that("split a mixture component with no space between the subcomponents", {
  expect_equal(split_comp(gmm_1, space = 0), gmm_6)
})

test_that("split no mixture component", {
  expect_equal(split_comp(gmm_1, NULL), gmm_1)
  expect_equal(split_comp(gmm_1, numeric()), gmm_1)
})
