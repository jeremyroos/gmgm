gmm_1 <- list(alpha = c(0.2, 0.2, 0.6),
              mu = matrix(c(- 1, 1, 1, 3, 1, 3), 2,
                          dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = 1,
              mu = matrix(c(0.6, 2.6), dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(2.24, 2.84, 2.84, 4.44), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(2, 2, 2, 3), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_3) <- "gmm"

test_that("merge all the mixture components", {
  expect_equal(merge_comp(gmm_1), gmm_2)
})

test_that("merge one mixture component", {
  expect_equal(merge_comp(gmm_3, 2), gmm_3)
})

test_that("merge unordered mixture components", {
  expect_equal(merge_comp(gmm_1, c(2, 1)), gmm_3)
})

test_that("merge duplicated mixture components", {
  expect_equal(merge_comp(gmm_1, c(1, 2, 2)), gmm_3)
})

test_that("merge no mixture component", {
  expect_equal(merge_comp(gmm_3, NULL), gmm_3)
  expect_equal(merge_comp(gmm_3, numeric()), gmm_3)
})
