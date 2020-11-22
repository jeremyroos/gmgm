gmm_1 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(0, 2, 1, 3), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_1) <- "gmm"
gmm_2 <- list(alpha = c(0.4, 0.6),
              mu = matrix(c(2, 0, 3, 1), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_2) <- "gmm"
gmm_3 <- list(alpha = c(0.6, 0.4),
              mu = matrix(c(1, 3, 0, 2), 2, dimnames = list(c("A", "B"), NULL)),
              sigma = list(matrix(c(2, 3, 3, 5), 2,
                                  dimnames = list(c("A", "B"), c("A", "B"))),
                           matrix(c(1, 1, 1, 2), 2,
                                  dimnames = list(c("A", "B"), c("A", "B")))))
class(gmm_3) <- "gmm"
gmm_4 <- list(alpha = c(0.6, 0.4),
              mu = matrix(c(3, 1, 2, 0), 2, dimnames = list(c("B", "A"), NULL)),
              sigma = list(matrix(c(5, 3, 3, 2), 2,
                                  dimnames = list(c("B", "A"), c("B", "A"))),
                           matrix(c(2, 1, 1, 1), 2,
                                  dimnames = list(c("B", "A"), c("B", "A")))))
class(gmm_4) <- "gmm"

test_that("reorder variables", {
  expect_equal(reorder(gmm_1, var = c("B", "A")), gmm_2)
  expect_equal(reorder(gmm_1, var = "B"), gmm_2)
})

test_that("reorder mixture components", {
  expect_equal(reorder(gmm_1, comp = c(2, 1)), gmm_3)
  expect_equal(reorder(gmm_1, comp = 2), gmm_3)
})

test_that("reorder variables and mixture components", {
  expect_equal(reorder(gmm_1, var = "B", comp = 2), gmm_4)
})

test_that("reorder nothing", {
  expect_equal(reorder(gmm_1), gmm_1)
  expect_equal(reorder(gmm_1, var = character(), comp = numeric()), gmm_1)
})
